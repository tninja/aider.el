(require 'ert)

;; all the tests need to refactor or rewrite due to the large
;; refactoring of aider.el

(ert-deftest aider-buffer-name-test ()
  "Test the aider-buffer-name function."
  ;; Test normal git repo case
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () "/path/to/git/repo/")))
    (should (equal (aider-buffer-name) "*aider:/path/to/git/repo/*")))
  
  ;; Test when magit-toplevel returns nil but buffer has a file
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () nil))
            ((symbol-function 'buffer-file-name)
             (lambda () "/path/to/some/file.el")))
    (should (equal (aider-buffer-name) "*aider:/path/to/some/*")))

  ;; Test when magit-toplevel returns fatal message but buffer has a file
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () "fatal: not a git repository"))
            ((symbol-function 'buffer-file-name)
             (lambda () "/another/path/file.txt")))
    (should (equal (aider-buffer-name) "*aider:/another/path/*")))

  ;; Test error case when not in git repo and no buffer file
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () nil))
            ((symbol-function 'buffer-file-name)
             (lambda () nil)))
    (should-error (aider-buffer-name) :type 'error))
  )

(ert-deftest aider--process-message-if-multi-line-test ()
  "Test the aider--process-message-if-multi-line function."
  ;; Test single line text
  (should (equal (aider--process-message-if-multi-line "single line")
                 "single line"))
  ;; Test multi-line text
  (should (equal (aider--process-message-if-multi-line "line 1\nline 2")
                 "{aider\nline 1\nline 2\naider}"))
  ;; Test empty string
  (should (equal (aider--process-message-if-multi-line "")
                 ""))
  ;; Test newline only
  (should (equal (aider--process-message-if-multi-line "\n")
                 "{aider\n\n\naider}")))

(ert-deftest aider--get-add-command-prefix-test ()
  "Test the aider--get-add-command-prefix function."
  ;; Test when read-only mode is off
  (let ((aider--add-file-read-only nil))
    (should (equal (aider--get-add-command-prefix) "/add")))
  ;; Test when read-only mode is on
  (let ((aider--add-file-read-only t))
    (should (equal (aider--get-add-command-prefix) "/read-only"))))

(ert-deftest aider-region-refactor-generate-command-test ()
  "Test the aider-region-refactor-generate-command function."
  ;; Test with function name
  (should (equal (aider-region-refactor-generate-command 
                 "code block" 
                 "test_func" 
                 "make it better")
                 "/architect \"in function test_func, for the following code block, make it better: code block\"\n"))
  
  ;; Test without function name
  (should (equal (aider-region-refactor-generate-command 
                 "code block" 
                 nil 
                 "make it better")
                 "/architect \"for the following code block, make it better: code block\"\n"))
  
  ;; Test multi-line code block
  (should (equal (aider-region-refactor-generate-command 
                 "line 1\nline 2" 
                 "test_func" 
                 "make it better")
                 "/architect \"in function test_func, for the following code block, make it better: line 1\nline 2\"\n"))
  
  ;; Test empty code block
  (should (equal (aider-region-refactor-generate-command 
                 "" 
                 "test_func" 
                 "make it better")
                 "/architect \"in function test_func, for the following code block, make it better: \"\n")))

(ert-deftest aider-font-lock-keywords-test ()
  "Test the aider-font-lock-keywords variable."
  ;; Ensure font-lock-keywords contains correct patterns and faces
  (should (equal (car (car aider-font-lock-keywords))
                 "^\x2500+\n?"))
  (should (equal (nth 2 (car aider-font-lock-keywords))
                 '(face aider-command-separator)))
  (should (equal (car (cadr aider-font-lock-keywords))
                 "^\x2500+"))
  (should (equal (nth 2 (cadr aider-font-lock-keywords))
                 '(face nil display (space :width 2)))))

(ert-deftest aider-write-unit-test-behavior ()
  "Test the behavior of aider-write-unit-test function."
  ;; Test when buffer is not visiting a file
  (with-temp-buffer
    (should (progn (aider-write-unit-test)
                   (equal (current-message) "Current buffer is not visiting a file."))))
  
  ;; Test when buffer is a test file
  (with-temp-buffer
    (let ((buffer-file-name "test_something.py"))
      (should (progn (aider-write-unit-test)
                    (equal (current-message) "Current buffer appears to be a test file.")))))
  
  ;; Test when buffer is a regular file with no function under cursor
  (with-temp-buffer
    (let ((buffer-file-name "regular.py")
          (aider-read-string-result "modified prompt"))
      (cl-letf (((symbol-function 'which-function) (lambda () nil))
                ((symbol-function 'aider-read-string) 
                 (lambda (prompt initial) aider-read-string-result))
                ((symbol-function 'aider-add-current-file) (lambda () t))
                ((symbol-function 'aider--send-command) (lambda (cmd switch) t)))
        (aider-write-unit-test)
        (should (not (equal (current-message) "Current buffer is not visiting a file.")))
        (should (not (equal (current-message) "Current buffer appears to be a test file.")))))))

(ert-deftest aider-faces-test ()
  "Test that aider faces are properly defined."
  ;; Test aider-command-separator face
  (should (face-differs-from-default-p 'aider-command-separator))
  ;; Test aider-command-text face
  (should (face-differs-from-default-p 'aider-command-text)))

;; Helper function to test if a face differs from the default face
(defun face-differs-from-default-p (face)
  "Check if FACE has different properties from default face."
  (let ((face-props (face-all-attributes face nil))
        (default-props (face-all-attributes 'default nil)))
    (not (equal face-props default-props))))

(ert-deftest aider--is-comment-line-test ()
  "Test the aider--is-comment-line function."
  ;; Test with semicolon comment style (Lisp)
  (let ((comment-start ";"))
    ;; Basic comment cases
    (should (aider--is-comment-line "; comment"))
    (should (aider--is-comment-line ";; double comment"))
    (should (aider--is-comment-line ";;; triple comment"))
    ;; Comments with leading whitespace
    (should (aider--is-comment-line "   ; indented comment"))
    (should (aider--is-comment-line "\t; tabbed comment"))
    ;; Non-comment cases
    (should-not (aider--is-comment-line "code ; with comment"))
    (should-not (aider--is-comment-line "regular code"))
    (should-not (aider--is-comment-line "")))
  ;; Test with hash comment style (Python)
  (let ((comment-start "#"))
    (should (aider--is-comment-line "# python comment"))
    (should (aider--is-comment-line "   ## indented python comment"))
    (should-not (aider--is-comment-line "code # with comment")))
  ;; Test with double slash comment style (C/Java)
  (let ((comment-start "//"))
    (should (aider--is-comment-line "// c style comment"))
    (should (aider--is-comment-line "   /// indented c comment"))
    (should-not (aider--is-comment-line "code // with comment")))
  ;; Test with nil comment-start
  (let ((comment-start nil))
    (should-not (aider--is-comment-line "; not a comment when no comment-start"))
    (should-not (aider--is-comment-line "# not a comment when no comment-start"))
    (should-not (aider--is-comment-line "// not a comment when no comment-start")))
  )
