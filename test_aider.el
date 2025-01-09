(require 'ert)

(ert-deftest aider-buffer-name-test ()
  "Test the aider-buffer-name function."
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () "/path/to/git/repo")))
    (should (equal (aider-buffer-name) "*aider:/path/to/git/repo*")))
  
  ;; Test error case when not in a git repo
  (cl-letf (((symbol-function 'magit-toplevel)
             (lambda () "fatal: not a git repository")))
    (should-error (aider-buffer-name) :type 'error)))

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
      (should (progn (aider-write-test)
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
        (aider-write-test)
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
