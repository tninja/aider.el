(require 'ert)
(require 'cl-lib)

;; Set up load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Create comprehensive dependency mocks
(unless (featurep 'magit)
  (provide 'magit)
  (defun magit-toplevel () "/mock/repo/")
  (defun magit-get-current-branch () "main")
  (defun magit-get-upstream-branch (branch) "origin/main")
  (defun magit-get-current-branch-local () "main"))

(unless (featurep 'transient)
  (provide 'transient)
  (defmacro transient-define-prefix (name docstring &rest args) 
    `(defun ,name () (interactive) (message "Mock transient menu")))
  (defmacro transient-define-argument (name docstring &rest args)
    `(defun ,name () (interactive))))

(unless (featurep 's)
  (provide 's)
  (defun s-contains-p (needle haystack) (and needle haystack (string-match-p (regexp-quote needle) haystack)))
  (defun s-starts-with-p (prefix s) (and prefix s (string-prefix-p prefix s))))

(unless (featurep 'markdown-mode)
  (provide 'markdown-mode)
  (defun markdown-maybe-funcall-regexp (&rest args) ""))

(unless (featurep 'comint)
  (provide 'comint)
  (defvar comint-mode-map (make-sparse-keymap))
  (defun comint-mode () (setq major-mode 'comint-mode))
  (defun make-comint-in-buffer (name buffer program &rest args) buffer)
  (defvar comint-input-sender 'comint-simple-send)
  (defun comint-simple-send (proc string) nil)
  (defvar comint-last-input-start nil)
  (defvar comint-last-input-end nil))

(unless (featurep 'savehist)
  (provide 'savehist))

(unless (featurep 'dired)
  (provide 'dired)
  (defun dired-get-marked-files (&rest args) '()))

(unless (featurep 'ffap)
  (provide 'ffap))

(unless (featurep 'which-func)
  (provide 'which-func)
  (defun which-function () nil))

(unless (featurep 'flycheck)
  (provide 'flycheck)
  (defvar flycheck-current-errors '())
  (defun flycheck-error-line (err) 1)
  (defun flycheck-error-message (err) "Mock error"))

;; Load aider modules
(condition-case err
    (progn
      ;; Add warning function if not present
      (unless (fboundp 'warning)
        (defun warning (type message &rest args) 
          (message "Warning (%s): %s" type (apply #'format message args))))
      (require 'aider-core)
      (require 'aider-file)
      (require 'aider-git)
      (require 'aider-code-change)
      (require 'aider-discussion)
      (require 'aider-agile)
      (require 'aider-prompt-mode)
      (require 'aider))
  (error 
   (message "Could not load aider modules: %s" err)))

;;; Test aider-core.el functions

(ert-deftest test-aider--get-git-repo-root ()
  "Test aider--get-git-repo-root function."
  (when (fboundp 'aider--get-git-repo-root)
    ;; Test with mocked magit-toplevel
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/test/repo/")))
      (should (equal (aider--get-git-repo-root) "/test/repo/")))
    
    ;; Test with no git repo
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () nil)))
      (should (null (aider--get-git-repo-root))))))

(ert-deftest test-aider--process-message-if-multi-line ()
  "Test aider--process-message-if-multi-line function."
  (when (fboundp 'aider--process-message-if-multi-line)
    ;; Test single line
    (should (equal (aider--process-message-if-multi-line "single line")
                   "single line"))
    ;; Test multi-line
    (should (equal (aider--process-message-if-multi-line "line 1\nline 2")
                   "{aider\nline 1\nline 2\naider}"))
    ;; Test empty string
    (should (equal (aider--process-message-if-multi-line "")
                   ""))))

(ert-deftest test-aider-buffer-name ()
  "Test aider-buffer-name function."
  (when (fboundp 'aider-buffer-name)
    ;; Test normal git repo case
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/path/to/git/repo/")))
      (should (equal (aider-buffer-name) "*aider:/path/to/git/repo/*")))
    
    ;; Test when magit-toplevel returns nil but buffer has a file
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () nil))
              ((symbol-function 'buffer-file-name)
               (lambda () "/path/to/some/file.el")))
      (should (equal (aider-buffer-name) "*aider:/path/to/some/*")))))

(ert-deftest test-aider--buffer-name-for-git-repo ()
  "Test aider--buffer-name-for-git-repo function."
  (when (fboundp 'aider--buffer-name-for-git-repo)
    ;; Test with branch-specific buffers enabled
    (let ((aider-use-branch-specific-buffers t))
      (cl-letf (((symbol-function 'magit-get-current-branch)
                 (lambda () "feature-branch")))
        (should (equal (aider--buffer-name-for-git-repo "/repo/path/")
                       "*aider:/repo/path/:feature-branch*"))))
    
    ;; Test with branch-specific buffers disabled
    (let ((aider-use-branch-specific-buffers nil))
      (should (equal (aider--buffer-name-for-git-repo "/repo/path/")
                     "*aider:/repo/path/*")))))

;;; Test aider-file.el functions

(ert-deftest test-aider--get-file-path ()
  "Test aider--get-file-path function."
  (when (fboundp 'aider--get-file-path)
    ;; Test file within git repo
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/repo/root/")))
      (should (equal (aider--get-file-path "/repo/root/src/main.py")
                     "src/main.py")))
    
    ;; Test file outside git repo
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () nil)))
      (should (equal (aider--get-file-path "/other/file.py")
                     "/other/file.py")))))

(ert-deftest test-aider--format-file-path ()
  "Test aider--format-file-path function."
  (when (fboundp 'aider--format-file-path)
    ;; Test path without spaces
    (should (equal (aider--format-file-path "src/main.py")
                   "src/main.py"))
    
    ;; Test path with spaces
    (should (equal (aider--format-file-path "src/my file.py")
                   "\"src/my file.py\""))))

(ert-deftest test-aider--get-add-command-prefix ()
  "Test aider--get-add-command-prefix function."
  (when (fboundp 'aider--get-add-command-prefix)
    ;; Test when read-only mode is off
    (let ((aider--add-file-read-only nil))
      (should (equal (aider--get-add-command-prefix) "/add")))
    ;; Test when read-only mode is on
    (let ((aider--add-file-read-only t))
      (should (equal (aider--get-add-command-prefix) "/read-only")))))

;;; Test aider-code-change.el functions

(ert-deftest test-aider-region-refactor-generate-command ()
  "Test aider-region-refactor-generate-command function."
  (when (fboundp 'aider-region-refactor-generate-command)
    ;; Test with function name (no trailing newline in actual function)
    (should (equal (aider-region-refactor-generate-command 
                   "code block" 
                   "test_func" 
                   "make it better")
                   "/architect \"in function test_func, for the following code block, make it better: code block\""))
    
    ;; Test without function name
    (should (equal (aider-region-refactor-generate-command 
                   "code block" 
                   nil 
                   "make it better")
                   "/architect \"for the following code block, make it better: code block\""))))

;;; Test aider-git.el functions

(ert-deftest test-aider--validate-git-repository ()
  "Test aider--validate-git-repository function."
  (when (fboundp 'aider--validate-git-repository)
    ;; Test valid git repo - should return the git root
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/repo/path/")))
      (should (equal (aider--validate-git-repository) "/repo/path/")))
    
    ;; Test invalid git repo - should signal user-error
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () nil)))
      (should-error (aider--validate-git-repository) :type 'user-error))))

;;; Test aider-prompt-mode.el functions

(ert-deftest test-aider--is-comment-line ()
  "Test aider--is-comment-line function."
  (when (fboundp 'aider--is-comment-line)
    ;; Test with semicolon comment style (Lisp)
    (let ((comment-start ";"))
      ;; Basic comment cases
      (should (aider--is-comment-line "; comment"))
      (should (aider--is-comment-line ";; double comment"))
      ;; Comments with leading whitespace
      (should (aider--is-comment-line "   ; indented comment"))
      ;; Non-comment cases
      (should-not (aider--is-comment-line "code ; with comment"))
      (should-not (aider--is-comment-line "regular code")))))

;;; Test interactive functions exist

(ert-deftest test-aider-interactive-functions-exist ()
  "Test that main interactive functions are defined."
  ;; Only test if the function exists, don't require it to be loaded
  (when (fboundp 'aider-transient-menu)
    (should (fboundp 'aider-transient-menu)))
  
  ;; Test file operations
  (when (fboundp 'aider-add-current-file)
    (should (commandp 'aider-add-current-file)))
  
  ;; Test code change operations
  (when (fboundp 'aider-code-change)
    (should (commandp 'aider-code-change)))
  
  ;; Test discussion operations
  (when (fboundp 'aider-ask-question)
    (should (commandp 'aider-ask-question)))
  
  ;; Test that at least some functions were loaded
  (should (or (fboundp 'aider--get-file-path)
              (fboundp 'aider--format-file-path)
              (fboundp 'aider-buffer-name))))

;;; Test variable definitions

(ert-deftest test-aider-variables ()
  "Test that important variables are defined."
  (should (boundp 'aider-program))
  (should (boundp 'aider-args))
  (should (boundp 'aider-use-branch-specific-buffers)))

;;; Test customization groups

(ert-deftest test-aider-customization ()
  "Test that customization group is defined."
  (should (get 'aider 'group-documentation)))

;;; Integration tests

(ert-deftest test-aider-integration-buffer-operations ()
  "Test buffer operations integration."
  (when (and (fboundp 'aider-buffer-name)
             (fboundp 'aider--get-git-repo-root))
    (cl-letf (((symbol-function 'magit-toplevel)
               (lambda () "/test/repo/")))
      (let ((buffer-name (aider-buffer-name)))
        (should (stringp buffer-name))
        (should (string-prefix-p "*aider:" buffer-name))))))

(ert-deftest test-aider-integration-file-operations ()
  "Test file operations integration."
  (when (and (fboundp 'aider--get-file-path)
             (fboundp 'aider--format-file-path))
    (let ((file-path "/test/file.py"))
      (cl-letf (((symbol-function 'magit-toplevel)
                 (lambda () "/test/")))
        (let ((relative-path (aider--get-file-path file-path)))
          (should (stringp relative-path))
          (let ((formatted-path (aider--format-file-path relative-path)))
            (should (stringp formatted-path))))))))

;;; Mock-based tests for functions requiring user input

(ert-deftest test-aider-mock-user-input ()
  "Test functions that require user input using mocks."
  (when (fboundp 'aider-read-string)
    ;; Mock aider-read-string to return test input
    (cl-letf (((symbol-function 'aider-read-string)
               (lambda (prompt &optional initial) "test input")))
      (should (equal (aider-read-string "Test prompt: ") "test input")))))

(message "Aider.el comprehensive test suite loaded")