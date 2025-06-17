;;; aider-code-change.el --- Code change operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code change functionality for the Aider package.

;;; Code:

(require 'which-func)
(require 'flycheck nil t)
(require 'cl-lib)
(require 'magit)

(require 'aider-core)
(require 'aider-file)

(defcustom aider-todo-keyword-pair '("TODO" . "comment line START with string: TODO:")
  "Pair of keyword and its definition for `aider-implement-todo`.
The car is the keyword string to search for in comments.
The cdr is the description of what these comments represent.
Another common choice is (\"AI!\" . \"comment line ending with string: AI!\")."
  :type '(cons string string)
  :group 'aider)

;; New function to get command from user and send it prefixed with "/code "
;;;###autoload
(defun aider-code-change ()
  "Make direct code change given user's prompt."
  (interactive)
  (let ((command (aider-read-string "Enter code change requirement: ")))
    (aider-current-file-command-and-switch "/code " command)))

;; New function to get command from user and send it prefixed with "/architect "
;;;###autoload
(defun aider-architect-discussion ()
  "Discuss with aider with the given prompt, and choose if we want to accept it."
  (interactive)
  (let ((command (aider-read-string "Enter architectural discussion topic/question: ")))
    (aider-current-file-command-and-switch "/architect " command)))

(defun aider-region-refactor-generate-command (region-text function-name user-command)
  "Generate the command string based on input parameters.
REGION-TEXT, FUNCTION-NAME, and USER-COMMAND."
  (let ((processed-region-text region-text))
    (if function-name
        (format "/architect \"in function %s, for the following code block, %s: %s\""
                function-name user-command processed-region-text)
      (format "/architect \"for the following code block, %s: %s\""
              user-command processed-region-text))))

;;;###autoload
(defun aider-function-or-region-refactor ()
  "Refactor code under cursor or in selected region.
If a region is selected, refactor that specific region.
Otherwise, refactor the function under cursor."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p)))
    (if (not (or region-active function-name))
        (message "No function or region selected.")
      ;; Original let* body continues here if region or function exists
      (let* ((region-in-function (and region-active function-name))
             (prompt (cond
                      (region-in-function (format "Code change instruction for selected region in function '%s': " function-name))
                      (function-name (format "Change %s: " function-name))
                  (region-active "Refactor instruction for selected region: ")
                  (t "Refactor instruction: ")))
         (is-test-file (and buffer-file-name
                            (string-match-p "test" (file-name-nondirectory buffer-file-name))))
         (candidate-list (if is-test-file
                             '("Write a new unit test function based on the given description."
                               "Refactor this test, using better testing patterns, reducing duplication, and improving readability and maintainability. Maintain the current functionality of the tests."
                               "This test failed. Please analyze and fix the source code functions to make this test pass without changing the test itself. Don't break any other test"
                               "Improve test assertions and add edge cases."
                               "Extract this logic into a separate helper function")
                           '("Implement the function given description and hint in comment, make it be able to pass all unit-tests if there is"
                             "Simplify this code, reduce complexity and improve readability while preserving functionality"
                             "Fix potential bugs or issues in this code"
                             "Given your code review feedback, make corresponding change" ;; code review using /ask firstly
                             "Make this code more maintainable and easier to test"
                             "Generate Docstring/Comment for This"
                             "Improve error handling and edge cases"
                             "Optimize this code for better performance"
                             "Extract this logic into a separate helper function")))
         (instruction (aider-read-string prompt nil candidate-list))
         (region-text (and region-active
                           (buffer-substring-no-properties (region-beginning) (region-end)))))
    (cond
     ;; Region selected case
     (region-active
      (let ((command (aider-region-refactor-generate-command
                      region-text function-name instruction)))
        (aider-add-current-file)
        (aider--send-command command t)))
     ;; Function case
     (function-name
      (when (aider-current-file-command-and-switch
             "/architect "
             (concat (format "refactor %s: " function-name) instruction))
        (message "Code change request sent to Aider"))))))))

(defun aider--is-comment-line (line)
  "Check if LINE is a comment line based on current buffer's comment syntax.
Returns non-nil if LINE starts with one or more comment characters,
ignoring leading whitespace."
  (when comment-start
    (let ((comment-str (string-trim-right comment-start)))
      (string-match-p (concat "^[ \t]*"
                             (regexp-quote comment-str)
                             "+")
                     (string-trim-left line)))))

;;;###autoload
(defun aider-implement-todo ()
  "Implement comments with configured keyword in current context.
If region is selected, implement that specific region.
If cursor is on a comment line with the configured keyword,
implement that specific comment.
If cursor is inside a function, implement comments with the
keyword for that function.
Otherwise implement comments with the keyword for the entire
current file.

The keyword and its definition are configured in
`aider-todo-keyword-pair`."
  (interactive)
  (when (aider--validate-buffer-file)
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-comment (aider--is-comment-line current-line))
           (function-name (which-function))
           (region-text (when (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end))))
           (keyword (car aider-todo-keyword-pair))
           (definition (cdr aider-todo-keyword-pair))
           (initial-input
            (cond
             (region-text
              (format "Please implement this code block in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific block."
                      region-text))
             (is-comment
              (format "Please implement this comment in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific comment."
                      current-line))
             (function-name
              (format "Please implement all %s in-place in function '%s'. The %s are %s. Keep the existing code structure and only implement these marked items."
                      keyword function-name keyword definition))
             (t
              (format "Please implement all %s in-place in file '%s'. The %s are %s. Keep the existing code structure and only implement these marked items."
                      keyword (file-name-nondirectory buffer-file-name) keyword definition))))
           (user-command (aider-read-string "TODO implementation instruction: " initial-input)))
      (aider-current-file-command-and-switch "/architect " user-command))))

;;;###autoload
(defun aider-write-unit-test ()
  "Generate unit test code for current buffer.
Do nothing if current buffer is not visiting a file.
If current buffer filename contains \"test\":
  - If cursor is inside a test function, implement that test
  - Otherwise show message asking to place cursor inside a test function
Otherwise:
  - If cursor is on a function, generate unit test for that function
  - Otherwise generate unit tests for the entire file"
  (interactive)
  (when (aider--validate-buffer-file)
    (let ((is-test-file (string-match-p "test" (file-name-nondirectory buffer-file-name)))
          (function-name (which-function)))
      (cond
       ;; Test file case
       (is-test-file
        (if (and function-name (let ((case-fold-search nil))
                                 (string-match-p "test" function-name))) ;; seems that the cursor is under a test function
            ;; implement a unit-test function given context of source function
            (let* ((initial-input
                    (format "Please implement test function '%s'. Follow standard unit testing practices and make it a meaningful test. Do not use Mock if possible."
                            function-name))
                   (user-command (aider-read-string "Test implementation instruction: " initial-input)))
              (aider-current-file-command-and-switch "/architect " user-command))
          ;; in test file, but not in a test function. write unit-test first given description
          (let* ((initial-input "Write test functions given the feature requirement description: ")
                 (user-command (aider-read-string "Feature requirement for tests: " initial-input)))
            (aider-current-file-command-and-switch "/architect " user-command))))
       ;; Non-test file case, assuming it is main source code
       (t
        (let* ((common-instructions "Keep existing tests if there are. Follow standard unit testing practices. Do not use Mock if possible.")
               (initial-input
                (if function-name
                    (format "Please write unit test code for function '%s'. %s"
                           function-name common-instructions)
                  (format "Please write unit test code for file '%s'. For each function %s"
                         (file-name-nondirectory buffer-file-name) common-instructions)))
               (user-command (aider-read-string "Unit test generation instruction: " initial-input)))
          (aider-current-file-command-and-switch "/architect " user-command)))))))

;;; New Flycheck integration
(defun aider-flycheck--get-errors-in-scope (start end)
  "Return a list of Flycheck errors within the given START and END buffer positions."
  (when (and (bound-and-true-p flycheck-mode) flycheck-current-errors)
    (cl-remove-if-not
     (lambda (err)
       (let ((pos (flycheck-error-pos err)))
         (and (integerp pos) (>= pos start) (< pos end))))
     flycheck-current-errors)))

(defun aider-flycheck--format-error-list (errors file-path-for-error-reporting)
  "Formats a list string for multiple Flycheck ERRORS.
FILE-PATH-FOR-ERROR-REPORTING is the relative file path
to include in each error report."
  (let ((error-reports '()))
    (dolist (err errors)
      (let* ((line (flycheck-error-line err))
             (col (flycheck-error-column err))
             (msg (flycheck-error-message err)))
        (if (and (integerp line) (integerp col))
            (let* ((error-line-text
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
              (push (format "File: %s:%d:%d\nError: %s\nContext line:\n%s"
                            file-path-for-error-reporting line col msg error-line-text)
                    error-reports))
          (progn
            (message "Aider: Flycheck error for %s. Line: %S, Col: %S. Full location/context not available. Sending general error info."
                     file-path-for-error-reporting line col)
            (push (format "File: %s (Location: Line %s, Column %s)\nError: %s"
                          file-path-for-error-reporting
                          (if (integerp line) (format "%d" line) "N/A")
                          (if (integerp col) (format "%d" col) "N/A")
                          msg)
                  error-reports)))))
    (mapconcat #'identity (nreverse error-reports) "\n\n")))

(defun aider--determine-flycheck-scope-parameters (apply-to-whole-file-p)
  "Determine and return the scope for Flycheck error fixing.
Returns a list (SCOPE-START SCOPE-END SCOPE-DESCRIPTION).
Signals a `user-error` if a valid scope cannot be determined.
APPLY-TO-WHOLE-FILE-P is non-nil if prefix argument was used."
  (let (scope-start scope-end scope-description)
    (cond
     (apply-to-whole-file-p
      (setq scope-start (point-min)
            scope-end (point-max)
            scope-description "the entire file"))
     ((region-active-p)
      (setq scope-start (region-beginning)
            scope-end (region-end)
            scope-description (format "the selected region (lines %d-%d)"
                                      (line-number-at-pos scope-start)
                                      (line-number-at-pos scope-end))))
     ((which-function)
      (let ((bounds (bounds-of-thing-at-point 'defun)))
        (if bounds
            (setq scope-start (car bounds)
                  scope-end (cdr bounds)
                  scope-description (format "function '%s' (lines %d-%d)"
                                            (which-function)
                                            (line-number-at-pos scope-start)
                                            (line-number-at-pos scope-end)))
          (user-error "Could not determine bounds for function '%s'. Select a region or use C-u for the entire file" (which-function)))))
     (t
      (user-error "No region active and not inside a function. Select a region, move into a function, or use C-u to process the entire file")))
    (list scope-start scope-end scope-description)))

;;;###autoload
(defun aider-flycheck-fix-errors-in-scope (&optional raw-prefix-arg)
  "Ask Aider to generate a patch fixing Flycheck errors.
With a prefix argument RAW-PREFIX-ARG (e.g., \\[universal-argument]), applies to the entire file.
Otherwise, applies to the active region if any.
If no region is active and no prefix argument, applies to the current function.
If not in a function, no region active, and no prefix arg, an error is signaled.
This command requires the `flycheck` package to be installed and available."
  (interactive "P")
  (unless (featurep 'flycheck)
    (user-error "Flycheck package not found. This feature is unavailable"))
  (when (and (aider--validate-buffer-file) (bound-and-true-p flycheck-mode))
    (unless flycheck-current-errors
      (message "No Flycheck errors found in the current buffer.")
      (cl-return-from aider-flycheck-fix-errors-in-scope nil))
    (let* ((git-root (or (magit-toplevel) default-directory))
           (rel-file (file-relative-name buffer-file-name git-root)))
      (cl-destructuring-bind (scope-start scope-end scope-description)
          (aider--determine-flycheck-scope-parameters raw-prefix-arg)
        (let ((errors-in-scope (aider-flycheck--get-errors-in-scope scope-start scope-end)))
          (unless errors-in-scope
            (message "No Flycheck errors found in %s." scope-description)
            (cl-return-from aider-flycheck-fix-errors-in-scope nil))
          (let ((error-list-string (aider-flycheck--format-error-list errors-in-scope rel-file)))
            (if (string-blank-p error-list-string)
                (message "No actionable Flycheck errors to send for %s." scope-description)
              (let* ((prompt
                      (if (string-equal "the entire file" scope-description)
                          (format "Please fix the following Flycheck errors in file %s:\n\n%s"
                                  rel-file error-list-string)
                        (format "Please fix the following Flycheck errors in %s of file %s:\n\n%s"
                                scope-description rel-file error-list-string)))
                     (edited-prompt (aider-read-string "Edit prompt for Aider: " prompt)))
                (when (and edited-prompt (not (string-blank-p edited-prompt)))
                  (aider-add-current-file)
                  (when (aider--send-command (concat "/architect " edited-prompt) t)
                    (message "Sent request to Aider to fix %d Flycheck error(s) in %s." (length errors-in-scope) scope-description)))))))))))

(provide 'aider-code-change)

;;; aider-code-change.el ends here
