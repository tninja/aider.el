;;; aider-code-change.el --- Code change operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code change functionality for the Aider package.

;;; Code:

(require 'which-func)
(require 'flycheck nil t)
(require 'cl-lib)
(require 'subr-x)
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

(defun aider-region-change-generate-command (region-text function-name user-command)
  "Generate the command string based on input parameters.
REGION-TEXT, FUNCTION-NAME, and USER-COMMAND."
  (let ((processed-region-text region-text))
    (if function-name
        (format "/architect \"in function %s, for the following code block, %s: %s\""
                function-name user-command processed-region-text)
      (format "/architect \"for the following code block, %s: %s\""
              user-command processed-region-text))))

(defun aider--handle-comment-requirement (line-text function-name)
  "Handle a standalone comment requirement at point.
Delete the comment line, prompt for instruction, and send change command."
  (let* ((req (replace-regexp-in-string
               (concat "^[ \t]*"
                       (regexp-quote (string-trim-right comment-start))
                       "+[ \t]*")
               ""
               line-text))
         (default-prompt
          (if function-name
              (format "In function %s, change code according to requirement: %s"
                      function-name req)
            (format "Change code according to requirement: %s" req)))
         (instruction (aider-read-string
                       "Code change instruction: " default-prompt))
         (cmd (concat "/architect " instruction)))
    (save-excursion
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position)))))
    (aider-add-current-file)
    (aider--send-command cmd t)))

(defun aider--handle-region-or-function (region-active function-name)
  "Handle changeing of selected region or containing function."
  (let* ((region-text (and region-active
                           (buffer-substring-no-properties
                            (region-beginning)
                            (region-end))))
         ;; Check if region is all comment lines
         (is-multi-line-comment (and region-active
                                     region-text
                                     (aider--is-all-comment-lines region-text))))
    (cond
     ;; Handle multi-line comment region
     ((and region-active is-multi-line-comment)
      (let* ((comment-content (aider--extract-comment-content region-text))
             (default-prompt
              (if function-name
                  (format "In function %s, change code according to requirement: %s"
                          function-name comment-content)
                (format "Change code according to requirement: %s" comment-content)))
             (instruction (aider-read-string
                           "Code change instruction: " default-prompt))
             (cmd (concat "/architect " instruction)))
        ;; Delete the comment region
        (delete-region (region-beginning) (region-end))
        (aider-add-current-file)
        (aider--send-command cmd t)))
     ;; Rest of existing logic...
     (t
      (let* ((prompt (cond
                      ((and region-active function-name)
                       (format "Code change instruction for selected region in function '%s': "
                               function-name))
                      (function-name
                       (format "Change %s: " function-name))
                      (region-active
                       "Change instruction for selected region: ")
                      (t
                       "Change instruction: ")))
             (is-test-file (and buffer-file-name
                                (string-match-p "test"
                                                (file-name-nondirectory
                                                 buffer-file-name))))
             (candidate-list (if is-test-file
                                 '("Write a new unit test function based on the given description."
                                   "Change this test, using better testing patterns, reducing duplication, and improving readability and maintainability. Maintain the current functionality of the tests."
                                   "This test failed. Please analyze and fix the source code functions to make this test pass without changing the test itself. Don't break any other test"
                                   "Improve test assertions and add edge cases."
                                   "Extract this logic into a separate helper function")
                               '("Implement the function given description and hint in comment, make it be able to pass all unit-tests if there is"
                                 "Simplify this code, reduce complexity and improve readability while preserving functionality"
                                 "Fix potential bugs or issues in this code"
                                 "Given your code review feedback, make corresponding change"
                                 "Make this code more maintainable and easier to test"
                                 "Generate Docstring/Comment for This"
                                 "Improve error handling and edge cases"
                                 "Optimize this code for better performance"
                                 "Extract this logic into a separate helper function")))
             (instruction (aider-read-string prompt nil candidate-list)))
        (cond
         (region-active
          (let ((command (aider-region-change-generate-command
                          region-text function-name instruction)))
            (aider-add-current-file)
            (aider--send-command command t)))
         (function-name
          (when (aider-current-file-command-and-switch
                 "/architect "
                 (concat (format "change %s: " function-name)
                         instruction))
            (message "Code change request sent to Aider"))))))))

;;;###autoload
(defun aider-function-or-region-change ()
  "Change code under cursor or in selected region.
If a region is selected, change that specific region.
Otherwise, change the function under cursor.
Additionally, if cursor is on a standalone comment line (and no region),
treat that comment as the requirement, remove it, and send it."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (line-text    (string-trim (thing-at-point 'line t))))
    (cond
     ;; 1) comment-line requirement
     ((and (not region-active)
           (aider--is-comment-line line-text))
      (aider--handle-comment-requirement line-text function-name))
     ;; 2) nothing selected
     ((not (or region-active function-name))
      (message "No function or region selected."))
     ;; 3) region or function
     (t
      (aider--handle-region-or-function region-active function-name)))))

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

(defun aider--is-all-comment-lines (text)
  "Check if all lines in TEXT are comment lines."
  (when (and comment-start text)
    (let ((lines (split-string text "\n" t)))
      (and lines
           (cl-every (lambda (line)
                       (aider--is-comment-line line))
                     lines)))))

(defun aider--extract-comment-content (comment-text)
  "Extract the actual content from COMMENT-TEXT, removing comment markers."
  (when comment-start
    (let* ((comment-str (string-trim-right comment-start))
           (lines (split-string comment-text "\n" t))
           (content-lines
            (mapcar (lambda (line)
                      (replace-regexp-in-string
                       (concat "^[ \t]*"
                               (regexp-quote comment-str)
                               "+[ \t]*")
                       ""
                       (string-trim line)))
                    lines)))
      (string-join content-lines " "))))

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
      (if is-test-file
          (aider--write-unit-test-test-file function-name)
        (aider--write-unit-test-source-file function-name)))))

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

;;;###autoload
(defun aider-flycheck-fix-errors-in-scope ()
  "Ask Aider to generate a patch fixing Flycheck errors.
If a region is active, operate on that region.
Otherwise prompt to choose scope: current line, current function (if any), or whole file.
Requires the `flycheck` package to be installed and available."
  (interactive)
  (unless (featurep 'flycheck)
    (user-error "Flycheck package not found. This feature is unavailable"))
  (when (and (aider--validate-buffer-file) (bound-and-true-p flycheck-mode))
    (if (null flycheck-current-errors)
        (message "No Flycheck errors found in the current buffer.")
      (let* ((git-root (or (magit-toplevel) default-directory))
             (rel-file (file-relative-name buffer-file-name git-root))
             ;; determine start/end/scope-description via helper
             (scope-data (aider--choose-flycheck-scope))
             (start (nth 0 scope-data))
             (end (nth 1 scope-data))
             (scope-description (nth 2 scope-data)))
        ;; collect errors and bail if none in that scope
        (let ((errors-in-scope (aider-flycheck--get-errors-in-scope start end)))
          (if (null errors-in-scope)
              (message "No Flycheck errors found in %s." scope-description)
            (let* ((error-list-string
                    (aider-flycheck--format-error-list errors-in-scope rel-file))
                   (prompt
                    (if (string-equal "the entire file" scope-description)
                        (format "Please fix the following Flycheck errors in file %s:\n\n%s"
                                rel-file error-list-string)
                      (format "Please fix the following Flycheck errors in %s of file %s:\n\n%s"
                              scope-description
                              rel-file
                              error-list-string)))
                   (edited-prompt (aider-read-string "Edit prompt for Aider: " prompt)))
              (when (and edited-prompt (not (string-blank-p edited-prompt)))
                (aider-add-current-file)
                (when (aider--send-command (concat "/architect " edited-prompt) t)
                  (message "Sent request to Aider to fix %d Flycheck error(s) in %s."
                           (length errors-in-scope)
                           scope-description))))))))))

(defun aider--choose-flycheck-scope ()
  "Return a list (START END DESCRIPTION) for Flycheck fixing scope."
  (let* ((scope (if (region-active-p) 'region
                  (intern
                   (completing-read
                    "Select Flycheck‐fixing scope: "
                    (delq nil
                          `("current-line"
                            ,(when (which-function) "current-function")
                            "whole-file"))
                    nil t))))
         start end description)
    (pcase scope
      ('region
       (setq start (region-beginning)
             end   (region-end)
             description
             (format "the selected region (lines %d–%d)"
                     (line-number-at-pos start)
                     (line-number-at-pos end))))
      ('current-line
       (setq start            (line-beginning-position)
             end              (line-end-position)
             description       (format "current line (%d)"
                                       (line-number-at-pos (point)))))
      ('current-function
       (let ((bounds (bounds-of-thing-at-point 'defun)))
         (unless bounds
           (user-error "Not inside a function; cannot select current function"))
         (setq start            (car bounds)
               end              (cdr bounds)
               description       (format "function '%s' (lines %d–%d)"
                                        (which-function)
                                        (line-number-at-pos (car bounds))
                                        (line-number-at-pos (cdr bounds))))))
      ('whole-file
       (setq start            (point-min)
             end              (point-max)
             description       "the entire file"))
      (_
       (user-error "Unknown Flycheck scope %s" scope)))
    (list start end description)))

(defun aider--write-unit-test-test-file (function-name)
  "Handle unit test generation in a test file given FUNCTION-NAME context."
  (if (and function-name (let ((case-fold-search nil))
                           (string-match-p "test" function-name)))
      (let* ((initial-input (format "Please implement test function '%s'. Follow standard unit testing practices and make it a meaningful test. Do not use Mock if possible." function-name))
             (user-command (aider-read-string "Test implementation instruction: " initial-input)))
        (aider-current-file-command-and-switch "/architect " user-command))
    (let* ((initial-input "Write test functions given the feature requirement description: ")
           (user-command (aider-read-string "Feature requirement for tests: " initial-input)))
      (aider-current-file-command-and-switch "/architect " user-command))))

(defun aider--write-unit-test-source-file (function-name)
  "Handle unit test generation in source file given FUNCTION-NAME context."
  (let* ((common-instructions "Keep existing tests if there are. Follow standard unit testing practices. Do not use Mock if possible.")
         (initial-input (if function-name
                            (format "Please write unit test code for function '%s'. %s" function-name common-instructions)
                          (format "Please write unit test code for file '%s'. For each function %s" (file-name-nondirectory buffer-file-name) common-instructions)))
         (user-command (aider-read-string "Unit test generation instruction: " initial-input)))
    (aider-current-file-command-and-switch "/architect " user-command)))

(provide 'aider-code-change)

;;; aider-code-change.el ends here
