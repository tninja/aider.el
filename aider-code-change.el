;;; aider-code-change.el --- Code change operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code change functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

(defcustom aider-todo-keyword-pair '("TODO" . "comments marked with TODO")
  "Pair of keyword and its definition for `aider-implement-todo`.
The car is the keyword string to search for in comments.
The cdr is the description of what these comments represent.
Another common choice is (\"AI!\" . \"comments ending with AI! that need implementation\")."
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
  (let ((command (aider-read-string "Enter architect discussion question: ")))
    (aider-current-file-command-and-switch "/architect " command)))

(defun aider-region-refactor-generate-command (region-text function-name user-command)
  "Generate the command string based on input parameters.
REGION-TEXT, FUNCTION-NAME, and USER-COMMAND."
  (let ((processed-region-text region-text))
    (if function-name
        (format "/architect \"in function %s, for the following code block, %s: %s\"\n"
                function-name user-command processed-region-text)
      (format "/architect \"for the following code block, %s: %s\"\n"
              user-command processed-region-text))))

;;;###autoload
(defun aider-function-or-region-refactor ()
  "Refactor code under cursor or in selected region.
If a region is selected, refactor that specific region.
Otherwise, refactor the function under cursor."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-in-function (and region-active function-name))
         (prompt (cond
                  (region-in-function (format "Refactor instruction for selected region in function '%s': " function-name))
                  (function-name (format "Refactor %s: " function-name))
                  (region-active "Refactor instruction for selected region: ")
                  (t "Refactor instruction: ")))
         (candidate-list '("Simplify this code while preserving functionality"
                          "Extract this logic into a separate helper function"
                          "Optimize this code for better performance"
                          "Improve error handling and edge cases"
                          "Refactor to reduce complexity and improve readability"
                          "Refactor this test, using better testing patterns, reducing duplication, and improving readability and maintainability. Maintain the current functionality of the tests."
                          "Make this code more maintainable and easier to test"
                          "Fix potential bugs or issues in this code"
                          "Improve variable names and add clarifying comments"))
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
      (aider-current-file-command-and-switch
       "/architect "
       (concat (format "refactor %s: " function-name) instruction)))
     ;; Fallback case
     (t (message "No function or region selected."))))
  (message "Refactoring request sent to Aider"))

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

(defun aider--is-todo-comment (line)
  "Check if LINE contains the configured TODO keyword.
Returns non-nil if the line is a comment and contains the keyword from `aider-todo-keyword-pair`."
  (when comment-start
    (let ((trimmed-line (string-trim line))
          (keyword (car aider-todo-keyword-pair)))
      (and (aider--is-comment-line trimmed-line)
           (string-match-p (regexp-quote keyword) trimmed-line)))))

;;;###autoload
(defun aider-implement-todo ()
  "Implement comments with configured keyword in current context.
If region is selected, implement that specific region.
If cursor is on a comment line with the configured keyword, implement that specific comment.
If cursor is inside a function, implement comments with the keyword for that function.
Otherwise implement comments with the keyword for the entire current file.

The keyword and its definition are configured in `aider-todo-keyword-pair`."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-todo-comment (aider--is-todo-comment current-line))
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
             (is-todo-comment
              (format "Please implement this comment in-place: '%s'. It contains %s '%s'. Please replace it with implementation. Keep the existing code structure and implement just this specific comment."
                      current-line definition keyword))
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
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (let ((is-test-file (string-match-p "test" (file-name-nondirectory buffer-file-name)))
          (function-name (which-function)))
      (cond
       ;; Test file case
       (is-test-file
        (if function-name
            (if (string-match-p "test" function-name)
                (let* ((initial-input
                       (format "Please implement test function '%s'. Follow standard unit testing practices and make it a meaningful test. Do not use Mock if possible."
                              function-name))
                      (user-command (aider-read-string "Test implementation instruction: " initial-input)))
                  (aider-current-file-command-and-switch "/architect " user-command))
              (message "Current function '%s' does not appear to be a test function." function-name))
          (message "Please place cursor inside a test function to implement.")))
       ;; Non-test file case
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

;;;###autoload
(defun aider-fix-failing-test-under-cursor ()
  "Report the current test failure to aider and ask it to fix the code.
This function assumes the cursor is on or inside a test function."
  (interactive)
  (if-let ((test-function-name (which-function)))
      (let* ((initial-input (format "The test '%s' is failing. Please analyze and fix the code to make the test pass. Don't break any other test"
                                   test-function-name))
             (test-output (aider-read-string "Architect question: " initial-input)))
        (aider-current-file-command-and-switch "/architect " test-output))
    (message "No test function found at cursor position.")))

(provide 'aider-code-change)

;;; aider-code-change.el ends here
