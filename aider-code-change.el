;;; aider-code-change.el --- Code change operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;;; Commentary:
;; This file provides code change functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

;; New function to get command from user and send it prefixed with "/code "
;;;###autoload
(defun aider-code-change ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/code \"."
  (interactive)
  (let ((command (aider-read-string "Enter code change requirement: ")))
    (aider-current-file-command-and-switch "/code " command)))

;; New function to get command from user and send it prefixed with "/architect "
;;;###autoload
(defun aider-architect-discussion ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/architect \"."
  (interactive)
  (let ((command (aider-read-string "Enter architect discussion question: ")))
    (aider-current-file-command-and-switch "/architect " command)))

(defun aider-region-refactor-generate-command (region-text function-name user-command)
  "Generate the command string based on REGION-TEXT, FUNCTION-NAME, and USER-COMMAND."
  (let ((processed-region-text region-text))
    (if function-name
        (format "/architect \"in function %s, for the following code block, %s: %s\"\n"
                function-name user-command processed-region-text)
      (format "/architect \"for the following code block, %s: %s\"\n"
              user-command processed-region-text))))

;;;###autoload
(defun aider-function-refactor ()
  "Get the function name under cursor and send refactor command to aider.
The command will be formatted as \"/architect\" followed by refactoring instructions
for the specified function."
  (interactive)
  (if-let ((function-name (which-function)))
      (let* ((prefix (format "refactor %s: " function-name))
             (prompt (format "Instruction to %s" prefix))
             (instruction (aider-read-string prompt))
             (command (format "/architect %s%s" prefix instruction)))
        (aider-current-file-command-and-switch "/architect " (concat prefix instruction)))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aider-region-refactor ()
  "Get a command from the user and send it to the corresponding aider comint buffer based on the selected region.
The command will be formatted as \"/architect \" followed by the user command and the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (user-command (aider-read-string "Enter your refactor instruction: "))
             (command (aider-region-refactor-generate-command region-text function-name user-command)))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No region selected.")))

;;;###autoload
(defun aider-function-or-region-refactor ()
  "Call aider-function-refactor when no region is selected, otherwise call aider-region-refactor."
  (interactive)
  (if (region-active-p)
      (aider-region-refactor)
    (aider-function-refactor)))

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
  "Implement TODO comments in current context.
If region is selected, implement that specific region.
If cursor is on a comment line, implement that specific comment.
If cursor is inside a function, implement TODOs for that function.
Otherwise implement TODOs for the entire current file."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
    (let* ((current-line (string-trim (thing-at-point 'line t)))
           (is-comment (aider--is-comment-line current-line))
           (function-name (which-function))
           (region-text (when (region-active-p)
                         (buffer-substring-no-properties 
                          (region-beginning) 
                          (region-end))))
           (initial-input
            (cond
             (region-text
              (format "Please implement this code block in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific block." 
                      region-text))
             (is-comment
              (format "Please implement this comment in-place: '%s'. It is already inside current code. Please replace it with implementation. Keep the existing code structure and implement just this specific comment." 
                      current-line))
             (function-name
              (format "Please implement the TODO items in-place in function '%s'. Keep the existing code structure and only implement the TODOs in comments." 
                      function-name))
             (t
              (format "Please implement all TODO items in-place in file '%s'. Keep the existing code structure and only implement the TODOs in comments." 
                      (file-name-nondirectory buffer-file-name)))))
           (user-command (aider-read-string "TODO implementation instruction: " initial-input))
           (command (format "/architect %s" user-command)))
      (aider-current-file-command-and-switch "/architect " user-command))))

;;;###autoload
(defun aider-write-unit-test ()
  "Generate unit test code for current buffer.
Do nothing if current buffer is not visiting a file.
If current buffer filename contains 'test':
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
          (aider-current-file-command-and-switch "/architect " user-command))
          )))))

;;;###autoload
(defun aider-fix-failing-test-under-cursor ()
  "Report the current test failure to aider and ask it to fix the code.
This function assumes the cursor is on or inside a test function."
  (interactive)
  (if-let ((test-function-name (which-function)))
      (let* ((initial-input (format "The test '%s' is failing. Please analyze and fix the code to make the test pass. Don't break any other test"
                                   test-function-name))
             (test-output (aider-read-string "Architect question: " initial-input))
             (command (format "/architect %s" test-output)))
        (aider-current-file-command-and-switch "/architect " test-output))
    (message "No test function found at cursor position.")))

(provide 'aider-code-change)

;;; aider-code-change.el ends here
