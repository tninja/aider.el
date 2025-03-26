;;; aider-code-change.el --- Code change operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides code change functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

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
                             "Make this code more maintainable and easier to test"
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
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file.")
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

;;;###autoload
(defun aider-refactor-book-method ()
  "Apply famous refactoring techniques from Martin Fowler's book.
Uses current context (function, class, selected region) to generate appropriate prompts.
Works across different programming languages."
  (interactive)
  (let* ((region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (current-function (which-function))
         (file-name (when buffer-file-name (file-name-nondirectory buffer-file-name)))
         (context-description (cond
                              (current-function (format "in function '%s'" current-function))
                              (file-name (format "in file '%s'" file-name))
                              (t "in current context")))
         (refactoring-techniques
          '(("Extract Method" . "Extract the selected code into a new method named [METHOD_NAME]. Identify parameters and return values needed, and place the new method in an appropriate location.")
            ("Rename Variable/Method" . "Rename [CURRENT_NAME] to [NEW_NAME]. Ensure all references are updated consistently following naming conventions appropriate for this codebase.")
            ("Inline Method" . "Replace calls to method [METHOD_NAME] with its body. Ensure the inlining doesn't change behavior or introduce bugs, and remove the original method if it's no longer needed.")
            ("Move Method" . "Move method [METHOD_NAME] to class [TARGET_CLASS]. Update all references to use the new location and consider creating a delegation if needed.")
            ("Replace Conditional with Polymorphism" . "Replace this conditional logic with polymorphic objects. Create appropriate class hierarchy and move conditional branches to overridden methods.")
            ("Extract Variable" . "Replace this complex expression with a well-named variable [VARIABLE_NAME]. Choose a name that clearly explains the expression's purpose.")
            ("Introduce Parameter Object" . "Replace these related parameters with a single parameter object named [OBJECT_NAME]. Create an appropriate class for the parameter object.")
            ("Decompose Conditional" . "Break down this complex conditional into smaller, more readable pieces. Extract conditions and branches into well-named methods that express the high-level logic.")
            ("Consolidate Duplicate Code" . "Extract this duplicated code into a shared method named [METHOD_NAME]. Identify variations that need to be parameterized.")
            ("Replace Temp with Query" . "Replace temporary variable [TEMP_NAME] with a query method named [METHOD_NAME]. Extract the expression into a method with a descriptive name.")))
         (technique-names (mapcar #'car refactoring-techniques))
         (selected-technique (aider-read-string "Select refactoring technique: " nil technique-names))
         (technique-description (cdr (assoc selected-technique refactoring-techniques)))
         ;; Replace placeholders with user input for techniques that need parameters
         (prompt-with-params 
          (cond
           ((string= selected-technique "Extract Method")
            (let ((method-name (aider-read-string "New method name: ")))
              (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name technique-description)))
           
           ((string= selected-technique "Rename Variable/Method")
            (let ((current-name (aider-read-string "Current name: "))
                  (new-name (aider-read-string "New name: ")))
              (-> technique-description
                  (replace-regexp-in-string "\\[CURRENT_NAME\\]" current-name)
                  (replace-regexp-in-string "\\[NEW_NAME\\]" new-name))))
           
           ((string= selected-technique "Inline Method")
            (let ((method-name (aider-read-string "Method to inline: ")))
              (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name technique-description)))
           
           ((string= selected-technique "Move Method")
            (let ((method-name (aider-read-string "Method to move: "))
                  (target-class (aider-read-string "Target class: ")))
              (-> technique-description
                  (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name)
                  (replace-regexp-in-string "\\[TARGET_CLASS\\]" target-class))))
           
           ((string= selected-technique "Extract Variable")
            (let ((var-name (aider-read-string "New variable name: ")))
              (replace-regexp-in-string "\\[VARIABLE_NAME\\]" var-name technique-description)))
           
           ((string= selected-technique "Introduce Parameter Object")
            (let ((object-name (aider-read-string "Parameter object name: ")))
              (replace-regexp-in-string "\\[OBJECT_NAME\\]" object-name technique-description)))
           
           ((string= selected-technique "Consolidate Duplicate Code")
            (let ((method-name (aider-read-string "New shared method name: ")))
              (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name technique-description)))
           
           ((string= selected-technique "Replace Temp with Query")
            (let ((temp-name (aider-read-string "Temporary variable name: "))
                  (method-name (aider-read-string "New query method name: ")))
              (-> technique-description
                  (replace-regexp-in-string "\\[TEMP_NAME\\]" temp-name)
                  (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name))))
           
           (t technique-description)))
         (final-instruction (format "%s %s. %s" 
                                   selected-technique 
                                   context-description
                                   prompt-with-params))
         (command (if region-active
                     (format "/architect \"%s\n\nSelected code:\n%s\"" 
                             final-instruction
                             region-text)
                   (format "/architect \"%s\"" final-instruction))))
    (aider-add-current-file)
    (aider--send-command command t)
    (message "%s refactoring request sent to Aider" selected-technique)))

(provide 'aider-code-change)

;;; aider-code-change.el ends here
