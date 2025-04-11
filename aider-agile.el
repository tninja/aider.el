;;; aider-agile.el --- Agile practices operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides agile practice operations such as refactoring and TDD cycle
;; for the Aider package. Extracted from aider-code-change.el.

;;; Code:

(require 'aider-core)
(require 'aider-file)

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
         ;; Provide different refactoring techniques based on whether a region is selected
         (refactoring-techniques
          (if region-active
              ;; Refactoring techniques for selected regions
              '(("Extract Method" . "Extract the selected code into a new method named [METHOD_NAME]. Identify parameters and return values needed, and place the new method in an appropriate location.")
                ("Extract Variable" . "Replace this complex expression with a well-named variable [VARIABLE_NAME]. Choose a name that clearly explains the expression's purpose.")
                ("Extract Parameter" . "Extract this expression into a new parameter named [PARAMETER_NAME] for the containing function. Update all call sites to pass this value as an argument.")
                ("Extract Field" . "Extract this expression into a class field named [FIELD_NAME]. Initialize the field appropriately and replace the expression with a reference to the field.")
                ("Decompose Conditional" . "Break down this complex conditional into smaller, more readable pieces. Extract conditions and branches into well-named methods that express the high-level logic."))
              ;; Refactoring techniques for entire functions or files
              '(("Rename Variable/Method" . "Rename [CURRENT_NAME] to [NEW_NAME]. Ensure all references are updated consistently following naming conventions appropriate for this codebase.")
                ("Inline Method" . "Replace calls to method [METHOD_NAME] with its body. Ensure the inlining doesn't change behavior or introduce bugs, and remove the original method if it's no longer needed.")
                ("Inline Variable" . "Replace all references to variable [VARIABLE_NAME] with its value. Ensure the inlining doesn't change behavior or introduce bugs.")
                ("Move Method" . "Move method [METHOD_NAME] to class [TARGET_CLASS]. Update all references to use the new location and consider creating a delegation if needed.")
                ("Replace Conditional with Polymorphism" . "Replace this conditional logic with polymorphic objects. Create appropriate class hierarchy and move conditional branches to overridden methods.")
                ("Introduce Parameter Object" . "Replace these related parameters with a single parameter object named [OBJECT_NAME]. Create an appropriate class for the parameter object."))))
         (technique-names (mapcar #'car refactoring-techniques))
         (prompt (if region-active
                     "Select refactoring technique for selected region: "
                   "Select refactoring technique: "))
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-description (cdr (assoc selected-technique refactoring-techniques)))
         ;; Replace placeholders with user input for techniques that need parameters
         (prompt-with-params
          (cond
           ((string= selected-technique "Extract Method")
            (let ((method-name (read-string "New method name: ")))
              (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name technique-description t)))
           ((string= selected-technique "Rename Variable/Method")
            (let* ((current-name (or (thing-at-point 'symbol t)
                                    (read-string "Current name: ")))
                   (new-name (read-string (format "Rename '%s' to: " current-name))))
              (replace-regexp-in-string 
               "\\[NEW_NAME\\]" new-name
               (replace-regexp-in-string 
                "\\[CURRENT_NAME\\]" current-name technique-description t) 
               t)))
           ((string= selected-technique "Inline Method")
            (let ((method-name (or (thing-at-point 'symbol t)
                                  (read-string "Method to inline: "))))
              (replace-regexp-in-string "\\[METHOD_NAME\\]" method-name technique-description t)))
           ((string= selected-technique "Inline Variable")
            (let ((variable-name (or (thing-at-point 'symbol t)
                                    (read-string "Variable to inline: "))))
              (replace-regexp-in-string "\\[VARIABLE_NAME\\]" variable-name technique-description t)))
           ((string= selected-technique "Move Method")
            (let ((method-name (or current-function
                                  (read-string "Method to move: ")))
                  (target-class (read-string "Target class: ")))
              (replace-regexp-in-string 
               "\\[TARGET_CLASS\\]" target-class
               (replace-regexp-in-string 
                "\\[METHOD_NAME\\]" method-name technique-description t) 
               t)))
           ((string= selected-technique "Extract Variable")
            (let ((var-name (read-string "New variable name: ")))
              (replace-regexp-in-string "\\[VARIABLE_NAME\\]" var-name technique-description t)))
           ((string= selected-technique "Extract Parameter")
            (let ((param-name (read-string "New parameter name: ")))
              (replace-regexp-in-string "\\[PARAMETER_NAME\\]" param-name technique-description t)))
           ((string= selected-technique "Introduce Parameter Object")
            (let ((object-name (read-string "Parameter object name: ")))
              (replace-regexp-in-string "\\[OBJECT_NAME\\]" object-name technique-description t)))
           ((string= selected-technique "Extract Field")
            (let ((field-name (read-string "New field name: ")))
              (replace-regexp-in-string "\\[FIELD_NAME\\]" field-name technique-description t)))
           (t technique-description)))
         (initial-final-instruction (format "%s %s. %s"
                                         selected-technique
                                         context-description
                                         prompt-with-params))
         (final-instruction (aider-read-string "Edit refactoring instruction: " initial-final-instruction))
         (command (if region-active
                     (format "\"%s\n\nSelected code:\n%s\""
                             final-instruction
                             region-text)
                   (format "\"%s\"" final-instruction))))
    (aider-current-file-command-and-switch "/architect " command)
    (message "%s refactoring request sent to Aider. After code refactored, better to re-run unit-tests." selected-technique)))

;;;###autoload
(defun aider-tdd-cycle ()
  "Guide through Test Driven Development cycle (Red-Green-Refactor).
Helps users follow Kent Beck's TDD methodology with AI assistance.
Works with both source code and test files that have been added to aider."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (is-test-file (and current-file 
                           (string-match-p "\\(test\\|spec\\)" 
                                          (file-name-nondirectory current-file))))
         (function-name (which-function))
         (cycle-stage (completing-read 
                       "Select TDD stage: " 
                       '("1. Red (Write failing test)" 
                         "2. Green (Make test pass)" 
                         "3. Refactor (Improve code quality)")
                       nil t))
         (stage-num (string-to-number (substring cycle-stage 0 1))))
    (cond
     ;; Red stage - write failing test
     ((= stage-num 1)
      (let* ((initial-input 
              (if function-name 
                  (format "Write a failing test for function '%s': " function-name)
                "Write a failing test for this feature: "))
             (feature-desc (aider-read-string "Describe the feature to test: " initial-input))
             (tdd-instructions 
              (format "%s Follow TDD principles - write only the test now, not the implementation. The test should fail when run because the functionality doesn't exist yet."
                      feature-desc)))
        (aider-current-file-command-and-switch "/architect " tdd-instructions)))
     ;; Green stage - make test pass
     ((= stage-num 2)
      (let* ((initial-input
              (if function-name
                  (format "Implement function '%s' with minimal code to make tests pass: " function-name)
                "Implement the minimal code needed to make the failing test pass: "))
             (implementation-desc (aider-read-string "Implementation instruction: " initial-input))
             (tdd-instructions 
              (format "%s Follow TDD principles - implement only the minimal code needed to make the test pass. Don't over-engineer or implement features not required by the test."
                      implementation-desc)))
        (aider-current-file-command-and-switch "/architect " tdd-instructions)))
     ;; Refactor stage
     ((= stage-num 3)
      (let* ((context-desc (if function-name
                              (format "in function '%s'" function-name)
                            "in this code"))
             (initial-input (format "Refactor the code %s while ensuring all tests continue to pass: " context-desc))
             (refactoring-suggestions
              (list
               (format "Improve naming of variables and functions %s" context-desc)
               (format "Extract duplicated code into helper methods %s" context-desc)
               (format "Simplify complex conditionals %s" context-desc)
               (format "Improve code organization and structure %s" context-desc)))
             (refactor-desc (aider-read-string 
                            "Describe the refactoring needed: " 
                            initial-input
                            refactoring-suggestions))
             (tdd-instructions 
              (format "%s Follow TDD principles - improve code quality without changing behavior. Ensure all tests still pass after refactoring."
                      refactor-desc)))
        (aider-current-file-command-and-switch "/architect " tdd-instructions))))))

(provide 'aider-agile)
;;; aider-agile.el ends here
