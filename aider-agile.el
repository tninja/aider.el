;;; aider-agile.el --- Agile practices operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides agile practice operations such as refactoring and TDD cycle
;; for the Aider package. Extracted from aider-code-change.el.

;;; Code:

(require 'aider-core)
(require 'aider-file)

(defun aider--get-refactoring-context ()
  "Get the current context for refactoring."
  (let ((region-active (region-active-p))
        (current-function (which-function))
        (file-name (when buffer-file-name (file-name-nondirectory buffer-file-name))))
    (list :region-active region-active
          :region-text (when region-active
                         (buffer-substring-no-properties (region-beginning) (region-end)))
          :current-function current-function
          :file-name file-name
          :context-description (cond
                               (current-function (format "in function '%s'" current-function))
                               (file-name (format "in file '%s'" file-name))
                               (t "in current context")))))

(defun aider--get-refactoring-techniques (region-active)
  "Return appropriate refactoring techniques based on REGION-ACTIVE."
  (if region-active
      ;; Refactoring techniques for selected regions
      '(("Suggest Refactoring Strategy" . "Let the LLM analyze the context and suggest the best refactoring technique.") ;; <-- Added
        ("Extract Method" . "Extract the selected code into a new method named [METHOD_NAME]. Identify parameters and return values needed, and place the new method in an appropriate location.")
        ("Extract Variable" . "Replace this complex expression with a well-named variable [VARIABLE_NAME]. Choose a name that clearly explains the expression's purpose.")
        ("Extract Parameter" . "Extract this expression into a new parameter named [PARAMETER_NAME] for the containing function. Update all call sites to pass this value as an argument.")
        ("Extract Field" . "Extract this expression into a class field named [FIELD_NAME]. Initialize the field appropriately and replace the expression with a reference to the field.")
        ("Decompose Conditional" . "Break down this complex conditional into smaller, more readable pieces. Extract conditions and branches into well-named methods that express the high-level logic.")
        ("Extract Class" . "Extract related fields and methods from the selected code or containing class into a new class named [NEW_CLASS_NAME]. Update the original class to use the new class.")
        ("Replace Nested Conditional with Guard Clauses" . "Simplify the selected nested conditional logic by using guard clauses. Check for edge cases or simple conditions first and return early.")
        ("Replace Magic Number with Symbolic Constant" . "Replace the selected magic number or string literal with a well-named constant [CONSTANT_NAME]. Define the constant appropriately.")
        ("Introduce Assertion" . "Add an assertion to the selected location to document an assumption about the program state. Specify the condition to assert [ASSERTION_CONDITION].")
        ("Consolidate Conditional Expression" . "Combine multiple conditional checks within the selection that lead to the same result into a single, clearer conditional expression."))
    ;; Refactoring techniques for entire functions or files
    '(("Suggest Refactoring Strategy" . "Let the LLM analyze the context and suggest the best refactoring technique.") ;; <-- Added
      ("Rename Variable/Method" . "Rename [CURRENT_NAME] to [NEW_NAME]. Ensure all references are updated consistently following naming conventions appropriate for this codebase.")
      ("Inline Method" . "Replace calls to method [METHOD_NAME] with its body. Ensure the inlining doesn't change behavior or introduce bugs, and remove the original method if it's no longer needed.")
      ("Inline Variable" . "Replace all references to variable [VARIABLE_NAME] with its value. Ensure the inlining doesn't change behavior or introduce bugs.")
      ("Move Method" . "Move method [METHOD_NAME] to class [TARGET_CLASS]. Update all references to use the new location and consider creating a delegation if needed.")
      ("Replace Conditional with Polymorphism" . "Replace this conditional logic with polymorphic objects. Create appropriate class hierarchy and move conditional branches to overridden methods.")
      ("Introduce Parameter Object" . "Replace these related parameters with a single parameter object named [OBJECT_NAME]. Create an appropriate class for the parameter object.")
      ("Extract Class" . "Extract related fields and methods from the current class into a new class named [NEW_CLASS_NAME]. Update the original class to use the new class.")
      ("Replace Nested Conditional with Guard Clauses" . "Simplify nested conditional logic within the current function/context by using guard clauses. Check for edge cases or simple conditions first and return early.")
      ("Encapsulate Field" . "Make the field [FIELD_NAME] private and provide public getter and setter methods for access. Update all direct accesses to use these methods.")
      ("Replace Magic Number with Symbolic Constant" . "Find magic numbers or string literals within the current function/context and replace them with a well-named constant [CONSTANT_NAME]. Define the constant appropriately.")
      ("Pull Up Method" . "Move method [METHOD_NAME] from the current class to its superclass [SUPERCLASS_NAME]. Ensure the method is applicable to the superclass context.")
      ("Push Down Method" . "Move method [METHOD_NAME] from the current class to specific subclass(es) [SUBCLASS_NAMES] where it is actually used.")
      ("Introduce Assertion" . "Add an assertion within the current function/context to document an assumption about the program state. Specify the condition to assert [ASSERTION_CONDITION].")
      ("Consolidate Conditional Expression" . "Combine multiple conditional checks within the current function/context that lead to the same result into a single, clearer conditional expression."))))

(defun aider--process-refactoring-parameters (selected-technique technique-description context)
  "Process parameters for SELECTED-TECHNIQUE with TECHNIQUE-DESCRIPTION.
Uses CONTEXT."
  (let ((current-function (plist-get context :current-function)))
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
     ((string= selected-technique "Extract Class")
      (let ((new-class-name (read-string "New class name: ")))
        (replace-regexp-in-string "\\[NEW_CLASS_NAME\\]" new-class-name technique-description t)))
     ((string= selected-technique "Replace Magic Number with Symbolic Constant")
      (let ((constant-name (read-string "Constant name: ")))
        (replace-regexp-in-string "\\[CONSTANT_NAME\\]" constant-name technique-description t)))
     ((string= selected-technique "Introduce Assertion")
      (let ((assertion-condition (read-string "Assertion condition: ")))
        (replace-regexp-in-string "\\[ASSERTION_CONDITION\\]" assertion-condition technique-description t)))
     ((string= selected-technique "Encapsulate Field")
      (let ((field-name (or (thing-at-point 'symbol t)
                           (read-string "Field to encapsulate: "))))
        (replace-regexp-in-string "\\[FIELD_NAME\\]" field-name technique-description t)))
     ((string= selected-technique "Pull Up Method")
      (let* ((method-name (or current-function
                             (thing-at-point 'symbol t)
                             (read-string "Method to pull up: ")))
             (superclass-name (read-string "Superclass name: ")))
        (replace-regexp-in-string
         "\\[SUPERCLASS_NAME\\]" superclass-name
         (replace-regexp-in-string
          "\\[METHOD_NAME\\]" method-name technique-description t)
         t)))
     ((string= selected-technique "Push Down Method")
      (let* ((method-name (or current-function
                             (thing-at-point 'symbol t)
                             (read-string "Method to push down: ")))
             (subclass-names (read-string "Comma-separated subclass names: ")))
        (replace-regexp-in-string
         "\\[SUBCLASS_NAMES\\]" subclass-names
         (replace-regexp-in-string
          "\\[METHOD_NAME\\]" method-name technique-description t)
         t)))
     (t technique-description))))

(defun aider--handle-specific-refactoring (selected-technique all-techniques context tdd-mode)
  "Handle the case where a specific refactoring technique is chosen.
Uses SELECTED-TECHNIQUE, ALL-TECHNIQUES, CONTEXT, and TDD-MODE.
If TDD-MODE is non-nil, adds TDD constraints to the instruction."
  (let* ((region-active (plist-get context :region-active))
         (region-text (plist-get context :region-text))
         (context-description (plist-get context :context-description))
         (technique-description (cdr (assoc selected-technique all-techniques)))
         (prompt-with-params (aider--process-refactoring-parameters
                              selected-technique technique-description context))
         (base-instruction (format "%s %s. %s"
                                   selected-technique
                                   context-description
                                   prompt-with-params))
         ;; Add TDD constraint if in TDD mode
         (tdd-constraint (if tdd-mode " Ensure all tests still pass after refactoring." ""))
         (initial-instruction (concat base-instruction tdd-constraint))
         (final-instruction (aider-read-string "Edit refactoring instruction: " initial-instruction))
         (command (if region-active
                      (format "\"%s\n\nSelected code:\n%s\""
                              final-instruction
                              region-text)
                    (format "\"%s\"" final-instruction)))
         (message-suffix (if tdd-mode " during TDD refactor stage" "")))
    (when (aider-current-file-command-and-switch "/architect " command)
      (message "%s refactoring request sent to Aider%s. After code refactored, better to re-run unit-tests."
               selected-technique message-suffix))))

(defun aider--handle-ask-llm-suggestion (context tdd-mode)
  "Handle the case where the user asks the LLM for a refactoring suggestion.
Uses CONTEXT and TDD-MODE.
If TDD-MODE is non-nil, adds TDD constraints to the prompt."
  (let* ((region-active (plist-get context :region-active))
         (region-text (plist-get context :region-text))
         (current-function (plist-get context :current-function))
         (context-info (cond
                        (region-active "Selected code region")
                        (current-function (format "Function '%s'" current-function))
                        (t "All added files")))
         (code-snippet (if region-active
                           (format "\n```\n%s\n```" region-text)
                         ""))
                 ;; Get the main instruction from the user
                 (user-instruction (aider-read-string "Edit suggestion request: "
                                                      "Analyze the code context below. Identify potential refactoring opportunities (e.g., complexity, duplication, clarity). Suggest the most impactful refactoring technique and explain why.")) ;; Improved initial-input
                 ;; Add TDD constraint if in TDD mode
                 (tdd-constraint (if tdd-mode " Ensure all tests still pass after refactoring." ""))
                 ;; Construct the prompt using user input and context
         (base-prompt (format "%s Context: %s%s"
                              user-instruction
                              context-info
                              code-snippet))
         (prompt (concat base-prompt tdd-constraint))
         (message-suffix (if tdd-mode " during TDD refactor stage" "")))
    ;; Send the prompt using the /ask command
    (when (aider-current-file-command-and-switch "/ask " prompt)
      ;; Inform the user
      (message "Requesting refactoring suggestion from Aider%s. If you are happy with the suggestion, use 'go ahead' to accept the change"
               message-suffix))))

;;;###autoload
(defun aider-refactor-book-method (&optional tdd-mode)
  "Apply refactoring techniques or request suggestions.
Uses current context (function, class, selected region).
If TDD-MODE is non-nil, adjusts prompts and instructions for the
TDD refactor stage."
  ;; The `interactive` spec needs to handle the optional argument if called directly,
  ;; but here it's primarily called programmatically from aider-tdd-cycle or interactively without args.
  ;; For interactive calls, tdd-mode will be nil.
  (interactive)
  (let* ((context (aider--get-refactoring-context))
         (region-active (plist-get context :region-active))
         ;; Get all refactoring techniques including "Suggest Refactoring Strategy"
         (all-techniques (aider--get-refactoring-techniques region-active))
         (technique-names (mapcar #'car all-techniques))
         (prompt-prefix (if tdd-mode "Select TDD refactoring technique" "Select refactoring technique"))
         (prompt-suffix (if region-active " for selected region: " ": "))
         (prompt (concat prompt-prefix prompt-suffix))
         (selected-technique (completing-read prompt technique-names nil t)))
    ;; Dispatch to appropriate handler based on user selection
    (if (string= selected-technique "Suggest Refactoring Strategy") ;; Corrected string
        (aider--handle-ask-llm-suggestion context tdd-mode)
      (aider--handle-specific-refactoring selected-technique all-techniques context tdd-mode))))

(defun aider--tdd-red-stage (function-name)
  "Handle the Red stage of TDD for FUNCTION-NAME: Write a failing test."
  (let* ((initial-input
          (if function-name
              (format "Write a failing test for function '%s': " function-name)
            "Write a failing test for this feature: "))
         (feature-desc (aider-read-string "Describe the feature to test: " initial-input))
         (tdd-instructions
          (format "%s Follow TDD principles - write only the test now, not the implementation. The test should fail when run because the functionality doesn't exist yet."
                  feature-desc)))
    (aider-current-file-command-and-switch "/architect " tdd-instructions)))

(defun aider--tdd-green-stage (function-name)
  "Handle the Green stage of TDD for FUNCTION-NAME: Make the test pass."
  (let* ((initial-input
          (if function-name
              (format "Implement function '%s' with minimal code to make tests pass: " function-name)
            "Implement the minimal code needed to make the failing test pass: "))
         (implementation-desc (aider-read-string "Implementation instruction: " initial-input))
         (tdd-instructions
          (format "%s Follow TDD principles - implement only the minimal code needed to make the test pass. Don't over-engineer or implement features not required by the test."
                  implementation-desc)))
    (aider-current-file-command-and-switch "/architect " tdd-instructions)))

;;;###autoload
(defun aider-tdd-cycle ()
  "Guide through Test Driven Development cycle (Red-Green-Refactor).
Helps users follow Kent Beck's TDD methodology with AI assistance.
Works with both source code and test files that have been added to aider."
  (interactive)
  (let* ((function-name (which-function))
         (cycle-stage (completing-read
                       "Select TDD stage: "
                       '("1. Red (Write failing test)"
                         "2. Green (Make test pass)"
                         "3. Refactor (Improve code quality)")
                       nil t))
         (stage-num (string-to-number (substring cycle-stage 0 1))))
    (cond
     ;; Red stage - write failing test
     ((= stage-num 1) (aider--tdd-red-stage function-name))
     ;; Green stage - make test pass
     ((= stage-num 2) (aider--tdd-green-stage function-name))
     ;; Refactor stage - call the main refactoring function in TDD mode
     ((= stage-num 3) (aider-refactor-book-method t)))))

(provide 'aider-agile)
;;; aider-agile.el ends here
