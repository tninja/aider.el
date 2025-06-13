;;; aider-legacy-code.el --- Legacy code handling for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides legacy code handling techniques based on Michael Feathers'
;; "Working Effectively with Legacy Code" for the Aider package.

;;; Code:

(require 'which-func)

(require 'aider-core)
(require 'aider-file)

;;;###autoload
(defun aider-legacy-code ()
  "Apply legacy code techniques from \"Working Effectively with Legacy Code\".
Provides a selection of different techniques for working with legacy code
based on the current context."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (context-description (cond
                              (region-active "selected region")
                              (function-name (format "function '%s'" function-name))
                              (t "current context")))
         (legacy-techniques
          `(("Identify Seams" . aider--legacy-code-identify-seams)
            ("Generate Characterization Tests" . aider--legacy-code-characterization-test)
            ("Break Dependencies" . aider--legacy-code-break-dependencies)
            ("Sprout Method" . aider--legacy-code-sprout-method)
            ("Wrap Method" . aider--legacy-code-wrap-method)
            ("Sprout Class" . aider--legacy-code-sprout-class)
            ("Wrap Class" . aider--legacy-code-wrap-class)
            ("Sensing Variable" . aider--legacy-code-sensing-variable)
            ("Extract and Override Call" . aider--legacy-code-extract-and-override-call)
            ("Extract and Override Getter" . aider--legacy-code-extract-and-override-getter)
            ("Extract and Override Setter" . aider--legacy-code-extract-and-override-setter) ; New
            ("Extract and Override Factory Method" . aider--legacy-code-extract-override-factory-method) ; New
            ("Introduce Static Setter" . aider--legacy-code-introduce-static-setter) ; New
            ("Replace Function with Function Pointer" . aider--legacy-code-replace-function-with-function-pointer)
            ("Adapt Parameter" . aider--legacy-code-adapt-parameter)
            ("Introduce Instance Delegator" . aider--legacy-code-introduce-instance-delegator)
            ("Encapsulate Global References" . aider--legacy-code-encapsulate-global-references) ; New
            ("Introduce Null Object Pattern" . aider--legacy-code-introduce-null-object) ; New
            ("Replace Conditional with Polymorphism" . aider--legacy-code-replace-conditional-polymorphism) ; New
            ("Analyze Change Points" . aider--legacy-code-analyze-change-points)))
         (technique-names (mapcar #'car legacy-techniques))
         (prompt (format "Select legacy code technique for %s: " context-description))
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-function (cdr (assoc selected-technique legacy-techniques))))
    (if technique-function
        (funcall technique-function)
      (message "No valid legacy code technique selected."))))

(defun aider--legacy-code-identify-seams ()
  "Identify seams in the current code where behavior can be changed.
A seam is a place where you can alter behavior in your program
without editing in that place. This function helps identify
potential seams in legacy code to make it more testable."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (initial-prompt (format (concat "Identify seams %s. Look for places where "
                                         "behavior can be changed without editing "
                                         "the code directly. Consider: preprocessing "
                                         "seams, link seams, object seams.")
                                 context-description))
         (user-prompt (aider-read-string "Seam identification instruction: " initial-prompt))
         (command (if region-active
                     (format "/ask \"Analyze this code to identify seams as defined in 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/ask \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-characterization-test ()
  "Generate characterization test for the current code.
Characterization tests document existing behavior without making
judgments about correctness. This is essential for safely
refactoring legacy code."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "for the selected code")
                              (function-name (format "for function '%s'" function-name))
                              (t "for the current file")))
         (initial-prompt (format (concat "Write characterization tests %s. These "
                                         "tests should document the existing "
                                         "behavior without making judgments about "
                                         "correctness. Focus on capturing all "
                                         "current behaviors, including edge cases "
                                         "and error conditions.")
                                 context-description))
         (user-prompt (aider-read-string "Characterization test instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Generate characterization tests for this code as described in 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-break-dependencies ()
  "Apply techniques to break dependencies in legacy code.
Offers various dependency-breaking techniques from
\"Working Effectively with Legacy Code\" to make the code more
testable."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (techniques '(("Extract Interface" . "Create an interface from a class to separate usage from implementation")
                      ("Parameterize Constructor" . "Pass dependencies into a class rather than creating them internally")
                      ("Parameterize Method" . "Pass dependencies into a method rather than accessing them globally")
                      ("Subclass and Override Method" . "Create a testing subclass that overrides problematic methods")
                      ("Extract Method" . "Move code to a separate method to make it easier to override or mock")
                      ("Adapt Parameter" . "Use an adapter to convert between incompatible interfaces")
                      ("Break Out Method Object" . "Move complex method to its own class to isolate dependencies")
                      ("Replace Global Reference with Getter" . "Add a getter method for global variables to allow overriding")))
         (technique-names (mapcar #'car techniques))
         (selected-technique (completing-read "Select dependency-breaking technique: " technique-names nil t))
         (technique-description (cdr (assoc selected-technique techniques)))
         (initial-prompt (format "Apply the '%s' technique %s. %s."
                               selected-technique
                               context-description
                               technique-description))
         (user-prompt (aider-read-string "Dependency breaking instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply this dependency-breaking technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-sprout-method ()
  "Apply the Sprout Method technique to add new functionality.
Creates a new method for the new functionality to minimize
changes to existing code."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "to the selected code")
                              (function-name (format "to function '%s'" function-name))
                              (t "to the current file")))
         (new-functionality (aider-read-string "Describe the new functionality to add: "))
         (method-name (aider-read-string "Name for the new method: "))
         (initial-prompt (format "Apply the Sprout Method technique %s to add this functionality: %s. Create a new method named '%s' that implements this functionality with minimal changes to existing code."
                               context-description
                               new-functionality
                               method-name))
         (user-prompt (aider-read-string "Sprout Method instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Sprout Method technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-wrap-method ()
  "Apply the Wrap Method technique to modify existing behavior.
Wraps an existing method to add behavior before/after it without
changing the original."
  (let* ((function-name (which-function))
         (method-to-wrap (or function-name
                            (aider-read-string "Method to wrap: ")))
         (wrap-type (completing-read "Wrap type: " 
                                    '("Before (add behavior before method)" 
                                      "After (add behavior after method)" 
                                      "Around (add behavior before and after method)")
                                    nil t))
         (new-behavior (aider-read-string "Describe the new behavior to add: "))
         (initial-prompt (format "Apply the Wrap Method technique to '%s'. %s: %s. Preserve the original method's behavior while adding the new functionality."
                               method-to-wrap
                               wrap-type
                               new-behavior))
         (user-prompt (aider-read-string "Wrap Method instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-sprout-class ()
  "Apply the Sprout Class technique to add new functionality.
Creates a new class for the new functionality to minimize changes
to existing code."
  (let* ((class-name (aider-read-string "Name for the new class: "))
         (new-functionality (aider-read-string "Describe the functionality for the new class: "))
         (initial-prompt (format "Apply the Sprout Class technique to create a new class named '%s' that implements this functionality: %s. Design the class to work with the existing code with minimal changes to the original code."
                               class-name
                               new-functionality))
         (user-prompt (aider-read-string "Sprout Class instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-wrap-class ()
  "Apply the Wrap Class technique to modify existing behavior.
Creates a wrapper class that delegates to the original class
while adding new behavior."
  (let* ((original-class (aider-read-string "Original class to wrap: "))
         (wrapper-class-name (aider-read-string "Name for the wrapper class: "))
         (new-behavior (aider-read-string "Describe the new behavior to add: "))
         (initial-prompt (format "Apply the Wrap Class technique to create a wrapper class named '%s' for the original class '%s'. The wrapper should add this behavior: %s. Use delegation to preserve the original class's behavior while adding the new functionality."
                               wrapper-class-name
                               original-class
                               new-behavior))
         (user-prompt (aider-read-string "Wrap Class instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-sensing-variable ()
  "Apply the Sensing Variable technique to expose internal state for testing.
Adds mechanisms to expose variables that would otherwise be hidden."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (variable-name (aider-read-string "Variable to expose for testing: "))
         (initial-prompt (format "Apply the Sensing Variable technique to expose the variable '%s' %s for testing purposes. Add the minimal code needed to make this internal state visible during tests without changing the normal behavior of the code."
                               variable-name
                               context-description))
         (user-prompt (aider-read-string "Sensing Variable instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Sensing Variable technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-extract-and-override-call ()
  "Apply the Extract and Override Call technique to replace problematic method call.
Extracts method calls to make them overridable for testing."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (call-to-extract (aider-read-string "Method call to extract and override: "))
         (new-method-name (aider-read-string "Name for the extracted method: "))
         (initial-prompt (format (concat "Apply the Extract and Override Call "
                                         "technique %s to extract the call to '%s' "
                                         "into a new method named '%s'. This will "
                                         "allow the call to be overridden in a "
                                         "testing subclass.")
                               context-description
                               call-to-extract
                               new-method-name))
         (user-prompt (aider-read-string "Extract and Override Call instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Extract and Override Call technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-extract-and-override-getter ()
  "Apply the Extract and Override Getter technique for testing.
Extracts field access into getter methods to allow overriding in tests."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (field-name (aider-read-string "Field to extract getter for: "))
         (getter-name (aider-read-string (format "Name for the getter method (default: get%s): " 
                                               (capitalize field-name))
                                       (format "get%s" (capitalize field-name))))
         (initial-prompt (format "Apply the Extract and Override Getter technique %s to extract access to the field '%s' into a getter method named '%s'. Replace all direct field access with calls to this getter method."
                               context-description
                               field-name
                               getter-name))
         (user-prompt (aider-read-string "Extract and Override Getter instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Extract and Override Getter technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-replace-function-with-function-pointer ()
  "Apply the Replace Function with Function Pointer technique.
Useful in C/C++ code to make static functions testable by
replacing them with function pointers."
  (let* ((function-name (which-function))
         (function-to-replace (or function-name
                                 (aider-read-string "Function to replace with pointer: ")))
         (initial-prompt (format "Apply the Replace Function with Function Pointer technique to make '%s' testable. Replace the direct function with a function pointer that can be changed during tests."
                               function-to-replace))
         (user-prompt (aider-read-string "Replace Function with Function Pointer instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-adapt-parameter ()
  "Apply the Adapt Parameter technique to handle dependencies.
Creates an adapter for a parameter to make the code more testable."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (parameter-name (aider-read-string "Parameter to adapt: "))
         (adapter-name (aider-read-string "Name for the adapter: "))
         (initial-prompt (format "Apply the Adapt Parameter technique %s to create an adapter named '%s' for the parameter '%s'. This will allow the dependency to be replaced during testing."
                               context-description
                               adapter-name
                               parameter-name))
         (user-prompt (aider-read-string "Adapt Parameter instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Adapt Parameter technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-introduce-instance-delegator ()
  "Apply the Introduce Instance Delegator technique.
Converts static method calls to instance method calls for better testability."
  (let* ((function-name (which-function))
         (static-method (or function-name
                           (aider-read-string "Static method to convert: ")))
         (class-name (aider-read-string "Class containing the static method: "))
         (initial-prompt (format "Apply the Introduce Instance Delegator technique to convert the static method '%s' in class '%s' to an instance method. This will make the code more testable by allowing the method to be overridden in subclasses."
                               static-method
                               class-name))
         (user-prompt (aider-read-string "Introduce Instance Delegator instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-analyze-change-points ()
  "Analyze change points in legacy code to identify safe modification areas.
Helps identify where changes can be made with minimal risk."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (feature-description (aider-read-string "Describe the feature or change you want to implement: "))
         (initial-prompt (format "Analyze change points %s to safely implement this feature: %s. Identify areas where changes can be made with minimal risk, considering: 1) Where are the natural seams? 2) Which areas have tests? 3) Which dependencies need to be broken? 4) What refactoring techniques would be most appropriate?"
                               context-description
                               feature-description))
         (user-prompt (aider-read-string "Change point analysis instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Analyze change points as described in 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-extract-and-override-setter ()
  "Apply the Extract and Override Setter technique for testing.
Extracts field modification into setter methods to allow overriding in tests."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (field-name (aider-read-string "Field to extract setter for: "))
         (setter-name (aider-read-string (format "Name for the setter method (default: set%s): "
                                               (capitalize field-name))
                                       (format "set%s" (capitalize field-name))))
         (initial-prompt (format "Apply the Extract and Override Setter technique %s to extract modification of the field '%s' into a setter method named '%s'. Replace all direct field modifications with calls to this setter method. This allows overriding the setter in tests."
                               context-description
                               field-name
                               setter-name))
         (user-prompt (aider-read-string "Extract and Override Setter instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Extract and Override Setter technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-extract-override-factory-method ()
  "Apply the Extract and Override Factory Method technique.
Extracts object creation logic into a separate method (factory method)
that can be overridden in tests to provide mock or fake objects."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (creation-call (aider-read-string "Object creation call to extract (e.g., new MyClass()): "))
         (factory-method-name (aider-read-string "Name for the new factory method: " "createMyClass")) ; Suggest a default
         (initial-prompt (format "Apply the Extract and Override Factory Method technique %s. Extract the creation logic '%s' into a new protected/virtual method named '%s'. Replace the original creation call with a call to this new factory method. This allows subclasses to override the factory method for testing."
                               context-description
                               creation-call
                               factory-method-name))
         (user-prompt (aider-read-string "Extract/Override Factory Method instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Extract/Override Factory Method technique from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-introduce-static-setter ()
  "Apply the Introduce Static Setter technique for testing.
Adds a static setter method to allow tests to replace static dependencies."
  (interactive)
  (let* ((static-variable (aider-read-string "Static variable or dependency to control: "))
         (class-name (aider-read-string "Class containing the static member: "))
         (setter-name (aider-read-string (format "Name for the static setter method (e.g., set%sForTesting): " (capitalize static-variable))
                                       (format "set%sForTesting" (capitalize static-variable))))
         (initial-prompt (format "Apply the Introduce Static Setter technique to class '%s' for the static member '%s'. Add a static setter method named '%s' (potentially with test-only visibility if possible in the language) that allows replacing or setting the static dependency during tests."
                               class-name
                               static-variable
                               setter-name))
         (user-prompt (aider-read-string "Introduce Static Setter instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-encapsulate-global-references ()
  "Apply the Encapsulate Global References technique.
Groups related global variables or external dependencies into a
single class or structure."
  (interactive)
  (let* ((globals-list (aider-read-string "List the global variables/references to encapsulate (comma-separated): "))
         (encapsulating-class (aider-read-string "Name for the new encapsulating class/structure: "))
         (initial-prompt (format "Apply the Encapsulate Global References technique. Create a new class/structure named '%s' to encapsulate the following global references: %s. Replace direct access to these globals with access through an instance of the new class/structure. Provide a way to inject or replace this instance for testing."
                               encapsulating-class
                               globals-list))
         (user-prompt (aider-read-string "Encapsulate Global References instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-introduce-null-object ()
  "Apply the Introduce Null Object pattern.
Replaces conditional checks for null/nil with a Null Object that
provides default do-nothing behavior."
  (interactive)
  (let* ((class-name (aider-read-string "Class for which to introduce a Null Object: "))
         (null-object-class-name (aider-read-string "Name for the Null Object class: " (format "Null%s" class-name)))
         (initial-prompt (format "Apply the Introduce Null Object pattern for the class '%s'. Create a Null Object class named '%s' that implements the same interface as '%s' but provides default (e.g., do-nothing) behavior. Modify the code to use an instance of '%s' instead of null/nil checks where appropriate."
                               class-name
                               null-object-class-name
                               class-name
                               null-object-class-name))
         (user-prompt (aider-read-string "Introduce Null Object instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-code-replace-conditional-polymorphism ()
  "Apply the Replace Conditional with Polymorphism refactoring.
Replaces complex conditional logic (e.g., switch statements,
if/else chains based on type) with polymorphic method calls."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (conditional-description (aider-read-string "Describe the conditional logic to replace (e.g., 'switch on type code', 'if/else checking status'): "))
         (initial-prompt (format (concat "Apply the Replace Conditional with "
                                         "Polymorphism refactoring %s. Identify "
                                         "the conditional logic related to '%s'. "
                                         "Introduce a class hierarchy or use "
                                         "existing polymorphism to replace the "
                                         "conditional checks with polymorphic "
                                         "method calls. Define the necessary "
                                         "interface/base class and subclasses.")
                               context-description
                               conditional-description))
         (user-prompt (aider-read-string "Replace Conditional/Polymorphism instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Apply the Replace Conditional with Polymorphism refactoring from 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))


(provide 'aider-legacy-code)
;;; aider-legacy-code.el ends here
