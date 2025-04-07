;;; aider-legacy.el --- Legacy code handling for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides legacy code handling techniques based on Michael Feathers'
;; "Working Effectively with Legacy Code" for the Aider package.

;;; Code:

(require 'aider-core)
(require 'aider-file)
(require 'which-func)

;;;###autoload
(defun aider-legacy ()
  "Apply legacy code techniques from 'Working Effectively with Legacy Code'.
Provides a selection of different techniques for working with legacy code
based on the current context."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "selected region")
                              (function-name (format "function '%s'" function-name))
                              (t "current context")))
         (legacy-techniques
          `(("Identify Seams" . aider--legacy-identify-seams)
            ("Generate Characterization Tests" . aider--legacy-characterization-test)
            ("Break Dependencies" . aider--legacy-break-dependencies)
            ("Sprout Method" . aider--legacy-sprout-method)
            ("Wrap Method" . aider--legacy-wrap-method)
            ("Sprout Class" . aider--legacy-sprout-class)
            ("Wrap Class" . aider--legacy-wrap-class)
            ("Sensing Variable" . aider--legacy-sensing-variable)
            ("Extract and Override Call" . aider--legacy-extract-and-override-call)
            ("Extract and Override Getter" . aider--legacy-extract-and-override-getter)
            ("Replace Function with Function Pointer" . aider--legacy-replace-function-with-function-pointer)
            ("Adapt Parameter" . aider--legacy-adapt-parameter)
            ("Introduce Instance Delegator" . aider--legacy-introduce-instance-delegator)
            ("Analyze Change Points" . aider--legacy-analyze-change-points)))
         (technique-names (mapcar #'car legacy-techniques))
         (prompt (format "Select legacy code technique for %s: " context-description))
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-function (cdr (assoc selected-technique legacy-techniques))))
    (if technique-function
        (funcall technique-function)
      (message "No valid legacy code technique selected."))))

(defun aider--legacy-identify-seams ()
  "Identify seams in the current code where behavior can be changed without editing.
A seam is a place where you can alter behavior in your program without editing in that place.
This function helps identify potential seams in legacy code to make it more testable."
  (interactive)
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "in the selected code")
                              (function-name (format "in function '%s'" function-name))
                              (t "in the current file")))
         (initial-prompt (format "Identify seams %s. Look for places where behavior can be changed without editing the code directly. Consider: preprocessing seams, link seams, object seams." context-description))
         (user-prompt (aider-read-string "Seam identification instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Analyze this code to identify seams as defined in 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-characterization-test ()
  "Generate characterization tests for the current code.
Characterization tests document existing behavior without making judgments about correctness.
This is essential for safely refactoring legacy code."
  (let* ((function-name (which-function))
         (region-active (region-active-p))
         (region-text (when region-active
                        (buffer-substring-no-properties (region-beginning) (region-end))))
         (context-description (cond
                              (region-active "for the selected code")
                              (function-name (format "for function '%s'" function-name))
                              (t "for the current file")))
         (initial-prompt (format "Write characterization tests %s. These tests should document the existing behavior without making judgments about correctness. Focus on capturing all current behaviors, including edge cases and error conditions." context-description))
         (user-prompt (aider-read-string "Characterization test instruction: " initial-prompt))
         (command (if region-active
                     (format "/architect \"Generate characterization tests for this code as described in 'Working Effectively with Legacy Code': %s\n\n```\n%s\n```\""
                             user-prompt region-text)
                   (format "/architect \"%s\"" user-prompt))))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-break-dependencies ()
  "Apply techniques to break dependencies in legacy code.
Offers various dependency-breaking techniques from 'Working Effectively with Legacy Code'
to make the code more testable."
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

(defun aider--legacy-sprout-method ()
  "Apply the Sprout Method technique to add new functionality.
Creates a new method for the new functionality to minimize changes to existing code."
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

(defun aider--legacy-wrap-method ()
  "Apply the Wrap Method technique to modify existing behavior.
Wraps an existing method to add behavior before/after it without changing the original."
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

(defun aider--legacy-sprout-class ()
  "Apply the Sprout Class technique to add new functionality.
Creates a new class for the new functionality to minimize changes to existing code."
  (let* ((class-name (aider-read-string "Name for the new class: "))
         (new-functionality (aider-read-string "Describe the functionality for the new class: "))
         (initial-prompt (format "Apply the Sprout Class technique to create a new class named '%s' that implements this functionality: %s. Design the class to work with the existing code with minimal changes to the original code."
                               class-name
                               new-functionality))
         (user-prompt (aider-read-string "Sprout Class instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-wrap-class ()
  "Apply the Wrap Class technique to modify existing behavior.
Creates a wrapper class that delegates to the original class while adding new behavior."
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

(defun aider--legacy-sensing-variable ()
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

(defun aider--legacy-extract-and-override-call ()
  "Apply the Extract and Override Call technique to replace problematic method calls.
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
         (initial-prompt (format "Apply the Extract and Override Call technique %s to extract the call to '%s' into a new method named '%s'. This will allow the call to be overridden in a testing subclass."
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

(defun aider--legacy-extract-and-override-getter ()
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

(defun aider--legacy-replace-function-with-function-pointer ()
  "Apply the Replace Function with Function Pointer technique.
Useful in C/C++ code to make static functions testable by replacing them with function pointers."
  (let* ((function-name (which-function))
         (function-to-replace (or function-name
                                 (aider-read-string "Function to replace with pointer: ")))
         (initial-prompt (format "Apply the Replace Function with Function Pointer technique to make '%s' testable. Replace the direct function with a function pointer that can be changed during tests."
                               function-to-replace))
         (user-prompt (aider-read-string "Replace Function with Function Pointer instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--legacy-adapt-parameter ()
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

(defun aider--legacy-introduce-instance-delegator ()
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

(defun aider--legacy-analyze-change-points ()
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

(provide 'aider-legacy)
;;; aider-legacy.el ends here
