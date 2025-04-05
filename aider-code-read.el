;;; aider-code-read.el --- Code reading enhancement for aider.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (aider "0.2.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider-code-read.el

;;; Commentary:
;; This package enhances aider.el with code reading techniques from
;; "Code Reading: The Open Source Perspective" by Diomidis Spinellis.

;;; Code:

(require 'aider)
(require 'transient)

(defun aider-analyze-code-unit ()
  "Analyze current function or region using bottom-up reading technique."
  (interactive)
  (if (region-active-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (initial-prompt
              "Analyze this code unit using bottom-up reading:
1. Identify basic operations and control structures
2. Explain data structures used
3. Document function calls and their purposes
4. Summarize the overall logic")
             (user-input (aider-read-string "Enter analysis instructions: " initial-prompt)))
        (aider--send-command (format "/ask %s\n\nCode:\n%s" user-input region-text) t))
    (if-let ((function-name (which-function)))
        (let* ((initial-prompt 
                (format "Analyze function '%s' using bottom-up reading approach. 
Explain its basic operations, data structures, and control flow." function-name))
               (user-input (aider-read-string "Enter analysis instructions: " initial-prompt)))
          (aider--send-command (format "/ask %s" user-input) t))
      (message "No function or region selected."))))

(defun aider-analyze-program-structure ()
  "Analyze code structure using top-down reading technique."
  (interactive)
  (let* ((initial-prompt 
          "Please analyze this code using top-down reading:
1. Identify main components and their relationships
2. Explain the program's architecture
3. Document key interfaces between components
4. Highlight important design patterns used
5. Map the control flow between major components")
         (user-input (aider-read-string "Enter structure analysis instructions: " initial-prompt)))
    (aider-add-current-file)
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider-analyze-dependencies ()
  "Analyze code dependencies following cross-reference technique."
  (interactive)
  (if-let ((function-name (which-function)))
      (let* ((initial-prompt 
              (format "For function '%s', please:
1. List all functions it calls
2. List all functions that call it
3. Identify key data dependencies
4. Map external library dependencies
5. Note any global state interactions" function-name))
             (user-input (aider-read-string "Enter dependency analysis instructions: " initial-prompt)))
        (aider-add-current-file)
        (aider--send-command (format "/ask %s" user-input) t))
    (message "No function at point.")))

(defun aider-identify-patterns ()
  "Identify common patterns in code following pattern recognition approach."
  (interactive)
  (let* ((initial-prompt
          "Please identify and explain:
1. Common design patterns used
2. Algorithmic patterns
3. Error handling patterns
4. Data structure patterns
5. Any anti-patterns that should be noted")
         (user-input (aider-read-string "Enter pattern analysis instructions: " initial-prompt)))
    (aider-add-current-file)
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider-analyze-class ()
  "Analyze class definition using OOP analysis technique."
  (interactive)
  (if-let ((class-name (aider--get-class-at-point)))
      (let* ((initial-prompt 
              (format "Analyze class '%s' using OOP perspective:
1. Class responsibility and purpose
2. Inheritance hierarchy and relationships
3. Key attributes and their purposes
4. Public interface and method contracts
5. Internal implementation patterns
6. Collaboration with other classes
7. State management approach" class-name))
             (user-input (aider-read-string "Enter class analysis instructions: " initial-prompt)))
        (aider-add-current-file)
        (aider--send-command (format "/ask %s" user-input) t))
    (message "No class definition at point.")))

(defun aider-analyze-file ()
  "Analyze current file using file-level reading technique."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (initial-prompt 
          (format "Analyze file '%s' using file-level perspective:
1. File's primary purpose and responsibilities
2. Key abstractions defined in the file
3. Dependencies and imports analysis
4. File organization and structure
5. Coding conventions used
6. Integration points with other files
7. Configuration and environment dependencies
8. Notable algorithms or business logic" (file-name-nondirectory file-name)))
         (user-input (aider-read-string "Enter file analysis instructions: " initial-prompt)))
    (aider-add-current-file)
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider-analyze-module ()
  "Analyze current directory/module using architectural reading technique."
  (interactive)
  (let* ((dir-name (file-name-directory (buffer-file-name)))
         (initial-prompt 
          (format "Analyze module in directory '%s' using architectural perspective:
1. Module's role in the system
2. Package organization and structure
3. Key components and their interactions
4. External dependencies and interfaces
5. Internal module architecture
6. Configuration management
7. Testing strategy
8. Integration patterns with other modules
9. Deployment considerations" (directory-file-name dir-name)))
         (user-input (aider-read-string "Enter module analysis instructions: " initial-prompt)))
    ;; Add all relevant files in the directory
    (aider-add-same-type-files-under-dir)
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider--get-class-at-point ()
  "Get the class name at point. Support multiple programming languages."
  (save-excursion
    (let ((case-fold-search nil))
      (when (re-search-backward "\\<\\(class\\|interface\\|trait\\)\\s-+\\([A-Za-z0-9_]+\\)" nil t)
        (match-string-no-properties 2)))))

;; Check if "Code Reading" section exists in the menu
(defun aider-code-read--has-section-p ()
  "Check if Code Reading section already exists in aider-transient-menu."
  (let ((layout (get 'aider-transient-menu 'transient--layout)))
    (cl-some (lambda (item)
               (and (listp item)
                    (equal (car item) "Code Reading")))
             layout)))

;; Add the Code Reading section if it doesn't exist
(with-eval-after-load 'aider
  (unless (aider-code-read--has-section-p)
    (transient-append-suffix 'aider-transient-menu '(0)
      '["Code Reading"
        ("R" "Analyze Code Unit" aider-analyze-code-unit)
        ("S" "Analyze Program Structure" aider-analyze-program-structure)
        ("D" "Dependency Analysis for Function" aider-analyze-dependencies)
        ("P" "Pattern Recognition" aider-identify-patterns)
        ("C" "Analyze Class" aider-analyze-class)
        ("F" "Analyze File" aider-analyze-file)
        ("M" "Analyze Module" aider-analyze-module)])))

(provide 'aider-code-read)

;;; aider-code-read.el ends here
