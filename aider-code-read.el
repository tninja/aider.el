;;; aider-code-read.el --- Code reading enhancement for aider.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This package enhances aider.el with code reading techniques from
;; "Code Reading: The Open Source Perspective" by Diomidis Spinellis.

;;; Code:

(require 'aider-core)

;;;###autoload
(defun aider-code-read ()
  "Analyze code using various reading techniques.
Provides a selection of different code reading approaches based on context."
  (interactive)
  (let* ((function-name (which-function))
         (class-name (aider--get-class-at-point))
         (region-active (region-active-p))
         (file-name (when buffer-file-name (file-name-nondirectory buffer-file-name)))
         (dir-name (when buffer-file-name (file-name-directory buffer-file-name)))
         (context-description (cond
                              (region-active "selected region")
                              (function-name (format "function '%s'" function-name))
                              (class-name (format "class '%s'" class-name))
                              (file-name (format "file '%s'" file-name))
                              (t "current context")))
         (reading-techniques
          `(("Analyze Code Unit (Bottom-up)" . 
             ,(if region-active 'aider--analyze-code-unit-region 'aider--analyze-code-unit))
            ("Analyze Program Structure (Top-down)" . aider--analyze-program-structure)
            ,@(when function-name 
                '(("Analyze Dependencies (Cross-reference)" . aider--analyze-dependencies)))
            ("Identify Patterns (Pattern Recognition)" . aider--identify-patterns)
            ,@(when class-name
                '(("Analyze Class (OOP Analysis)" . aider--analyze-class)))
            ("Analyze File (File-level)" . aider--analyze-file)
            ("Analyze Module (Architectural)" . aider--analyze-module)))
         (technique-names (mapcar #'car reading-techniques))
         (prompt (format "Select reading technique for %s: " context-description))
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-function (cdr (assoc selected-technique reading-techniques))))
    (if technique-function
        (if (eq technique-function 'aider--analyze-code-unit-region)
            (funcall technique-function)
          (funcall technique-function))
      (message "No valid reading technique selected."))))

(defun aider--analyze-code-unit ()
  "Analyze current function using bottom-up reading technique."
  (if-let ((function-name (which-function)))
      (let* ((initial-prompt 
              (format "Analyze function '%s' using bottom-up reading approach. 
Explain its basic operations, data structures, and control flow." function-name))
             (user-input (aider-read-string "Enter analysis instructions: " initial-prompt)))
        (aider--send-command (format "/ask %s" user-input) t))
    (message "No function at point.")))

(defun aider--analyze-code-unit-region ()
  "Analyze selected region using bottom-up reading technique."
  (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (initial-prompt
          "Analyze this code unit using bottom-up reading:
1. Identify basic operations and control structures
2. Explain data structures used
3. Document function calls and their purposes
4. Summarize the overall logic")
         (user-input (aider-read-string "Enter analysis instructions: " initial-prompt)))
    (aider--send-command (format "/ask %s\n\nCode:\n%s" user-input region-text) t)))

(defun aider--analyze-program-structure ()
  "Analyze code structure using top-down reading technique."
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

(defun aider--analyze-dependencies ()
  "Analyze code dependencies following cross-reference technique."
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

(defun aider--identify-patterns ()
  "Identify common patterns in code following pattern recognition approach."
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

(defun aider--analyze-class ()
  "Analyze class definition using OOP analysis technique."
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

(defun aider--analyze-file ()
  "Analyze current file using file-level reading technique."
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

(defun aider--analyze-module ()
  "Analyze current directory/module using architectural reading technique."
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

(defun aider-add-same-type-files-under-dir ()
  "Add all files with same suffix as current file under current directory to Aider.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files (directory-files dir t
                                   (concat "\\." current-suffix "$")
                                   t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files) max-files current-suffix)
        (let ((command (concat "/add " (mapconcat #'identity files " "))))
          (aider--send-command command t))
        (message "Added %d files with suffix .%s"
                 (length files) current-suffix)))))

(defun aider--get-class-at-point ()
  "Get the class name at point. Support multiple programming languages."
  (save-excursion
    (let ((case-fold-search nil))
      (when (re-search-backward "\\<\\(class\\|interface\\|trait\\)\\s-+\\([A-Za-z0-9_]+\\)" nil t)
        (match-string-no-properties 2)))))

(provide 'aider-code-read)

;;; aider-code-read.el ends here
