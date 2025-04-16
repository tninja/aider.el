;;; aider-code-read.el --- Code reading enhancement for aider.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This package enhances aider.el with code reading techniques from
;; "Code Reading: The Open Source Perspective" by Diomidis Spinellis,
;; and adds analysis for specific concerns like security, performance,
;; and maintainability.

;;; Code:

(require 'aider-core)
(require 'aider-file)

;;;###autoload
(defun aider-code-read ()
  "Analyze code using various reading techniques.
Provides a selection of different code reading approaches based on context,
including general analysis and specific concerns like security or performance."
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
            ("Analyze Module (Architectural)" . aider--analyze-module)
            ;; --- New Specific Concern Techniques ---
            ("Analyze for Security Issues" . aider--analyze-for-security)
            ("Analyze for Performance Bottlenecks" . aider--analyze-for-performance)
            ("Analyze Code Quality & Maintainability" . aider--analyze-for-maintainability)
            ;; --- End New Techniques ---
            ))
         (technique-names (mapcar #'car reading-techniques))
         (prompt (format "Select reading technique for %s: " context-description))
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-function (cdr (assoc selected-technique reading-techniques))))
    (if technique-function
        ;; Ensure region analysis calls the specific function
        (if (eq technique-function 'aider--analyze-code-unit-region)
            (funcall technique-function)
          ;; Other techniques likely need file context
          (progn
            ;; Add file context unless it's a region-specific analysis
            (unless (or region-active
                        ;; Add other functions here that *don't* need the file added automatically
                        (eq technique-function 'aider--analyze-module) ;; Module analysis adds files itself
                        )
              (aider-add-current-file))
            (funcall technique-function)))
      (message "No valid reading technique selected."))))

;; --- Existing Analysis Functions (Minor change: ensure file context is added by caller) ---

(defun aider--analyze-code-unit ()
  "Analyze current function using bottom-up reading technique.
Assumes the current file has been added to the Aider context."
  (if-let ((function-name (which-function)))
      (let* ((initial-prompt
              (format "In the current file, analyze function '%s' using bottom-up reading approach.
Explain its basic operations, data structures, and control flow." function-name))
             (user-input (aider-read-string "Enter analysis instructions: " initial-prompt)))
        ;; File context is added by aider-code-read
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
    ;; Sends region directly, doesn't add file context automatically
    (aider--send-command (format "/ask %s\n\nCode:\n%s" user-input region-text) t)))

(defun aider--analyze-program-structure ()
  "Analyze code structure using top-down reading technique.
Assumes the current file has been added to the Aider context."
  (let* ((initial-prompt
          "Please analyze the structure of the code in the current file using top-down reading:
1. Identify main components and their relationships
2. Explain the program's architecture (as represented in this file)
3. Document key interfaces between components within this file
4. Highlight important design patterns used
5. Map the control flow between major components in this file")
         (user-input (aider-read-string "Enter structure analysis instructions: " initial-prompt)))
    ;; File context is added by aider-code-read
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider--analyze-dependencies ()
  "Analyze code dependencies following cross-reference technique.
Assumes the current file has been added to the Aider context."
  (if-let ((function-name (which-function)))
      (let* ((initial-prompt
              (format "In the current file, analyze dependencies for function '%s':
1. List all functions it calls (within this file and potentially others if known)
2. List all functions within this file that call it
3. Identify key data dependencies (variables, parameters, return values)
4. Map external library dependencies used by this function
5. Note any global state interactions" function-name))
             (user-input (aider-read-string "Enter dependency analysis instructions: " initial-prompt)))
        ;; File context is added by aider-code-read
        (aider--send-command (format "/ask %s" user-input) t))
    (message "No function at point.")))

(defun aider--identify-patterns ()
  "Identify common patterns in code following pattern recognition approach.
Assumes the current file has been added to the Aider context."
  (let* ((initial-prompt
          "In the current file, please identify and explain:
1. Common design patterns used
2. Algorithmic patterns
3. Error handling patterns
4. Data structure patterns
5. Any anti-patterns or code smells that should be noted")
         (user-input (aider-read-string "Enter pattern analysis instructions: " initial-prompt)))
    ;; File context is added by aider-code-read
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider--analyze-class ()
  "Analyze class definition using OOP analysis technique.
Assumes the current file has been added to the Aider context."
  (if-let ((class-name (aider--get-class-at-point)))
      (let* ((initial-prompt
              (format "In the current file, analyze class '%s' using OOP perspective:
1. Class responsibility and purpose
2. Inheritance hierarchy and relationships (if applicable within the file)
3. Key attributes and their purposes
4. Public interface and method contracts
5. Internal implementation patterns
6. Collaboration with other classes (within the file)
7. State management approach" class-name))
             (user-input (aider-read-string "Enter class analysis instructions: " initial-prompt)))
        ;; File context is added by aider-code-read
        (aider--send-command (format "/ask %s" user-input) t))
    (message "No class definition at point.")))

(defun aider--analyze-file ()
  "Analyze current file using file-level reading technique.
Assumes the current file has been added to the Aider context."
  (let* ((file-name (buffer-file-name))
         (initial-prompt
          (format "Analyze the entire file '%s' from a file-level perspective:
1. File's primary purpose and responsibilities
2. Key abstractions defined in the file (classes, functions, modules)
3. Dependencies and imports analysis
4. File organization and structure
5. Coding conventions used
6. Integration points with other files (if inferrable)
7. Configuration and environment dependencies mentioned
8. Notable algorithms or business logic contained within" (file-name-nondirectory file-name)))
         (user-input (aider-read-string "Enter file analysis instructions: " initial-prompt)))
    ;; File context is added by aider-code-read
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider--analyze-module ()
  "Analyze current directory/module using architectural reading technique."
  (let* ((dir-name (file-name-directory (buffer-file-name)))
         (initial-prompt
          (format "Analyze the module represented by the files in directory '%s' using an architectural perspective (based on the files added):
1. Module's likely role in the overall system
2. Package organization and structure observed
3. Key components and their interactions within the added files
4. External dependencies and interfaces used by these files
5. Internal module architecture patterns observed
6. Configuration management hints
7. Testing strategy hints (e.g., presence of test files)
8. Integration patterns with other modules (if inferrable)
9. Deployment considerations mentioned" (directory-file-name dir-name)))
         (user-input (aider-read-string "Enter module analysis instructions: " initial-prompt)))
    ;; Add all relevant files in the directory first
    (aider-add-same-type-files-under-dir)
    ;; Then send the command
    (aider--send-command (format "/ask %s" user-input) t)))

;; --- New Functions for Specific Concerns ---

(defun aider--analyze-for-security ()
  "Analyze the current file for potential security vulnerabilities.
Assumes the current file has been added to the Aider context."
  (let* ((initial-prompt
          "Please analyze the code in the current file for potential security vulnerabilities:
1. Input Validation: Are inputs properly sanitized and validated? (e.g., against injection, path traversal)
2. Output Encoding: Is output properly encoded for its context? (e.g., HTML, SQL)
3. Authentication/Authorization: Are there checks missing or implemented insecurely?
4. Session Management: Are sessions handled securely?
5. Sensitive Data Exposure: Is sensitive data handled, stored, or logged insecurely?
6. Error Handling: Do error messages reveal too much information?
7. Dependency Security: Are there known vulnerabilities in used libraries (if inferrable)?
8. Concurrency Issues: Are there potential race conditions or deadlocks?")
         (user-input (aider-read-string "Enter security analysis instructions: " initial-prompt)))
    ;; File context is added by aider-code-read
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider--analyze-for-performance ()
  "Analyze the current file for potential performance bottlenecks.
Assumes the current file has been added to the Aider context."
  (let* ((initial-prompt
          "Please analyze the code in the current file for potential performance bottlenecks:
1. Algorithmic Complexity: Are there algorithms with high complexity (e.g., O(n^2)) in critical paths?
2. Loops: Are there inefficient loops (e.g., excessive work inside, redundant computations)?
3. I/O Operations: Is I/O performed inefficiently (e.g., too frequently, large data transfers)?
4. Data Structures: Are appropriate data structures used for the operations performed?
5. Memory Usage: Are there signs of excessive memory allocation or potential memory leaks?
6. Concurrency: Are there potential performance issues related to locking or resource contention?
7. Caching: Are there opportunities for caching results that are missed?
8. Database Interaction: Are database queries potentially inefficient (e.g., N+1 problem)?")
         (user-input (aider-read-string "Enter performance analysis instructions: " initial-prompt)))
    ;; File context is added by aider-code-read
    (aider--send-command (format "/ask %s" user-input) t)))

(defun aider--analyze-for-maintainability ()
  "Analyze the current file for code quality and maintainability issues.
Assumes the current file has been added to the Aider context."
  (let* ((initial-prompt
          "Please analyze the code in the current file for maintainability and code quality:
1. Readability: Is the code clear, well-formatted, and easy to understand? Are variable/function names meaningful?
2. Complexity: Are functions/methods too long or complex (high cyclomatic complexity)? Are classes too large (violating SRP)?
3. Duplication: Is there significant duplicated code (potential for DRY principle violation)?
4. Code Smells: Are there common code smells (e.g., magic numbers, feature envy, inappropriate intimacy)?
5. Comments/Documentation: Is the code adequately commented? Is documentation (e.g., docstrings) present and accurate?
6. Testability: Is the code structured in a way that makes it easy to write unit tests (e.g., low coupling, dependency injection)?
7. Consistency: Is the code style consistent throughout the file?
8. Modularity: Is the code well-modularized with clear responsibilities?")
         (user-input (aider-read-string "Enter maintainability analysis instructions: " initial-prompt)))
    ;; File context is added by aider-code-read
    (aider--send-command (format "/ask %s" user-input) t)))

;; --- Helper Functions (Unchanged) ---

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
        (let ((formatted-files (mapcar #'aider--format-file-path
                                       (mapcar (lambda (f) (aider--get-file-path f)) files)))
              (command nil))
          (when formatted-files
            (setq command (concat "/add " (mapconcat #'identity formatted-files " ")))
            (aider--send-command command t)
            (message "Added %d files with suffix .%s"
                     (length files) current-suffix))
          )))))

(defun aider--get-class-at-point ()
  "Get the class name at point. Support multiple programming languages."
  (save-excursion
    (let ((case-fold-search nil))
      (when (re-search-backward "\\<\\(class\\|interface\\|trait\\|struct\\)\\s-+\\([A-Za-z0-9_]+\\)" nil t)
        (match-string-no-properties 2)))))

(provide 'aider-code-read)

;;; aider-code-read.el ends here
