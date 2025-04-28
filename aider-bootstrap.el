;;; aider-bootstrap.el --- Code bootstrapping utilities for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides functions to bootstrap common code structures using Aider.

;;; Code:

(require 'aider-core)
(require 'aider-file) ; For aider-add-current-file

;; Add this new function definition here
(defun aider--get-language-from-extension (filename)
  "Return a programming language name based on FILENAME's extension."
  (let ((extension (file-name-extension filename)))
    (cond
     ((null extension) "Unknown") ; No extension
     ((string-equal extension "py") "Python")
     ((string-equal extension "js") "JavaScript")
     ((string-equal extension "ts") "TypeScript")
     ((string-equal extension "java") "Java")
     ((string-equal extension "go") "Go")
     ((string-equal extension "rb") "Ruby")
     ((string-equal extension "rs") "Rust")
     ((string-equal extension "cpp") "C++")
     ((string-equal extension "c") "C")
     ((string-equal extension "h") "C") ; Also C/C++ header
     ((string-equal extension "hpp") "C++") ; Also C++ header
     ((string-equal extension "cs") "C#")
     ((string-equal extension "php") "PHP")
     ((string-equal extension "swift") "Swift")
     ((string-equal extension "kt") "Kotlin")
     ((string-equal extension "scala") "Scala")
     ((string-equal extension "lisp") "Lisp")
     ((string-equal extension "clj") "Clojure")
     ((string-equal extension "hs") "Haskell")
     ((string-equal extension "pl") "Perl")
     ((string-equal extension "sh") "Shell")
     ((string-equal extension "bash") "Shell")
     ((string-equal extension "md") "Markdown")
     ((string-equal extension "org") "Org Mode")
     ((string-equal extension "txt") "Text")
     ((string-equal extension "sql") "SQL")
     (t "Unknown")))) ; Default for unknown extensions

;;;###autoload
(defun aider-bootstrap-code ()
  "Bootstrap common code structures using Aider.
Provides a selection of language-agnostic bootstrapping prompts."
  (interactive)
  (let* ((bootstrap-techniques
          '(("Basic File Structure" . aider--bootstrap-basic-file)
            ("Class/Module Outline" . aider--bootstrap-class-module-outline)
            ("Basic CLI Application" . aider--bootstrap-cli-app)
            ("README Template" . aider--bootstrap-readme)
            ("Project Structure" . aider--bootstrap-project-structure)
            ("Database Model/Schema" . aider--bootstrap-data-model)
            ("Docker Configuration" . aider--bootstrap-docker-config)
            ("Web Customer Data Analysis Report" . aider--bootstrap-org-data-analysis-report)))
         (technique-names (mapcar #'car bootstrap-techniques))
         (prompt "Select bootstrapping technique: ")
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-function (cdr (assoc selected-technique bootstrap-techniques))))
    (if technique-function
        (funcall technique-function)
      (message "No valid bootstrapping technique selected."))))

;; --- Internal Helper Functions for Bootstrapping ---

(defun aider--bootstrap-basic-file ()
  "Generate a basic file structure for a new file."
  (interactive)
  (let* ((file-purpose (aider-read-string "Describe the main purpose of this file: "))
         ;; Ask for full filename including extension
         (filename (read-file-name "Enter filename (including extension): " nil nil t))
         ;; Derive language from filename
         (language (aider--get-language-from-extension filename))
         ;; Generate prompt *before* switching buffer
         (initial-prompt (format "Generate a basic file structure in %s for a file named '%s' intended for '%s'. Include standard header comments (if applicable for %s), common imports (if predictable), and a basic entry point (like a main function or initial setup)."
                               language (file-name-nondirectory filename) file-purpose language))
         (user-prompt (aider-read-string "Basic File Structure instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--bootstrap-class-module-outline ()
  "Generate an outline for a class or module in a new file."
  (interactive)
  (let* ((name (aider-read-string "Name for the class/module: "))
         (purpose (aider-read-string "Briefly describe its purpose: "))
         ;; Ask for full filename including extension
         (filename (read-file-name "Enter filename (including extension): " nil nil t))
         ;; Derive language from filename
         (language (aider--get-language-from-extension filename))
         ;; Generate prompt *before* switching buffer
         (initial-prompt (format "Generate a basic outline in %s for a class or module named '%s' in file '%s'. Its purpose is: '%s'. Include placeholders for initialization (constructor), key public methods, potential private helper methods, and essential attributes. Add docstrings/comments explaining each part."
                               language name (file-name-nondirectory filename) purpose))
         (user-prompt (aider-read-string "Class/Module Outline instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--bootstrap-cli-app ()
  "Generate a basic structure for a command-line application in a new file."
  (interactive)
  (let* ((app-name (aider-read-string "Name of the CLI application: "))
         (purpose (aider-read-string "Briefly describe its main function: "))
         ;; Ask for full filename including extension
         (filename (read-file-name "Enter filename (including extension): " nil nil t))
         ;; Derive language from filename
         (language (aider--get-language-from-extension filename))
         ;; Generate prompt *before* switching buffer
         (initial-prompt (format "Generate a basic structure in %s for a command-line application named '%s' in file '%s'. Purpose: '%s'. Include argument parsing (using a common library for %s, e.g., argparse in Python, getopt in C, commander in Node.js), a main execution function, basic help message handling, and placeholder logic for the core task."
                               language app-name (file-name-nondirectory filename) purpose language))
         (user-prompt (aider-read-string "CLI Application instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--bootstrap-readme ()
  "Generate a template for a README file."
  (interactive)
  (let* ((project-name (aider-read-string "Project Name: " (file-name-nondirectory default-directory)))
         (purpose (aider-read-string "Briefly describe the project's purpose: "))
         ;; Keep suggestion for standard filename
         (suggested-filename "README.md")
         (filename (read-file-name "Save file as: " nil nil t suggested-filename))
         ;; Generate prompt *before* switching buffer
         (initial-prompt (format "Generate a standard README.md template for a project named '%s'. Purpose: '%s'. Include sections for: Project Title, Description, Installation, Usage, Contributing, License, and Contact/Support Info. Use Markdown formatting."
                               project-name purpose))
         (user-prompt (aider-read-string "README Template instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--bootstrap-project-structure ()
  "Generate a complete project structure."
  (interactive)
  (let* ((project-type (completing-read "Project type: "
                                       '("Web Application" "CLI Tool" "Library/Package"
                                         "API Service" "Data Processing" "Desktop Application")
                                       nil t))
         (project-name (aider-read-string "Project name: " (file-name-nondirectory default-directory)))
         ;; Keep language prompt here as it's about the overall project, not a single file
         (language (aider-read-string "Primary language: " "Python")) ; Example default
         (initial-prompt (format "Generate a complete project structure for a %s named '%s' using %s. Include:
1. Directory structure with explanations
2. Key files (source code, tests, configuration)
3. Build/dependency management files
4. Documentation files
5. Common project files (.gitignore, etc.)
For each file, provide a brief description of its purpose and basic content."
                               project-type project-name language))
         (user-prompt (aider-read-string "Project Structure instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider--send-command command t)))

(defun aider--bootstrap-data-model ()
  "Generate database model/schema code in a new file."
  (interactive)
  (let* ((db-type (completing-read "Database type: "
                                  '("PostgreSQL" "MySQL" "SQLite" "MongoDB" "Oracle" "SQL Server" "DynamoDB")
                                  nil t "PostgreSQL"))
         (entity-name (aider-read-string "Entity name (e.g., 'User', 'Product'): "))
         (fields-str (aider-read-string "Fields (e.g., 'id:int, name:string, created_at:datetime'): "))
         ;; Ask for full filename including extension
         (filename (read-file-name "Enter filename (including extension): " nil nil t))
         ;; Derive language from filename
         (language (aider--get-language-from-extension filename))
         ;; Generate prompt *before* switching buffer
         (initial-prompt (format "Generate a database model in %s for '%s' with these fields: %s in file '%s' for %s database. Include:
1. Complete model/schema definition
2. Field types and constraints
3. Relationships (if applicable)
4. Indexes (if applicable)
5. Comments explaining the structure"
                               language entity-name fields-str (file-name-nondirectory filename) db-type))
         (user-prompt (aider-read-string "Data Model instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--bootstrap-docker-config ()
  "Generate Docker configuration files."
  (interactive)
  (let* (;; Keep this language prompt as it informs content, not filename derivation
         (language (aider-read-string "Primary language for application content: " "Python")) ; Example default
         (config-type (completing-read "Configuration type: "
                                      '("Dockerfile" "Docker Compose" "Both")
                                      nil t "Both"))
         (services (when (or (string= config-type "Docker Compose") (string= config-type "Both"))
                     (aider-read-string "Services (comma-separated, e.g., 'app, db, cache'): ")))
         (multi-stage (when (or (string= config-type "Dockerfile") (string= config-type "Both"))
                        (y-or-n-p "Use multi-stage build? ")))
         (multi-stage-text (if multi-stage "Use a multi-stage build approach to minimize image size. " ""))
         ;; Keep filename prompt with suggestions based on type
         (suggested-filename (cond
                              ((string= config-type "Dockerfile") "Dockerfile")
                              ((string= config-type "Docker Compose") "docker-compose.yml")
                              (t "docker-setup.txt"))) ; For "Both" or other cases
         (filename (read-file-name "Save file as: " nil nil t suggested-filename))
         ;; Generate prompt *before* switching buffer - uses the explicitly prompted 'language'
         (initial-prompt
          (cond
           ((string= config-type "Dockerfile")
            (format "Generate a Dockerfile for a %s application. %sInclude:
1. Appropriate base image selection
2. Proper dependency installation
3. Security best practices
4. Clear comments explaining each step
5. Optimizations for build speed and image size" language multi-stage-text))
           ((string= config-type "Docker Compose")
            (format "Generate a Docker Compose configuration for a %s application with these services: %s. Include:
1. Service definitions with appropriate images/builds
2. Network configuration
3. Volume mounts
4. Environment variables
5. Clear comments explaining each service and configuration option" language services))
           ((string= config-type "Both")
            (format "Generate both a Dockerfile and Docker Compose configuration for a %s application. %sFor Docker Compose, include these services: %s. Include:
1. Well-structured Dockerfile with best practices
2. Complete Docker Compose setup with service definitions
3. Network and volume configuration
4. Environment variables
5. Clear comments throughout" language multi-stage-text services))))
         (user-prompt (aider-read-string "Docker Configuration instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(defun aider--bootstrap-org-data-analysis-report ()
  "Generate an Org-mode template for a data analysis report focused on web company customer data."
  (interactive)
  (let* ((report-title (aider-read-string "Report title: " "Web Customer Data Analysis Report"))
         (data-source-type (completing-read "Primary data source: " 
                                           '("PostgreSQL Database" 
                                             "Web Analytics" 
                                             "CRM Data"
                                             "Customer Events Log"
                                             "Mixed Sources") 
                                           nil t "PostgreSQL Database"))
         (analysis-focus (completing-read "Analysis focus: " 
                                         '("Customer Segmentation" 
                                           "Conversion Funnel Analysis" 
                                           "Retention Analysis" 
                                           "Behavioral Patterns" 
                                           "Churn Prediction"
                                           "Customer Lifetime Value"
                                           "A/B Test Results"
                                           "Feature Usage Analysis") 
                                         nil t "Customer Segmentation"))
         (time-period (aider-read-string "Analysis time period: " "Last 3 months"))
         (analysis-tools (completing-read-multiple "Select analysis tools (space-separated): " 
                                                 '("PostgreSQL" "Python" "R" "Pandas" "Scikit-learn")
                                                 nil t))
         (include-dashboards (y-or-n-p "Include dashboard recommendations? "))
         (include-ml (y-or-n-p "Include machine learning models? "))
         (tools-text (if analysis-tools
                         (format "Include org-babel code blocks for %s with appropriate examples for each tool:
- PostgreSQL: data extraction, customer metrics, and cohort analysis
- Python: data manipulation with pandas, visualization with matplotlib/seaborn
- R: statistical analysis with tidyverse, modeling, and ggplot2 visualizations
- Machine learning: %s examples for predictive modeling
"
                                 (string-join analysis-tools ", ")
                                 (if include-ml "scikit-learn/caret" ""))
                       ""))
         (dashboard-text (if include-dashboards
                             "Include a section on dashboard recommendations with metrics to track and visualization suggestions. "
                           ""))
         ;; Keep suggestion for standard filename extension
         (suggested-filename (concat (downcase (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" report-title)) ".org"))
         (filename (read-file-name "Save report as: " nil nil t suggested-filename))
         ;; Generate prompt *before* switching buffer
         (initial-prompt (format "Generate a comprehensive Org-mode template for a web company customer data analysis report titled '%s'. The report analyzes '%s' data focusing on %s over %s. %s%sThe template should include:
1. A properly structured Org document with title, author, date, and table of contents
2. Standard sections for a web analytics/customer data report:
   - Executive Summary
   - Business Context and Objectives
   - Data Sources and Methodology
   - Customer Segmentation/Profiling
   - Key Metrics and KPIs
   - Detailed Analysis Findings
   - Business Recommendations
   - Technical Appendix (queries, code)
3. Proper use of Org-mode features:
   - Headings with appropriate levels
   - Properties and tags for filtering
   - TODO items for analysis steps
   - Tables for key metrics
   - Export options for HTML/PDF/presentations
Include placeholder text that guides the user on what to write in each section, with specific focus on actionable business insights from customer data."
                               report-title data-source-type analysis-focus time-period 
                               tools-text dashboard-text))
         (user-prompt (aider-read-string "Web Customer Data Analysis Report instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    ;; Remove old check for .org file
    ;; Create/find file, add it, then send command
    (find-file filename)
    (aider-add-current-file)
    (aider--send-command command t)))

(provide 'aider-bootstrap)

;;; aider-bootstrap.el ends here
