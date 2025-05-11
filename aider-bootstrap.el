;;; aider-bootstrap.el --- Code bootstrapping utilities for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides functions to bootstrap common code / doc structures using Aider.

;;; Code:

(require 's)

(require 'aider-core)
(require 'aider-file) ; For aider-add-current-file

;;;###autoload
(defun aider-bootstrap ()
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
            ("Contextual Note Generation" . aider--bootstrap-contextual-note)
            ("General Plan Outline" . aider--bootstrap-general-plan)
            ("Org-mode Slides Outline" . aider--bootstrap-org-slides)))
         (technique-names (mapcar #'car bootstrap-techniques))
         (prompt "Select bootstrapping technique: ")
         (selected-technique (completing-read prompt technique-names nil t))
         (technique-function (cdr (assoc selected-technique bootstrap-techniques))))
    (if technique-function
        (funcall technique-function)
      (message "No valid bootstrapping technique selected."))))

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
    (aider--send-command command t)))

(defun aider--bootstrap-contextual-note ()
  "Generate or update a note based on the current Aider context and user queries."
  (interactive)
  (let* ((action-choice (completing-read "Action: " '("Create new note" "Update existing note") nil t nil "Create new note"))
         (is-update (string-equal action-choice "Update existing note"))
         (note-filename
          (if is-update
              (read-file-name "Enter filename of the note to update: " nil nil t)
            (read-file-name "Enter filename for the new note (e.g., notes.md, report.org): " nil nil t "context_notes.md")))
         (context-questions (aider-read-string "Key questions/topics for the note (regarding current context): ")))
    (when (s-blank? note-filename)
      (message "Note filename cannot be empty.")
      (error "Filename required"))
    (when (s-blank? context-questions)
      (message "Context questions cannot be empty.")
      (error "Context questions required"))
    (let* ((prompt-intro
            (if is-update
                (format "Please update the content of the file '%s'. This file should have just been added to our context. The update should focus on incorporating new insights or refining existing content based on the current overall context (other added files) and the following points:"
                        (file-name-nondirectory note-filename))
              (format "Generate content for a new note file named '%s'. This note should synthesize information from the current context (files already added to our discussion) to address the following:"
                      (file-name-nondirectory note-filename))))
           (is-org-file (string-suffix-p ".org" note-filename :ignore-case))
           (prompt-questions (format "\nKey Questions/Topics:\n%s" context-questions))
           (base-update-instructions
            (concat "\nWhen incorporating new information or elaborating on existing points based on the key questions/topics, "
                    "please use a structured outlining approach: use clear headings for main topics and sub-headings/bullet points for details. "
                    "Ensure the overall updated note remains coherent and well-organized, integrating this new perspective with the existing content of the file."))
           (base-create-instructions
            (concat "\nOrganize the information as a detailed hierarchical outline. "
                    "Use clear headings for main topics derived from the key questions/topics you were given. "
                    "Under each heading, use sub-headings and bullet points to present supporting details, synthesized information from the context, and answers to the specific aspects of the questions. "
                    "Ensure the output is suitable for direct inclusion in the specified file."))
           (org-specific-instructions
            (if is-org-file
                (concat " Since the target file is an Org-mode file, please use Org-mode syntax for headings (e.g., `* Main Topic`, `** Sub-topic`), lists (e.g., `- item`), and consider using checkboxes (e.g., `- [ ] Actionable item`) for actionable points if appropriate, to structure the content effectively.")
              ""))
           (final-prompt (concat prompt-intro prompt-questions
                                 (if is-update
                                     (concat base-update-instructions org-specific-instructions)
                                   (concat base-create-instructions org-specific-instructions))))
           (command (format "/architect \"%s\"" final-prompt)))
      (if is-update
          (progn
            (unless (file-exists-p note-filename)
              (error "Note file '%s' does not exist. Cannot update." note-filename))
            ;; Add the existing note file to Aider's context
            (aider--send-command (format "/add %s" (shell-quote-argument note-filename)) t)))
      ;; Send the main architect command
      (aider--send-command command t)
      (message "Contextual note generation/update request sent to Aider for '%s'." (file-name-nondirectory note-filename)))))

(defun aider--bootstrap-general-plan ()
  "Generate a general plan outline for various purposes."
  (interactive)
  (let* ((objective (aider-read-string "What is the main objective or goal of this plan? (e.g., 'Develop new login feature', 'Plan family weekend trip'): "))
         (timeframe (aider-read-string "What is the timeframe for this plan? (e.g., 'Next 2 weeks', 'This weekend', 'Q3'): "))
         (key-people (aider-read-string "Who are the key people involved or who is this plan for? (e.g., 'Dev team', 'Myself', 'The family'): "))
         (main-areas (aider-read-string "What are the key areas or main tasks to consider? (comma-separated, e.g., 'Backend, Frontend, Testing' or 'Activities, Logistics, Budget'): "))
         (constraints (aider-read-string "Any specific constraints or important considerations? (e.g., 'Limited budget', 'Existing codebase dependencies', 'Kid-friendly options'): "))
         (filename (read-file-name "Save plan outline as: " nil nil t "plan_outline.txt"))
         (initial-prompt (format "Generate a plan outline for the objective: '%s'.
    This plan is intended for '%s' and aims to be accomplished within '%s'.
    The plan should be saved in a file named '%s'.
    Key areas or main tasks to cover:
    %s
    Specific constraints or important considerations:
    %s
    Please structure the output as a clear and actionable plan outline. This might include:
    - A suitable title for the plan.
    - Sections for each key area or main task identified.
    - Within each section, list potential sub-tasks, action items, milestones, or points to consider.
    - If applicable, suggest a logical flow or dependencies between tasks.
    - Incorporate the specified constraints and considerations throughout the plan.
    The goal is to create a practical starting point that can be further detailed by the user."
                                   objective
                                   key-people
                                   timeframe
                                   (file-name-nondirectory filename)
                                   main-areas
                                   constraints))
         (user-prompt (aider-read-string "General Plan Outline instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider--send-command command t)))

(defun aider--bootstrap-org-slides ()
  "Generate an Org-mode slide outline for a presentation."
  (interactive)
  (let* ((title (aider-read-string "What is the title of your presentation?: "))
         (author (aider-read-string "Who is the author? (Leave blank if not needed): "))
         (goal (aider-read-string "Primary goal/objective? (e.g., inform, persuade, train): "))
         (key-takeaways (aider-read-string "Key takeaways for the audience? (2-3 points): "))
         (main-sections (aider-read-string "Main sections/topics? (comma-separated, e.g., Intro, Topic A, Conclusion): "))
         (audience (aider-read-string "(Optional) Target audience?: "))
         (style (aider-read-string "Desired style/tone? (e.g., formal, technical, engaging): "))
         (duration (aider-read-string "(Optional) Estimated duration? (e.g., 20 mins, 1 hour): "))
         (call-to-action (aider-read-string "(Optional) Specific call to action at the end?: "))
         (num-slides-approx (aider-read-string "(Optional) Approx. number of slides?: "))
         (filename (read-file-name "Save Org slides as: " nil nil t (concat (s-replace " " "_" (downcase title)) ".org")))
         (initial-prompt
          (format "Generate an Org-mode slide outline for a presentation titled '%s'%s.
    The presentation should be saved in a file named '%s'.
    Primary Goal/Objective: %s.
    Key Takeaways for Audience: %s.
    Main sections/topics to cover: %s.
    Desired Style/Tone: %s.
    %s%s%s%s
    Please structure the output as a valid Org-mode file suitable for export to both reveal.js and LaTeX Beamer. Include:
    1. Standard Org-mode metadata at the top (e.g., #+TITLE:, #+AUTHOR:, #+DATE:, #+OPTIONS: toc:nil num:nil).
       Also include common setup lines for both reveal.js and Beamer export backends. For example:
       For reveal.js: `#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js`, `#+REVEAL_THEME: sky` (or other themes like league, black, white), `#+REVEAL_SLIDE_NUMBER: c/t`.
       For Beamer: `#+LATEX_CLASS: beamer`, `#+LATEX_CLASS_OPTIONS: [presentation]`, `#+BEAMER_THEME: Madrid` (or other themes like AnnArbor, Warsaw), `#+BEAMER_COLORTHEME: default`.
       These options can be included with comments or guidance for the user on selecting the backend.
    2. Top-level headlines (e.g., `* Section Title`) for each main section, reflecting the presentation's goal and key takeaways.
    3. Second-level headlines (e.g., `** Slide Title`) for individual slides within each section. This structure should be compatible with both export backends.
    4. For each slide, provide 2-3 bullet points or a brief placeholder sentence suggesting content. This content should align with the specified style/tone.
    5. Include an introductory slide (e.g., Title Slide, Agenda/Objective Slide) and a concluding slide (e.g., Summary of Key Takeaways, Call to Action, Q&A).
    The content placeholders should be guided by the goal, key takeaways, and desired tone.
    The goal is to create a well-structured Org-mode file that serves as a detailed starting point for a presentation, compatible with both reveal.js and Beamer export."
                  title
                  (if (s-blank? author) "" (format " by '%s'" author))
                  (file-name-nondirectory filename)
                  goal
                  key-takeaways
                  main-sections
                  style
                  (if (s-blank? audience) "" (format "Target audience: %s.\n    " audience))
                  (if (s-blank? duration) "" (format "Estimated duration: %s.\n    " duration))
                  (if (s-blank? call-to-action) "" (format "Call to Action: %s.\n    " call-to-action))
                  (if (s-blank? num-slides-approx) "" (format "Aim for approximately %s slides.\n    " num-slides-approx))))
         (user-prompt (aider-read-string "Org-mode Slides Outline instruction: " initial-prompt))
         (command (format "/architect \"%s\"" user-prompt)))
    (aider--send-command command t)))

(provide 'aider-bootstrap)

;;; aider-bootstrap.el ends here
