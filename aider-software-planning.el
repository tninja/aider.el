;;; aider-software-planning.el --- Software Planning features for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides functionality for interactive software planning sessions
;; with Aider, guided by a structured prompting methodology.
;;
;; Software Planning is a mcp server designed to facilitate software development planning through an interactive, structured approach. It helps break down complex software projects into manageable tasks.
;;
;; Given code and prompt from, and credit to this mcp servers:
;; https://github.com/NightTrek/Software-planning-mcp

;;; Code:

(require 'which-func)
(require 'subr-x)

(require 'aider-core)
(require 'aider-file)

(defconst aider-software-planning--sequential-thinking-prompt
  "You are a senior software architect guiding the development of a software feature through a question-based sequential thinking process. Your role is to:

1. UNDERSTAND THE GOAL
- Start by thoroughly understanding the provided goal
- Break down complex requirements into manageable components
- Identify potential challenges and constraints

2. ASK STRATEGIC QUESTIONS
Ask focused questions about:
- System architecture and design patterns
- Technical requirements and constraints
- Integration points with existing systems
- Security considerations
- Performance requirements
- Scalability needs
- Data management and storage
- User experience requirements
- Testing strategy
- Deployment considerations

3. ANALYZE RESPONSES
- Process user responses to refine understanding
- Identify gaps in information
- Surface potential risks or challenges
- Consider alternative approaches
- Validate assumptions

4. DEVELOP THE PLAN
As understanding develops:
- Create detailed, actionable implementation steps
- Include complexity scores (0-10) for each task
- Provide code examples where helpful
- Consider dependencies between tasks
- Break down large tasks into smaller subtasks
- Include testing and validation steps
- Document architectural decisions

5. ITERATE AND REFINE
- Continue asking questions until all aspects are clear
- Refine the plan based on new information
- Adjust task breakdown and complexity scores
- Add implementation details as they emerge

6. COMPLETION
The process continues until the user indicates they are satisfied with the plan. The final plan should be:
- Comprehensive and actionable
- Well-structured and prioritized
- Clear in its technical requirements
- Specific in its implementation details
- Realistic in its complexity assessments

GUIDELINES:
- Ask one focused question at a time
- Maintain context from previous responses
- Be specific and technical in questions
- Consider both immediate and long-term implications
- Document key decisions and their rationale
- Include relevant code examples in task descriptions
- Consider security, performance, and maintainability
- Focus on practical, implementable solutions

Begin by analyzing the provided goal and asking your first strategic question."
  "The prompt text for guiding the software planning session with Aider.")

;;;###autoload
(defun aider-start-software-planning ()
  "Start an interactive software planning session with Aider, Giving context."
  (interactive)
  (let* ((file (buffer-file-name))
         (file-name (and file (file-name-nondirectory file)))
         (function (which-function))
         (region-active (region-active-p))
         (region-text (and region-active
                           (buffer-substring-no-properties (region-beginning) (region-end))))
         ;; Compose default goal from context
         (prompt "Enter / select your software development goal: ")
         (scope
          (if region-active
              'region
            (let* ((candidates
                    (append
                     (when function   (list "function"))
                     (when file-name (list "file"))
                     (list "repo")))
                   (choice
                    (completing-read
                     "Select planning scope: "
                     candidates nil t nil nil (car candidates))))
            (intern choice))))
         ;; provide planning-oriented choices for the user
         (candidate-list ;; choose questions based on selected scope
          (pcase scope
            ('region
             '("How should we refactor the selected code for clarity?"
               "How can we extend the selected code to add a new feature?"
               "What are the best ways to optimize the selected code for performance?"
               "How should we add error handling to the selected code?"
               "What tests should we write for the selected code?"
               "How should we document the selected code?"))
            ('function
             '("How should we refactor this function for clarity?"
               "How can we extend this function to handle more cases?"
               "What are the best ways to optimize this function for performance?"
               "How should we add error handling to this function?"
               "What tests should we write for this function?"
               "How should we document this function?"))
            ('file
             '("How should we review this file for code quality improvements?"
               "What missing tests should we identify in this file?"
               "How can we refactor this file for better structure?"
               "How should we extend this file with a new feature?"
               "What are the best ways to optimize this file for performance?"
               "How should we add documentation to this file?"))
            ('repo
             '("What are the top areas for improvement in this repository?"
               "What would be the most useful new feature given the repository's current state?"
               "What code quality issues should we address and how?"
               "How should we decompose modules for a large upcoming feature?"
               "What performance and scaling requirements should we define for this codebase?"
               "How should we design the API interfaces for the current code?"
               "What are the key integration points with external services or libraries?"
               "What security considerations and potential vulnerabilities should we evaluate?"
               "What testing strategy (unit, integration, E2E) should we implement?"
               "What documentation needs should we address and how?"
               "How can we enhance the CI/CD and deployment pipeline?"))))
         (goal (aider-read-string prompt nil candidate-list))
         ;; Collect context information
         (context ;; build context based on selected scope
                  (pcase scope
                    ('region
                     (format "Context:\n- Selected region in function: %s\n- Region content: %s\n- Current file: %s"
                             function region-text file-name))
                    ('function
                     (format "Context:\n- Current function: %s\n- Current file: %s"
                             function file-name))
                    ('file
                     (format "Context:\n- Current file: %s" file-name))
                    ('repo
                     ;; List added files when at repository scope
                     (let ((added (aider-core--parse-added-file-list)))
                       (if added
                           (format "Context:\n- Added files: %s"
                                   (mapconcat 'identity added ", "))
                         "Context: (no added files)"))))))
    (if (string-empty-p goal)
        (message "Goal cannot be empty. Planning session not started.")
      (when (aider--validate-aider-buffer)
        ;; if context present, add current file to the session
        (when (memq scope '(region function file)) ;; only add current file for region/function/file
          (aider-add-current-file))
        (when (aider--send-command
               (format "/ask %s\n\n%s\n\nMy goal is: %s"
                       aider-software-planning--sequential-thinking-prompt
                       context
                       goal) t)
          (message "Software planning session started for goal: %s" goal))))))

(provide 'aider-software-planning)

;;; aider-software-planning.el ends here
