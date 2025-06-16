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
         (default-goal
          (cond
           (region-active
            (format "For selected region%s: %s: "
                    (if function (format " in function '%s'" function) "") region-text))
           (function (format "For function '%s': " function))
           (file-name (format "For file '%s': " file-name))
           (t "Plan new software feature")))
         (prompt (format "Enter your software development goal (Backspace to clear): " default-goal))
         ;; provide planning-oriented choices for the user
         (candidate-list
          '("Review this repository and identify top areas for improvement"
            "Suggest the most useful new feature given the repository's current state"
            "Perform a code quality assessment and propose concrete refactorings"
            "Outline a module decomposition for a large upcoming feature"
            "Define performance and scaling requirements for this codebase"
            "Discuss API design and interfaces for the current code"
            "Identify key integration points with external services or libraries"
            "Evaluate security considerations and potential vulnerabilities"
            "Plan a comprehensive testing strategy (unit, integration, E2E)"
            "Assess documentation needs and propose improvements"
            "Outline CI/CD and deployment pipeline enhancements"))
         (goal (aider-read-string prompt nil candidate-list)))
    (if (string-empty-p goal)
        (message "Goal cannot be empty. Planning session not started.")
      (when (aider--validate-aider-buffer)
        ;; if context present, add current file to the session
        (when (or region-active function file-name)
          (aider-add-current-file))
        (when (aider--send-command
               (format "/ask %s\n\nMy goal is: %s"
                       aider-software-planning--sequential-thinking-prompt
                       goal) t)
          (message "Software planning session started for goal: %s" goal))))))

(provide 'aider-software-planning)

;;; aider-software-planning.el ends here
