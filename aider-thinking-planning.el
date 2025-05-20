;;; aider-software-planning.el --- Software planning features for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides functionality for interactive software planning sessions
;; with Aider, guided by a structured prompting methodology.
;; Given code and prompt from, and credit to this repo: https://github.com/NightTrek/Software-planning-mcp

;;; Code:

(require 'aider-core)

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
  "Start an interactive software planning session with Aider.
Prompts the user for a software development goal and initiates
a structured planning discussion with Aider using the
`aider-software-planning--sequential-thinking-prompt`."
  (interactive)
  (let ((goal (aider-read-string "Enter your software development goal: ")))
    (if (string-empty-p goal)
        (message "Goal cannot be empty. Planning session not started.")
      (progn
        ;; Ensure Aider is running
        (unless (get-buffer (aider-buffer-name))
          (call-interactively #'aider-run-aider))
        (if (get-buffer (aider-buffer-name))
            (let ((initial-message (format "/ask %s\n\nMy goal is: %s"
                                           aider-software-planning--sequential-thinking-prompt
                                           goal)))
              (aider--send-command initial-message t)
              (message "Software planning session started for goal: %s" goal))
          (message "Aider buffer could not be created or found. Planning session not started."))))))

(defconst aider--sequential-thinking-tool-description-prompt
  "A detailed tool for dynamic and reflective problem-solving through thoughts.
This tool helps analyze problems through a flexible thinking process that can adapt and evolve.
Each thought can build on, question, or revise previous insights as understanding deepens.

When to use this tool:
- Breaking down complex problems into steps
- Planning and design with room for revision
- Analysis that might need course correction
- Problems where the full scope might not be clear initially
- Problems that require a multi-step solution
- Tasks that need to maintain context over multiple steps
- Situations where irrelevant information needs to be filtered out

Key features:
- You can adjust total_thoughts up or down as you progress
- You can question or revise previous thoughts
- You can add more thoughts even after reaching what seemed like the end
- You can express uncertainty and explore alternative approaches
- Not every thought needs to build linearly - you can branch or backtrack
- Generates a solution hypothesis
- Verifies the hypothesis based on the Chain of Thought steps
- Repeats the process until satisfied
- Provides a correct answer

Parameters explained:
- thought: Your current thinking step, which can include:
* Regular analytical steps
* Revisions of previous thoughts
* Questions about previous decisions
* Realizations about needing more analysis
* Changes in approach
* Hypothesis generation
* Hypothesis verification
- next_thought_needed: True if you need more thinking, even if at what seemed like the end
- thought_number: Current number in sequence (can go beyond initial total if needed)
- total_thoughts: Current estimate of thoughts needed (can be adjusted up/down)
- is_revision: A boolean indicating if this thought revises previous thinking
- revises_thought: If is_revision is true, which thought number is being reconsidered
- branch_from_thought: If branching, which thought number is the branching point
- branch_id: Identifier for the current branch (if any)
- needs_more_thoughts: If reaching end but realizing more thoughts needed

You should:
1. Start with an initial estimate of needed thoughts, but be ready to adjust
2. Feel free to question or revise previous thoughts
3. Don't hesitate to add more thoughts if needed, even at the \"end\"
4. Express uncertainty when present
5. Mark thoughts that revise previous thinking or branch into new paths
6. Ignore information that is irrelevant to the current step
7. Generate a solution hypothesis when appropriate
8. Verify the hypothesis based on the Chain of Thought steps
9. Repeat the process until satisfied with the solution
10. Provide a single, ideally correct answer as the final output
11. Only set next_thought_needed to false when truly done and a satisfactory answer is reached"
  "The prompt text describing the MCP Sequential Thinking Tool, for guiding Aider.")

;;;###autoload
(defun aider-start-sequential-thinking ()
  "Start an interactive sequential thinking session with Aider.
Prompts the user for a problem or topic and initiates a
structured discussion with Aider using the
`aider--sequential-thinking-tool-description-prompt`."
  (interactive)
  (let ((problem-topic (aider-read-string "Enter your problem/topic for sequential thinking: ")))
    (if (string-empty-p problem-topic)
        (message "Problem/topic cannot be empty. Session not started.")
      (progn
        ;; Ensure Aider is running
        (unless (get-buffer (aider-buffer-name))
          (call-interactively #'aider-run-aider))
        (if (get-buffer (aider-buffer-name))
            (let ((initial-message (format "/ask %s\n\nMy problem/topic is: %s"
                                           aider--sequential-thinking-tool-description-prompt
                                           problem-topic)))
              (aider--send-command initial-message t)
              (message "Sequential thinking session started for: %s" problem-topic))
          (message "Aider buffer could not be created or found. Session not started."))))))

;; add an interactive function aider-thinking-or-planning, given
;; completing-reading of candidates of either Sequential Thinking or
;; Software Planning, call corresponding function

;;;###autoload
(defun aider-thinking-or-planning ()
  "Offer a choice between Sequential Thinking and Software Planning and start the selected session."
  (interactive)
  (let* ((choices '("Sequential Thinking" "Software Planning"))
         (prompt "Choose thinking/planning mode: ")
         (choice (completing-read prompt choices nil t)))
    (cond
     ((string-equal choice "Sequential Thinking")
      (call-interactively #'aider-start-sequential-thinking))
     ((string-equal choice "Software Planning")
      (call-interactively #'aider-start-software-planning))
     (t
      (message "Invalid choice. No session started.")))))

(provide 'aider-thinking-planning)

;;; aider-software-planning.el ends here
