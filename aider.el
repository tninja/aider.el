;;; aider.el --- Interact with Aider: AI pair programming made simple -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0") (magit "2.1.0") (markdown-mode "2.5"))
;; Keywords: agent ai gpt sonnet llm aider gemini-pro deepseek ai-assisted-coding 
;; URL: https://github.com/tninja/aider.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Tired of coding alone? This package + Aider (https://aider.chat/)
;; brings an AI pair programmer *inside* Emacs! Aider works seamlessly
;; with both *new* and *existing* codebases in your local Git repo,
;; using AI models (Claude, ChatGPT, even local ones!) to help you. It
;; can suggest improvements, squash bugs, or even write whole new
;; sections of code. Boost your coding with AI, without ever leaving
;; your Emacs comfort zone.
;;
;; In-editor Aider experience:
;; - Manages Aider sessions per Git repo.
;; - Menu for AI-assisted coding
;;
;; Alternatives to aidermacs:
;; - More Focus on build prompts using your code (buffer/selection).
;; - Reuse prompts easily, fuzzy search with helm.
;; - Organize project with repo specific Aider prompt file
;; - More Focus on code quality tool (Code Review, Agile + AI).
;; - Snippets for community prompts.
;; - Less configurations, simplified menu.

;;; Code:

(require 'comint)
(require 'transient)
(require 'which-func)

(require 'aider-core)
(require 'aider-prompt-mode)
(require 'aider-file)
(require 'aider-code-change)
(require 'aider-discussion)
(require 'aider-agile)
(require 'aider-code-read)

(defcustom aider-popular-models '("sonnet"  ;; really good in practical
                                  "o3-mini" ;; very powerful. good for difficult task
                                  "gemini-exp"  ;; SOTA at 2025-03-25
                                  "deepseek"  ;; low price, pretty good performance
                                  )
  "List of available AI models for selection.
Each model should be in the format expected by the aider command line interface.
Also based on aider LLM benchmark: https://aider.chat/docs/leaderboards/"
  :type '(repeat string)
  :group 'aider)

(defclass aider--switch-to-buffer-type (transient-lisp-variable)
  ((variable :initform 'aider--switch-to-buffer-other-frame)
   (format :initform "%k %d %v")
   (reader :initform #'transient-lisp-variable--read-value))
  "Class for toggling `aider--switch-to-buffer-other-frame`.")

(transient-define-infix aider--infix-switch-to-buffer-other-frame ()
  "Toggle `aider--switch-to-buffer-other-frame` between nil and t."
  :class 'aider--switch-to-buffer-type
  :key "^"
  :description "Open in new frame"
  :reader (lambda (_prompt _initial-input _history)
           (not aider--switch-to-buffer-other-frame)))

;; Transient menu for Aider commands
;; The instruction in the autoload comment is needed, see
;; https://github.com/magit/transient/issues/280.
;;;###autoload (autoload 'aider-transient-menu "aider" "Transient menu for Aider commands." t)
(transient-define-prefix aider-transient-menu ()
  "Transient menu for Aider commands."
  ["Aider: AI Pair Programming"
   ["Aider Process"
    (aider--infix-switch-to-buffer-other-frame)
    ("a" "Run Aider (C-u: args) " aider-run-aider)
    ("z" "Switch to Aider Buffer" aider-switch-to-buffer)
    ("p" "Input with Repo Prompt File" aider-open-prompt-file)
    ("s" "Reset Aider (C-u: clear)" aider-reset)
    ("o" "Select Model (C-u: leadboard)" aider-change-model)
    ("x" "Exit Aider" aider-exit)
    ]
   ["File Operation"
    ("f" "Add Current/Marked File (C-u: readonly)" aider-add-current-file-or-dired-marked-files)
    ("w" "Add All Files in Window" aider-add-files-in-current-window)
    ("O" "Drop Current File" aider-drop-current-file)
    ("m" "Show Last Commit (C-u: magit-log)" aider-magit-show-last-commit-or-log)
    ("u" "Undo Last Change" aider-undo-last-change)
    ("v" "Pull or Review Code Change" aider-pull-or-review-diff-file)
    ("h" "Open History" aider-open-history)
    ]
   ["Code Change"
    ("r" "Change Function/Region" aider-function-or-region-refactor)
    ("i" "Implement Requirement" aider-implement-todo)
    ("t" "Architect Discuss/Change" aider-architect-discussion)
    ("U" "Write Unit Test" aider-write-unit-test)
    ("R" "Refactor Code" aider-refactor-book-method)
    ("T" "Test Driven Development" aider-tdd-cycle)
    ("c" "Direct Code Change" aider-code-change)
    ]
   ["Discussion"
    ("q" "Question on Function/Region" aider-ask-question)
    ("y" "Then Go Ahead" aider-go-ahead)
    ("Q" "Question without Context" aider-general-question)
    ("d" "Code Reading" aider-code-read)
    ("e" "Debug Exception" aider-debug-exception)
    ("H" "Help (C-u: homepage)" aider-help)
    ]
   ])

;; Add a function, aider-clear-buffer. It will switch aider buffer and call comint-clear-buffer
;;;###autoload
(defun aider-clear-buffer ()
  "Switch to the Aider buffer and clear its contents."
  (interactive)
  (when-let ((buffer (get-buffer (aider-buffer-name))))
    (with-current-buffer buffer
      (comint-clear-buffer)
      (aider--send-command "/clear"))
    (aider-switch-to-buffer)))

;; Function to reset the Aider buffer
;;;###autoload
(defun aider-reset (&optional clear)
  "Send the command \"/reset\" to the Aider buffer.
With prefix argument CLEAR, clear the buffer contents instead of just resetting."
  (interactive "P")
  (if clear
      (aider-clear-buffer)
    (aider--send-command "/reset")))

;;;###autoload
(defun aider-exit ()
  "Send the command \"/exit\" to the Aider buffer."
  (interactive)
  (aider--send-command "/exit"))

;;; Model selection functions
;;;###autoload
(defun aider-change-model (leaderboards)
  "Interactively select and change AI model in current aider session.
With prefix argument LEADERBOARDS, open the Aider LLM leaderboard in a browser.
Allows selecting between /model, /editor-model, and /weak-model commands."
  (interactive "P")
  (if leaderboards
      (browse-url "https://aider.chat/docs/leaderboards/")
    (let* ((commands '("/model" "/editor-model" "/weak-model"))
           (command (completing-read "Select model command: " commands nil t))
           (model (completing-read "Select AI model: " aider-popular-models nil t nil nil (car aider-popular-models))))
      (when model
        (aider--send-command (format "%s %s" command model) t)
        (message "%s changed to %s, customize aider-popular-models for the model candidates"
                 (substring command 1) model)))))

(provide 'aider)

;;; aider.el ends here
