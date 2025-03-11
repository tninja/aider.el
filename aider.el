;;; aider.el --- Interact with Aider: AI-powered pair programming, made simple. -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0") (magit "2.1.0") (helm "3.0") (markdown-mode "2.5")
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This package provides an interactive interface
;; to communicate with https://github.com/paul-gauthier/aider.

;;; Code:

(require 'comint)
(require 'transient)
(require 'which-func)

(require 'aider-core)
(require 'aider-prompt-mode)
(require 'aider-file)
(require 'aider-code-change)
(require 'aider-discussion)

(defcustom aider-popular-models '("sonnet"  ;; really good in practical
                                  "o3-mini" ;; very powerful. good for difficult task
                                  "deepseek"  ;; low price, pretty good performance
                                  "gemini-2.0-flash-exp"  ;; free
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
    ("p" "Repo Prompt File" aider-open-prompt-file)
    ("o" "Select Model (C-u: leadboard)" aider-change-model)
    ("s" "Reset Aider (C-u: clear)" aider-reset)
    ("l" "Other Command (C-u: manual)" aider-other-process-command)
    ]
   ["File Operation"
    ("f" "Add Current / Marked File (C-u: readonly)" aider-add-current-file-or-dired-marked-files)
    ("w" "Add All Files in Window" aider-add-files-in-current-window)
    ("d" "Add Same Type Files in dir" aider-add-same-type-files-under-dir)
    ("O" "Drop Current File" aider-drop-current-file)
    ("m" "Last Commit (C-u: magit-log)" aider-magit-show-last-commit)
    ("u" "Undo Last Change" aider-undo-last-change)
    ]
   ["Code Change"
    ("t" "Architect Discuss / Change" aider-architect-discussion)
    ("c" "Direct Code Change" aider-code-change)
    ("r" "Refactor Function / Region" aider-function-or-region-refactor)
    ("i" "Implement Requirement" aider-implement-todo)
    ("U" "Write Unit Test" aider-write-unit-test)
    ("T" "Fix Failing Test" aider-fix-failing-test-under-cursor)
    ]
   ["Discussion"
    ("q" "Ask Question (C-u: no context)" aider-ask-question)
    ("y" "Then Go Ahead" aider-go-ahead)
    ("e" "Explain Function / Region" aider-function-or-region-explain)
    ("D" "Debug Exception" aider-debug-exception)
    ("h" "Help (C-u: homepage)" aider-help)
    ("x" "Exit Aider" aider-exit)
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

;;;###autoload
(defun aider-other-process-command (&optional manual)
  "Send process control commands to aider.
With prefix argument MANUAL, manually enter the command
Prompts user to select from a list of available commands:
- /clear: Clear the chat history
- /copy: Copy the last chat message
- /drop: Drop all files
- /ls: List tracked files
- /lint: Run linter on tracked files
- /map: Show file map
- /map-refresh: Refresh file map
- /paste: Paste the last copied chat message
- /settings: Show current settings
- /tokens: Show token usage"
  (interactive "P")
  (if manual
      (aider-general-command)
    (let* ((commands '("/clear" "/copy" "/drop" "/ls" "/lint" "/map"
                       "/map-refresh" "/paste" "/settings" "/tokens"))
           (command (completing-read "Select command: " commands nil t)))
      (aider--send-command command t))))

;; Function to send a custom command to corresponding aider buffer
;;;###autoload
(defun aider-general-command ()
  "Input COMMAND and send it to the corresponding aider comint buffer."
  (interactive)
  (let ((command (aider-read-string "Enter command to send to aider: ")))
    ;; Use the shared helper function to send the command
    (aider--send-command command t)))

;;; Model selection functions
;;;###autoload
(defun aider-change-model (leaderboards)
  "Interactively select and change AI model in current aider session.
With prefix argument LEADERBOARDS, open the Aider LLM leaderboard in a browser."
  (interactive "P")
  (if leaderboards
      (browse-url "https://aider.chat/docs/leaderboards/")
    (let ((model (aider--select-model)))
      (when model
        (aider--send-command (format "/model %s" model) t)
        (message "Model changed to %s, customize aider-popular-models for the model candidates" model)))))

(defun aider--select-model ()
  "Private function for model selection with completion."
  (completing-read "Select AI model: " aider-popular-models nil t nil nil (car aider-popular-models)))

;; now you should explicitly require the modules you need

;; ;; doom
;; (when (featurep 'doom)
;;   (require 'aider-doom))

(provide 'aider)

;;; aider.el ends here
