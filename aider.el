;;; aider.el --- Aider package for interactive conversation with aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (transient "0.3.0") (magit "2.1.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el

;;; Commentary:
;; This package provides an interactive interface to communicate with https://github.com/paul-gauthier/aider.

;;; Code:

(require 'comint)
(require 'dired)
(require 'transient)
(require 'magit)
(require 'which-func)

(require 'aider-core)
(require 'aider-prompt-mode)
(require 'aider-file)
(require 'aider-code-change)

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
  "Class for toggling aider--switch-to-buffer-other-frame.")

(transient-define-infix aider--infix-switch-to-buffer-other-frame ()
  "Toggle aider--switch-to-buffer-other-frame between nil and t."
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

;;;###autoload
(defun aider-run-aider (&optional edit-args)
  "Create a comint-based buffer and run \"aider\" for interactive conversation.
With the universal argument, prompt to edit aider-args before running."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (current-args (if edit-args
                           (split-string
                            (read-string "Edit aider arguments: "
                                         (mapconcat 'identity aider-args " ")))
                         aider-args)))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
      (with-current-buffer buffer-name
        (comint-mode)
        (setq-local comint-input-sender 'aider-input-sender) ;; this will only impact the prompt entered directly inside comint buffer. comint-send-string function won't be affected. so aider--process-message-if-multi-line won't be triggered twice.
        (font-lock-add-keywords nil aider-font-lock-keywords t)))
    (aider-switch-to-buffer)))

(defun aider-input-sender (proc string)
  "Handle multi-line inputs being sent to Aider."
  (comint-simple-send proc (aider--process-message-if-multi-line string)))

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
  "Send the command \"/reset\" to the Aider buffer."
  (interactive "P")
  (if clear
      (aider-clear-buffer)
    (aider--send-command "/reset")
    ))

;;;###autoload
(defun aider-exit ()
  "Send the command \"/exit\" to the Aider buffer."
  (interactive)
  (aider--send-command "/exit"))

;;;###autoload
(defun aider-other-process-command (&optional manual)
  "Send process control commands to aider.
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
      (aider--send-command command t))
    ))


;; Function to send a custom command to corresponding aider buffer
;;;###autoload
(defun aider-general-command ()
  "Prompt the user to input COMMAND and send it to the corresponding aider comint buffer."
  (interactive)
  (let ((command (aider-read-string "Enter command to send to aider: ")))
    ;; Use the shared helper function to send the command
    (aider--send-command command t)))


;; New function to get command from user and send it prefixed with "/ask "
;;;###autoload
(defun aider-ask-question (&optional no-context)
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/ask \".
If a region is active, append the region text to the question.
If cursor is inside a function, include the function name as context."
  (interactive "P")
  ;; Dispatch to general question if in aider buffer
  (if (or no-context
       (string= (buffer-name) (aider-buffer-name)))
    (aider-general-question)
    (let* ((function-name (which-function))
           (prompt (if function-name 
                       (format "About function '%s': " function-name)
                     "Question for the selected region: "
                     ))
           (raw-question (aider-read-string prompt))
           (question (if function-name
                         (concat prompt raw-question)
                       raw-question))
           (region-text (and (region-active-p) 
                             (buffer-substring-no-properties (region-beginning) (region-end))))
           (question-context (if region-text
                                 (format "%s: %s" question region-text)
                               question)))
      (aider-current-file-command-and-switch "/ask " question-context)
      )))

;;;###autoload
(defun aider-general-question ()
  "Prompt the user for a general question and send it to the corresponding aider comint buffer prefixed with \"/ask \"."
  (interactive)
  (let ((question (aider-read-string "Enter general question to ask: ")))
    (let ((command (format "/ask %s" question)))
      (aider--send-command command t))))

;; New function to get command from user and send it prefixed with "/help "
;;;###autoload
(defun aider-help (&optional homepage)
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/help \"."
  (interactive "P")
  (if homepage
      (aider-open-aider-home) 
    (let ((command (aider-read-string "Enter help question: ")))
      (aider-current-file-command-and-switch "/help " command))
      ))

;;;###autoload
(defun aider-open-aider-home ()
  "Open the Aider home page in the default browser."
  (interactive)
  (browse-url "https://aider.chat"))


;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
;;;###autoload
(defun aider-debug-exception ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/debug \",
replacing all newline characters except for the one at the end."
  (interactive)
  (let ((command (aider-read-string "Enter exception, can be multiple lines: ")))
    (aider--send-command (concat "/ask Investigate the following exception, with current added files as context: " command) t)))

;;;###autoload
(defun aider-go-ahead ()
  "Send the command \"go ahead\" to the corresponding aider comint buffer."
  (interactive)
  (aider--send-command "go ahead" t))


;; New function to explain the code in the selected region
;;;###autoload
(defun aider-region-explain ()
  "Get a command from the user and send it to the corresponding aider comint buffer based on the selected region.
The command will be formatted as \"/ask \" followed by the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (processed-region-text region-text)
             (command (if function-name
                          (format "/ask in function %s, explain the following code block: %s"
                                  function-name
                                  processed-region-text)
                        (format "/ask explain the following code block: %s"
                                processed-region-text))))
        (aider-add-current-file)
        (aider--send-command command t))
    (message "No region selected.")))

;; New function to ask Aider to explain the function under the cursor
;;;###autoload
(defun aider-function-explain ()
  "Ask Aider to explain the function under the cursor.
Prompts user for specific questions about the function."
  (interactive)
  (if-let ((function-name (which-function)))
      (let* ((prefix (format "explain %s: " function-name))
             (prompt (format "Enter your question to %s" prefix))
             (user-question (aider-read-string prompt))
             (command (format "/ask %s%s" prefix user-question)))
        (aider-current-file-command-and-switch "/ask " (concat prefix user-question)))
    (message "No function found at cursor position.")))

;;;###autoload
(defun aider-function-or-region-explain ()
  "Call aider-function-explain when no region is selected, otherwise call aider-region-explain."
  (interactive)
  (if (region-active-p)
      (aider-region-explain)
    (aider-function-explain)))

(defun aider-current-file-command-and-switch (prefix command)
  "Send COMMAND to the Aider buffer prefixed with PREFIX."
  (aider-add-current-file)
  (aider--send-command (concat prefix command) t))

;;; Model selection functions
;;;###autoload
(defun aider-change-model (leaderboards)
  "Interactively select and change AI model in current aider session.
With prefix argument (C-u), open the Aider LLM leaderboard in a browser."
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

;; doom
(when (featurep 'doom)
  (require 'aider-doom))

(provide 'aider)

;;; aider.el ends here
