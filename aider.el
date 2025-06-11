;;; aider.el --- AI assisted programming in Emacs with Aider  -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.11.1
;; Package-Requires: ((emacs "26.1") (transient "0.9.0") (magit "2.1.0") (markdown-mode "2.5") (s "1.13.0"))
;; Keywords: ai gpt sonnet llm aider gemini-pro deepseek ai-assisted-coding
;; URL: https://github.com/tninja/aider.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Boost your programming efficiency! This package + Aider (https://aider.chat/)
;; brings AI-assisted programming capabilities *inside* Emacs! Aider works seamlessly
;; with both *new* and *existing* codebases in your local Git repo,
;; using AI models (Claude, ChatGPT, Gemini, even local ones!) to assist you. It
;; can suggest improvements, squash bugs, or even write whole new
;; sections of code. Enhance your coding with AI without ever leaving
;; your Emacs comfort zone. The package also supports AI-assisted Agile
;; development workflows and AI-assisted code reading to help you understand
;; complex codebases faster and more thoroughly.
;;
;; To use aider.el, you need to install the Aider command line tool: https://aider.chat/#getting-started
;; After that, configure it with (use sonnet as example):
;;
;; (use-package aider
;;   :config
;;   ;; For latest claude sonnet model
;;   (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
;;   (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))
;;
;; For more details, see https://github.com/tninja/aider.el
;;
;; Comparing to its forked peer (aidermacs), Aider.el has brought in lots of application-level features and tools to enhance daily programming. These include:
;;   - AI-assisted agile development methodologies (like TDD, refactoring and legacy code handling based on established software engineering books)
;;   - Diff extraction and AI code review tools
;;   - Advanced code / module reading assistant
;;   - Project software planning discussion capabilities
;;   - Let aider to fix the errors reported by flycheck
;;   - Code / repo evolution analysis with git blame and git log
;;   - Utilities for bootstrapping new files and projects.
;;   - Organize project with repo specific Aider prompt file
;;   - Snippets from community and aider use experience and pattern
;; Besides of that, aider.el focus on simplicity. It has much less configurations (transparent to aider config), simplified menu.

;;; Code:

(require 'comint)
(require 'transient)
(require 'which-func)

(require 'aider-core)
(require 'aider-prompt-mode)
(require 'aider-file)
(require 'aider-git)
(require 'aider-code-change)
(require 'aider-discussion)
(require 'aider-agile)
(require 'aider-code-read)
(require 'aider-legacy-code)
(require 'aider-bootstrap)
(require 'aider-software-planning)


(defcustom aider-popular-models '("sonnet"  ;; really good in practical
                                  "gemini"  ;; SOTA
                                  "o4-mini" ;; good for difficult task
                                  "deepseek/deepseek-reasoner" ;; DeepSeek R1 (0528), low price, pretty good performance
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

;; Define each menu section as a reusable variable
;;; Transient menu items for the “Aider Process” section.
(transient-define-group aider--menu-aider-process
  (aider--infix-switch-to-buffer-other-frame)
  ("a" "Run Aider (C-u: args)"           aider-run-aider)
  ("z" "Switch to Aider Buffer"          aider-switch-to-buffer)
  ("p" "Input with Repo Prompt File"     aider-open-prompt-file)
  ("s" "Reset Aider (C-u: clear)"        aider-reset)
  ("o" "Select Model (C-u: benchmark)"   aider-change-model)
  ("x" "Exit Aider"                      aider-exit))

;;; Transient menu items for the “File Operation” section.
(transient-define-group aider--menu-file-operation
  ("f" "Add Current/Marked File (C-u: readonly)"  aider-add-current-file-or-dired-marked-files)
  ("w" "Add All Files in Window"                  aider-add-files-in-current-window)
  ("M" "Add Module w/o grep (C-u: readonly)"      aider-add-module)
  ("O" "Drop File in Buffer / under Cursor"       aider-drop-current-file)
  ("m" "Show Last Commit (C-u: All Commits)"      aider-magit-show-last-commit-or-log)
  ("u" "Undo Last Change"                         aider-undo-last-change)
  ("v" "Pull or Review Code Change"               aider-pull-or-review-diff-file)
  ("e" "File Evolution Analysis (C-u: Repo)"      aider-magit-blame-or-log-analyze))

;;; Transient menu items for the “Code Change” section.
(transient-define-group aider--menu-code-change
  ("r" "Change Function/Region"      aider-function-or-region-refactor)
  ("i" "Implement Requirement"       aider-implement-todo)
  ("t" "Architect Discuss/Change"    aider-architect-discussion)
  ("U" "Write Unit Test"             aider-write-unit-test)
  ("F" "Fix Flycheck Errors (C-u: file)"          aider-flycheck-fix-errors-in-scope)
  ("R" "Refactor Code"               aider-refactor-book-method)
  ("T" "Test Driven Development"     aider-tdd-cycle)
  ("l" "Work with Legacy Code"       aider-legacy-code)
  ("B" "Code/Doc Bootstrap"          aider-bootstrap))

;;; Transient menu items for the “Discussion” section.
(transient-define-group aider--menu-discussion
  ("q" "Question (C-u no context)"  aider-ask-question)
  ("y" "Then Go Ahead"              aider-go-ahead)
  ("d" "Code Reading"               aider-code-read)
  ("c" "Copy To Clipboard"          aider-copy-to-clipboard)
  ("P" "Software Planning"          aider-start-software-planning)
  ("E" "Debug Exception"            aider-debug-exception)
  ("h" "Open History"               aider-open-history)
  ("?" "Help (C-u: homepage)"       aider-help))

;; The instruction in the autoload comment is needed, see
;; https://github.com/magit/transient/issues/280.
;;;###autoload (autoload 'aider-transient-menu "aider" "Transient menu for Aider commands." t)
(transient-define-prefix aider-transient-menu ()
  "Transient menu for Aider commands."
  ["Aider: AI Pair Programming"
   ["Aider Process"   aider--menu-aider-process]
   ["File Operation"  aider--menu-file-operation]
   ["Code Change"     aider--menu-code-change]
   ["Discussion"      aider--menu-discussion]])

(transient-define-prefix aider-transient-menu-2cols ()
  "Transient menu for Aider commands."
  ["Aider: AI Pair Programming"
   ["Aider Process"   aider--menu-aider-process]
   ;; adjust space to align
   [""  ""]
   ["File Operation"  aider--menu-file-operation]]
  [["Code Change" aider--menu-code-change]
   ;; adjust space to align
   ;; ["     "  ""]
   ["Discussion" aider--menu-discussion]])

(transient-define-prefix aider-transient-menu-1col ()
  "Transient menu for Aider commands."
  ["Aider: AI Pair Programming"
   ["Aider Process"
    aider--menu-aider-process
    ""
    "File Operation"
    aider--menu-file-operation
    ""
    "Code Change"
    aider--menu-code-change
    ""
    "Discussion"
    aider--menu-discussion]])

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
(defun aider--maybe-prompt-and-set-reasoning-effort (command model)
  "Conditionally prompt for and set reasoning effort for certain models.
This is typically done when COMMAND is \"/model\" and MODEL matches
specific patterns (e.g., OpenAI's o4, o3, o1 series)."
  (when (and (string= command "/model")
             (string-match-p "^\\(o4\\|o3\\|o1\\)" model))
    (let* ((efforts '("low" "medium" "high"))
           (effort (completing-read "Select reasoning effort: " efforts nil t nil nil "medium")))
      (when effort
        (when (aider--send-command (format "/reasoning-effort %s" effort) t)
          (message "Reasoning effort set to %s for model %s" effort model))))))

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
        (when (aider--send-command (format "%s %s" command model) t)
          (message "%s changed to %s, customize aider-popular-models for the model candidates"
                   (substring command 1) model)
          (aider--maybe-prompt-and-set-reasoning-effort command model))))))

(provide 'aider)

;;; aider.el ends here
