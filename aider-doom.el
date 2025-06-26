;;; aider-doom.el --- Description -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; This file is not part of GNU Emacs.
;;
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;;
;;  Doom integration for Aider
;;
;;; Code:

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


(defun aider-doom-setup-keys ()
  "Setup Aider keybindings if the current buffer is in a git repository."
  (when (and (featurep 'doom-keybinds)
             (vc-backend (or (buffer-file-name) default-directory)))
    (map! :leader
          (:prefix ("A" . "Aider")
                   (:prefix ("p" . "Process")
                    :desc "Run Aider (C-u: args)" "a" #'aider-run-aider
                    :desc "Switch to Aider Buffer" "z" #'aider-switch-to-buffer
                    :desc "Input with Repo Prompt File" "p" #'aider-open-prompt-file
                    :desc "Create / Checkout Branch" "c" #'magit-branch-or-checkout
                    :desc "Reset Aider (C-u: clear)" "s" #'aider-reset
                    :desc "Select Model (C-u: benchmark)" "o" #'aider-change-model
                    :desc "Exit Aider" "X" #'aider-exit)

                   (:prefix ("f" . "Files")
                    :desc "Add Current/Marked File (C-u: readonly)" "f" #'aider-add-current-file-or-dired-marked-files
                    :desc "Add All Files in Window" "w" #'aider-add-files-in-current-window
                    :desc "Add Module w/o grep (C-u: readonly)" "M" #'aider-add-module
                    :desc "Drop File in Buffer / under Cursor" "O" #'aider-drop-current-file
                    :desc "Expand Context for Current File" "x" #'aider-expand-context-current-file
                    :desc "Show Last Commit (C-u: magit-log)" "m" #'aider-magit-show-last-commit-or-log
                    :desc "Undo Last Change" "u" #'aider-undo-last-change
                    :desc "Pull or Review Code Change" "v" #'aider-pull-or-review-diff-file
                    :desc "File Evolution Analysis (C-u: Repo)" "e" #'aider-magit-blame-or-log-analyze)

                   (:prefix ("c" . "Code")
                    :desc "Change Function/Region" "c" #'aider-function-or-region-change
                    :desc "Implement Requirement" "i" #'aider-implement-todo
                    :desc "Architect Discuss/Change" "t" #'aider-architect-discussion
                    :desc "Write Unit Test" "U" #'aider-write-unit-test
                    :desc "Fix Flycheck Errors" "F" #'aider-flycheck-fix-errors-in-scope
                    :desc "Refactor Code" "r" #'aider-refactor-book-method
                    :desc "Test Driven Development" "T" #'aider-tdd-cycle
                    :desc "Work with Legacy Code" "l" #'aider-legacy-code
                    :desc "Code/Doc Bootstrap" "B" #'aider-bootstrap)

                   (:prefix ("d" . "Discuss")
                    :desc "Question (C-u no context)" "q" #'aider-ask-question
                    :desc "Then Go Ahead" "y" #'aider-go-ahead
                    :desc "Code Reading" "d" #'aider-code-read
                    :desc "Copy To Clipboard" "C" #'aider-copy-to-clipboard
                    :desc "Software Planning" "P" #'aider-start-software-planning
                    :desc "Debug Exception" "E" #'aider-debug-exception
                    :desc "Open History" "h" #'aider-open-history
                    :desc "Help (C-u: homepage)" "?" #'aider-help)

                   :desc "Transient Menu" "t" #'aider-transient-menu))))

;;;###autoload
(defun aider-doom-enable ()
  "Enable Aider integration with Doom Emacs.
This adds the necessary hooks to set up keybindings in appropriate buffers."
  (interactive)
  (add-hook 'find-file-hook #'aider-doom-setup-keys)
  (add-hook 'dired-mode-hook #'aider-doom-setup-keys)
  (add-hook 'after-change-major-mode-hook #'aider-doom-setup-keys))

(aider-doom-enable)

(provide 'aider-doom)
;;; aider-doom.el ends here
