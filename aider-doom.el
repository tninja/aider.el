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
                    :desc "Reset Aider (C-u: clear)" "s" #'aider-reset
                    :desc "Select Model (C-u: leaderboard)" "o" #'aider-change-model
                    :desc "Clear Aider buffer" "c" #'aider-clear-buffer
                    :desc "Exit Aider" "x" #'aider-exit)

                   (:prefix ("f" . "Files")
                    :desc "Add Current/Marked File (C-u: readonly)" "f" #'aider-add-current-file-or-dired-marked-files
                    :desc "Add All Files in Window" "w" #'aider-add-files-in-current-window
                    :desc "Add Module w/o grep (C-u: readonly)" "M" #'aider-add-module
                    :desc "Drop Current File" "O" #'aider-drop-current-file
                    :desc "Show Last Commit (C-u: magit-log)" "m" #'aider-magit-show-last-commit-or-log
                    :desc "Undo Last Change" "u" #'aider-undo-last-change
                    :desc "Pull or Review Code Change" "v" #'aider-pull-or-review-diff-file
                    :desc "File Evolution Analysis" "b" #'aider-magit-blame-analyze)

                   (:prefix ("c" . "Code")
                    :desc "Change Function/Region" "r" #'aider-function-or-region-refactor
                    :desc "Implement Requirement" "i" #'aider-implement-todo
                    :desc "Architect Discuss/Change" "t" #'aider-architect-discussion
                    :desc "Write Unit Test" "U" #'aider-write-unit-test
                    :desc "Refactor Code" "R" #'aider-refactor-book-method
                    :desc "Test Driven Development" "T" #'aider-tdd-cycle
                    :desc "Work with Legacy Code" "l" #'aider-legacy-code
                    :desc "Code/Doc Bootstrap" "B" #'aider-bootstrap)

                   (:prefix ("d" . "Discuss")
                    :desc "Ask question" "q" #'aider-ask-question
                    :desc "General question" "Q" #'aider-general-question
                    :desc "Then Go Ahead" "y" #'aider-go-ahead
                    :desc "Code Reading" "d" #'aider-code-read
                    :desc "Copy To Clipboard" "c" #'aider-copy-to-clipboard
                    :desc "Software Planning" "P" #'aider-start-software-planning
                    :desc "Debug Exception" "e" #'aider-debug-exception
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
