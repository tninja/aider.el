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
                    :desc "Run Aider" "a" #'aider-run-aider
                    :desc "Switch to Aider buffer" "z" #'aider-switch-to-buffer
                    :desc "Open prompt file" "p" #'aider-open-prompt-file
                    :desc "Reset Aider" "s" #'aider-reset
                    :desc "Change model" "o" #'aider-change-model
                    :desc "Clear Aider buffer" "c" #'aider-clear-buffer
                    :desc "Exit Aider" "x" #'aider-exit)

                   (:prefix ("f" . "Files")
                    :desc "Add current/marked files" "f" #'aider-add-current-file-or-dired-marked-files
                    :desc "Add files in window" "w" #'aider-add-files-in-current-window
                    :desc "Drop current file" "O" #'aider-drop-current-file
                    :desc "Show last commit" "m" #'aider-magit-show-last-commit-or-log
                    :desc "Undo last change" "u" #'aider-undo-last-change
                    :desc "Pull or review diff" "v" #'aider-pull-or-review-diff-file
                    :desc "Open history" "h" #'aider-open-history)

                   (:prefix ("c" . "Code")
                    :desc "Refactor function/region" "r" #'aider-function-or-region-refactor
                    :desc "Implement requirement" "i" #'aider-implement-todo
                    :desc "Architect discussion" "t" #'aider-architect-discussion
                    :desc "Write unit test" "U" #'aider-write-unit-test
                    :desc "Refactor code" "R" #'aider-refactor-book-method
                    :desc "Direct code change" "c" #'aider-code-change)

                   (:prefix ("d" . "Discuss")
                    :desc "Ask question" "q" #'aider-ask-question
                    :desc "Go ahead" "y" #'aider-go-ahead
                    :desc "General question" "Q" #'aider-general-question
                    :desc "Debug exception" "e" #'aider-debug-exception
                    :desc "Help" "H" #'aider-help)

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
