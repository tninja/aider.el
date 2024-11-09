;;; aider-doom.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
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
                   (:prefix ("a" . "Add")
                    :desc "Current file" "c" #'aider-add-current-file
                    :desc "Files in window" "w" #'aider-add-files-in-current-window
                    :desc "Batch direct marked files" "b" #'aider-batch-add-dired-marked-files
                    :desc "Find files in repo" "g" #'aider-repo-find-name-dired
                    :desc "Open repo root" "d" #'aider-git-repo-root-dired)

                   (:prefix ("b" . "Buffer")
                    :desc "Switch to Aider" "b" #'aider-switch-to-buffer
                    :desc "Clear Aider" "c" #'aider-clear)

                   (:prefix ("s" . "Send")
                    :desc "File read-only" "f" #'aider-current-file-read-only
                    :desc "Line at cursor" "l" #'aider-send-line-under-cursor
                    :desc "Paragraph at cursor" "p" #'aider-send-paragraph)

                   (:prefix ("c" . "Code")
                    :desc "Change" "c" #'aider-code-change
                    :desc "Refactor region" "r" #'aider-region-refactor
                    :desc "Undo change" "u" #'aider-undo-last-change)

                   (:prefix ("d" . "Discuss")
                    :desc "Ask question" "a" #'aider-ask-question
                    :desc "Architecture" "d" #'aider-architect-discussion
                    :desc "Region explanation" "r" #'aider-region-explain
                    :desc "Exception debugggin" "e" #'aider-debug-exception)

                   (:prefix ("z" . "Other")
                    :desc "General command" "c" #'aider-general-command
                    :desc "Help" "h" #'aider-help
                    :desc "Show last commit" "g" #'aider-magit-show-last-commit)


                   :desc "Open Aider" "o" #'aider-run-aider
                   :desc "Reset Aider" "r" #'aider-reset
                   :desc "Exit Aider" "x" #'aider-exit))))

;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'aider-doom-setup-keys)
(add-hook 'dired-mode-hook #'aider-doom-setup-keys)
(add-hook 'after-change-major-mode-hook #'aider-doom-setup-keys)

(provide 'aider-doom)
;;; aider-doom.el ends here
