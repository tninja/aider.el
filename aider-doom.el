;;; aider-doom.el --- Description -*- lexical-binding: t; no-byte-compile: t -*-
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
                    :desc "File read-only" "f" #'aider-current-file-read-only
                    :desc "Files in window" "w" #'aider-add-files-in-current-window
                    :desc "Add Same Type Files under dir" "d" #'aider-add-same-type-files-under-dir
                    :desc "Batch direct marked files" "b" #'aider-batch-add-dired-marked-files
                    )

                   (:prefix ("b" . "Buffer")
                    :desc "Switch to Aider" "b" #'aider-switch-to-buffer
                    :desc "Clear Aider" "c" #'aider-clear
                    )

                   (:prefix ("s" . "Send")
                    :desc "Line at cursor" "l" #'aider-send-line-or-region
                    :desc "Paragraph at cursor, line by line" "p" #'aider-send-region-by-line
                    :desc "Region as block" "r" #'aider-send-block-or-region
                    )

                   (:prefix ("c" . "Code")
                    :desc "Architecture" "d" #'aider-architect-discussion
                    :desc "Change" "c" #'aider-code-change
                    :desc "Refactor Function or Region" "r" #'aider-function-or-region-refactor
                    :desc "Implement Requirement in-place" "i" #'aider-implement-todo
                    :desc "Undo change" "u" #'aider-undo-last-change
                    :desc "Show last commit" "g" #'aider-magit-show-last-commit
                    )

                   (:prefix ("d" . "Discuss")
                    :desc "Ask question" "a" #'aider-ask-question
                    :desc "Explain Function or Region" "r" #'aider-function-or-region-explain
                    :desc "Exception debugging" "e" #'aider-debug-exception
                    )

                   (:prefix ("t" . "Test")
                    :desc "Write Unit Test" "w" #'aider-write-unit-test
                    :desc "Fix Failed Test" "f" #'aider-fix-failing-test-under-cursor
                    )

                   (:prefix ("z" . "Other")
                    :desc "General command" "c" #'aider-general-command
                    :desc "Help" "h" #'aider-help
                    )

                   :desc "Open Aider" "o" #'aider-run-aider
                   :desc "Reset Aider" "r" #'aider-reset
                   :desc "Exit Aider" "x" #'aider-exit
                   ))))

;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'aider-doom-setup-keys)
(add-hook 'dired-mode-hook #'aider-doom-setup-keys)
(add-hook 'after-change-major-mode-hook #'aider-doom-setup-keys)

(provide 'aider-doom)
;;; aider-doom.el ends here
