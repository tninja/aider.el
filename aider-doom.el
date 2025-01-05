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
                    :desc "Line at cursor" "l" #'aider-send-line-under-cursor
                    :desc "Paragraph at cursor, line by line" "p" #'aider-send-paragraph-by-line
                    :desc "Region as block" "r" #'aider-send-region
                    )

                   (:prefix ("c" . "Code")
                    :desc "Architecture" "d" #'aider-architect-discussion
                    :desc "Change" "c" #'aider-code-change
                    :desc "Refactor Function or Region" "r" #'aider-function-or-region-refactor
                    :desc "Undo change" "u" #'aider-undo-last-change
                    :desc "Show last commit" "g" #'aider-magit-show-last-commit
                    )

                   (:prefix ("d" . "Discuss")
                    :desc "Ask question" "a" #'aider-ask-question
                    :desc "Explain Function or Region" "r" #'aider-function-or-region-explain
                    :desc "Exception debugging" "e" #'aider-debug-exception
                    )

                   (:prefix ("z" . "Other")
                    :desc "General command" "c" #'aider-general-command
                    :desc "Help" "h" #'aider-help
                    )

                   :desc "Open Aider" "o" #'aider-run-aider
                   :desc "Reset Aider" "r" #'aider-reset
                   :desc "Exit Aider" "x" #'aider-exit
                   ))))

;; Function to send "/read <current buffer file full path>" to corresponding aider buffer
;;;###autoload
(defun aider-current-file-read-only ()
  "Send the command \"/read-only <current buffer file full path>\" to the corresponding aider comint buffer. This is only useful for doom menu now"
  (interactive)
  (aider-add-or-read-current-file "/read-only"))

;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'aider-doom-setup-keys)
(add-hook 'dired-mode-hook #'aider-doom-setup-keys)
(add-hook 'after-change-major-mode-hook #'aider-doom-setup-keys)

(provide 'aider-doom)
;;; aider-doom.el ends here
