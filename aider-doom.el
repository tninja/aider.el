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
                   (:prefix ("f" . "Files")
                    :desc "Add current/marked files" "a" #'aider-add-current-file-or-dired-marked-files
                    :desc "Add file read-only" "r" #'aider-current-file-read-only
                    :desc "Add files in window" "w" #'aider-add-files-in-current-window
                    :desc "Add same type files in dir" "s" #'aider-add-same-type-files-under-dir
                    :desc "Batch add marked files" "m" #'aider-batch-add-dired-marked-files
                    :desc "Drop current file" "d" #'aider-drop-current-file
                    :desc "Show last commit" "c" #'aider-magit-show-last-commit
                    :desc "Undo last change" "u" #'aider-undo-last-change
                    )

                   (:prefix ("c" . "Code")
                    :desc "Architecture discussion" "a" #'aider-architect-discussion
                    :desc "Direct code change" "c" #'aider-code-change
                    :desc "Refactor function/region" "r" #'aider-function-or-region-refactor
                    :desc "Implement requirement" "i" #'aider-implement-todo
                    :desc "Write unit test" "t" #'aider-write-unit-test
                    :desc "Fix failing test" "f" #'aider-fix-failing-test-under-cursor
                    )

                   (:prefix ("d" . "Discuss")
                    :desc "Ask question" "q" #'aider-ask-question
                    :desc "Go ahead" "y" #'aider-go-ahead
                    :desc "Explain function/region" "e" #'aider-function-or-region-explain
                    :desc "Debug exception" "d" #'aider-debug-exception
                    :desc "Help" "h" #'aider-help
                    )

                   (:prefix ("p" . "Process")
                    :desc "Switch to Aider buffer" "b" #'aider-switch-to-buffer
                    :desc "Clear Aider buffer" "c" #'aider-clear-buffer
                    :desc "Reset Aider" "r" #'aider-reset
                    :desc "Open prompt file" "p" #'aider-open-prompt-file
                    :desc "Change model" "m" #'aider-change-model
                    :desc "Other command" "o" #'aider-other-process-command
                    :desc "Send line/region" "l" #'aider-send-line-or-region
                    :desc "Send region by line" "s" #'aider-send-region-by-line
                    )

                   :desc "Run Aider" "o" #'aider-run-aider
                   :desc "Exit Aider" "x" #'aider-exit
                   :desc "Transient Menu" "t" #'aider-transient-menu
                   ))))

;; Add the setup function to appropriate hooks
(add-hook 'find-file-hook #'aider-doom-setup-keys)
(add-hook 'dired-mode-hook #'aider-doom-setup-keys)
(add-hook 'after-change-major-mode-hook #'aider-doom-setup-keys)

(provide 'aider-doom)
;;; aider-doom.el ends here
