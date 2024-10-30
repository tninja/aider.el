;;; doom-aider.el --- Description -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Doom integration for Aider
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We will want to check if the current buffer is in version control
;;; before enabling Doom-specific keybindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define a keymap for Doom-specific keybindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar aider-doom-leader-map
  (let ((map (make-sparse-keymap)))
    ;; Define leader keybindings for aider-doom-mode
    (define-key map (kbd "l a c") #'aider-add-current-file)
    (define-key map (kbd "l a w") #'aider-add-files-in-current-window)
    (define-key map (kbd "l a b") #'aider-batch-add-dired-marked-files)
    (define-key map (kbd "l a g") #'aider-repo-find-name-dired)
    (define-key map (kbd "l a d") #'aider-git-repo-root-dired)

    (define-key map (kbd "l r f") #'aider-current-file-read-only)
    (define-key map (kbd "l r l") #'aider-send-line-under-cursor)
    (define-key map (kbd "l r p") #'aider-send-paragraph)

    (define-key map (kbd "l c c") #'aider-code-change)
    (define-key map (kbd "l c r") #'aider-region-refactor)
    (define-key map (kbd "l c u") #'aider-undo-last-change)

    (define-key map (kbd "l d a") #'aider-ask-question)
    (define-key map (kbd "l d d") #'aider-architect-discussion)
    (define-key map (kbd "l d r") #'aider-region-explain)
    (define-key map (kbd "l d e") #'aider-debug-exception)

    (define-key map (kbd "l o c") #'aider-general-command)
    (define-key map (kbd "l o h") #'aider-help)
    (define-key map (kbd "l o g") #'aider-magit-show-last-commit)

    (define-key map (kbd "l x") #'aider-exit)
    map)
  "Keymap for `aider-doom-mode' leader commands.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uses a minor mode to enable Doom-specific keybindings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun aider-doom-mode-setup ()
  "Setup `aider-doom-mode' with leader bindings if in a git directory."
  (when (vc-backend (buffer-file-name))
    ;; Only enable leader keymap if in a version-controlled directory
    (define-key aider-doom-mode-map (kbd "SPC") aider-leader-map)))

(define-minor-mode aider-doom-mode
  "A minor mode for enabling Doom-style keybindings for Aider."
  :lighter " Aider"
  :keymap (make-sparse-keymap)
  (if aider-doom-mode
      ;; Set up bindings if the mode is enabled
      (aider-doom-mode-setup)
    ;; Cleanup when the mode is disabled
    (define-key aider-doom-mode-map (kbd "SPC") nil)))

(provide 'doom-aider)

;;; doom-aider.el ends here
