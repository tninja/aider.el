;;; aider-prompt-mode.el --- Aider prompt mode for editing aider prompt files -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;;; Commentary:
;; This file provides a major mode for editing aider prompt files.

;;; Code:

(require 'org)
(require 'aider-core)

;; Define the keymap for Aider Prompt Mode
(defvar aider-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'aider-send-line-or-region)
    (define-key map (kbd "C-c C-c") 'aider-send-block-or-region)
    (define-key map (kbd "C-c C-z") 'aider-switch-to-buffer)
    map)
  "Keymap for Aider Prompt Mode.")

;;;###autoload
(defun aider-send-line-or-region ()
  "Send text to the Aider buffer.
If region is active, send the selected region line by line.
Otherwise, send the line under cursor."
  (interactive)
  (if (region-active-p)
      (aider-send-region-by-line)
    (let ((line (thing-at-point 'line t)))
      (aider--send-command (string-trim line) t))))

;;;###autoload
(defun aider-send-region-by-line ()
  "Get the text of the current selected region, split them into lines,
strip the newline character from each line,
for each non-empty line, send it to aider session.
If no region is selected, show a message."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties 
                         (region-beginning) 
                         (region-end))))
        (mapc (lambda (line)
                (unless (string-empty-p line)
                  (aider--send-command line t)))
              (split-string region-text "\n" t)))
    (message "No region selected.")))

;;;###autoload
(defun aider-send-block-or-region ()
  "Send the current active region text or, if no region is active, send the current paragraph content to the aider session.
When sending paragraph content, preserve cursor position and deactivate mark afterwards."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (unless (string-empty-p region-text)
          (aider--send-command region-text t)))
    (save-excursion  ; preserve cursor position
      (let ((region-text
             (progn
               (mark-paragraph)  ; mark paragraph
               (buffer-substring-no-properties (region-beginning) (region-end)))))
        (unless (string-empty-p region-text)
          (aider--send-command region-text t))
        (deactivate-mark)))))  ; deactivate mark after sending

;;;###autoload
(defun aider-open-prompt-file ()
  "Open aider prompt file under git repo root.
If file doesn't exist, create it with command binding help and sample prompt."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (prompt-file (when git-root
                       (expand-file-name aider-prompt-file-name git-root))))
    (if prompt-file
        (progn
          (find-file-other-window prompt-file)
          (unless (file-exists-p prompt-file)
            ;; Insert initial content for new file
            (insert "# Aider Prompt File - Command Reference:\n")
            (insert "# C-c C-n: Single line prompt: Send current line or selected region line by line as multiple prompts\n")
            (insert "# C-c C-c: Multi-line prompt: Send current block or selected region as a single prompt\n")
            (insert "# C-c C-z: Switch to aider buffer\n\n")
            (insert "* Sample task:\n\n")
            (insert "/ask what this repo is about?\n")
            (save-buffer)))
      (message "Not in a git repository"))))

;; Define the Aider Prompt Mode (derived from org-mode)
;;;###autoload
(define-derived-mode aider-prompt-mode org-mode "Aider Prompt"
  "Major mode derived from org-mode for editing aider prompt files.
Special commands:
\\{aider-prompt-mode-map}"
  ;; syntax highlighting
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  ;; use YASnippet
  (when (require 'yasnippet nil t)
    (yas-minor-mode 1)
    (aider--setup-snippets)))

;;;###autoload
(add-to-list 'auto-mode-alist 
             `(,(concat "\\<" (regexp-quote aider-prompt-file-name) "\\'") . aider-prompt-mode))

(provide 'aider-prompt-mode)

;;; aider-prompt-mode.el ends here
