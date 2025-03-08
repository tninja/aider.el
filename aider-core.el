;;; aider-core.el --- Core functionality for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides core functionality for the Aider package.

;;; Code:

(require 'comint)
(require 'magit)

(defgroup aider nil
  "Customization group for the Aider package."
  :prefix "aider-"
  :group 'convenience)

(defcustom aider-program "aider"
  "The name or path of the aider program."
  :type 'string
  :group 'aider)

(defcustom aider-args '()
  "Arguments to pass to the Aider command."
  :type '(repeat string)
  :group 'aider)

(defcustom aider--switch-to-buffer-other-frame nil
  "When non-nil, open Aider buffer in a new frame using `switch-to-buffer-other-frame'.
When nil, use standard `display-buffer' behavior."
  :type 'boolean
  :group 'aider)

(defface aider-command-separator
  '((((type graphic)) :strike-through t :extend t)
    (((type tty)) :inherit font-lock-comment-face :underline t :extend t))
  "Face for command separator in aider."
  :group 'aider)

(defface aider-command-text
  '((t :inherit bold))
  "Face for commands sent to aider buffer."
  :group 'aider)

(defvar aider-font-lock-keywords '(("^\x2500+\n?" 0 '(face aider-command-separator) t)
                                   ("^\x2500+" 0 '(face nil display (space :width 2))))
  "Font lock keywords for aider buffer.")

(defvar aider-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "Keymap for `aider-comint-mode'.")

(define-derived-mode aider-comint-mode comint-mode "Aider Session"
  "Major mode for interacting with Aider.
Inherits from `comint-mode' with some Aider-specific customizations.

\\{aider-comint-mode-map}"
  ;; Set up font-lock
  (setq font-lock-defaults '(nil t))
  (font-lock-add-keywords nil aider-font-lock-keywords t)
  ;; Set up input sender for multi-line handling
  (setq-local comint-input-sender 'aider-input-sender))

(defvar aider-read-string-history nil
  "History list for aider read string inputs.")
(if (bound-and-true-p savehist-loaded)
    (add-to-list 'savehist-additional-variables 'aider-read-string-history)
  (add-hook 'savehist-mode-hook
            (lambda ()
              (add-to-list 'savehist-additional-variables 'aider-read-string-history))))

;;;###autoload
(defun aider-plain-read-string (prompt &optional initial-input)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
This function can be customized or redefined by the user."
  (read-string prompt initial-input 'aider-read-string-history))

;;;###autoload
(defalias 'aider-read-string 'aider-plain-read-string)

(eval-and-compile
  ;; Ensure the alias is always available in both compiled and interpreted modes.
  (defalias 'aider-read-string 'aider-plain-read-string))

(defun aider-buffer-name ()
  "Generate the Aider buffer name based on the git repo or current buffer file path.
If not in a git repository and no buffer file exists, an error is raised."
  (let ((git-repo-path (magit-toplevel))
        (current-file (buffer-file-name)))
    (cond
     ;; Case 1: Valid git repo path (not nil and not containing "fatal")
     ((and git-repo-path 
           (stringp git-repo-path)
           (not (string-match-p "fatal" git-repo-path)))
      (format "*aider:%s*" (file-truename git-repo-path)))
     ;; Case 2: Has buffer file (handles both nil and "fatal" git-repo-path cases)
     (current-file
      (format "*aider:%s*" 
              (file-truename (file-name-directory current-file))))
     ;; Case 3: No git repo and no buffer file
     (t
      (error "Not in a git repository and current buffer is not associated with a file")))))

(defun aider--process-message-if-multi-line (str)
  "Entering multi-line chat messages
https://aider.chat/docs/usage/commands.html#entering-multi-line-chat-messages
If STR contains newlines, wrap it in {aider\\nstr\\naider}.
Otherwise return STR unchanged."
  (if (string-match-p "\n" str)
      (format "{aider\n%s\naider}" str)
    str))

(defun aider--comint-send-string-syntax-highlight (buffer text)
  "Send TEXT to the comint BUFFER with syntax highlighting.
This function ensures proper syntax highlighting by inheriting face properties
from the source buffer and maintaining proper process markers."
  (with-current-buffer buffer
    (let ((process (get-buffer-process buffer))
          (inhibit-read-only t))
      (goto-char (process-mark process))
      ;; Insert text with proper face properties
      (insert (propertize text
                         'face 'aider-command-text
                         'font-lock-face 'aider-command-text
                         'rear-nonsticky t))
      ;; Update process mark and send text
      (set-marker (process-mark process) (point))
      (comint-send-string process text))))

;; Shared helper function to send commands to corresponding aider buffer
(defun aider--send-command (command &optional switch-to-buffer)
  "Send COMMAND to the corresponding aider comint buffer after performing necessary checks.
COMMAND should be a string representing the command to send."
  ;; Check if the corresponding aider buffer exists
  (if-let ((aider-buffer (get-buffer (aider-buffer-name))))
      (let* ((command (aider--process-message-if-multi-line command))
             (aider-process (get-buffer-process aider-buffer)))
        ;; Check if the corresponding aider buffer has an active process
        (if (and aider-process (comint-check-proc aider-buffer))
            (progn
              ;; Send the command to the aider process
              (aider--comint-send-string-syntax-highlight aider-buffer (concat command "\n"))
              ;; Provide feedback to the user
              ;; (message "Sent command to aider buffer: %s" (string-trim command))
              (when switch-to-buffer
                (aider-switch-to-buffer))
              (sleep-for 0.2))
          (message "No active process found in buffer %s." (aider-buffer-name))))
    (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))))

;;;###autoload
(defun aider-switch-to-buffer (&optional source-buffer)
  "Switch to the Aider buffer.
When `aider--switch-to-buffer-other-frame' is non-nil, open in a new frame.
If the current buffer is already the Aider buffer, do nothing.
Optional SOURCE-BUFFER specifies the buffer to inherit syntax highlighting from;
if nil, use current buffer."
  (interactive)
  (if (string= (buffer-name) (aider-buffer-name))
      (message "Already in Aider buffer")
    (let ((source-buffer (or source-buffer (current-buffer))))
      (if-let ((buffer (get-buffer (aider-buffer-name))))
          (progn
            (if aider--switch-to-buffer-other-frame
                (switch-to-buffer-other-frame buffer)
              (pop-to-buffer buffer))
            (when (with-current-buffer source-buffer
                    (derived-mode-p 'prog-mode))
              (aider--inherit-source-highlighting source-buffer)))
        (message "Aider buffer '%s' does not exist." (aider-buffer-name))))))

(defun aider--inherit-source-highlighting (source-buffer)
  "Inherit syntax highlighting settings from SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (let ((source-keywords font-lock-keywords)
          (source-keywords-only font-lock-keywords-only)
          (source-keywords-case-fold-search font-lock-keywords-case-fold-search)
          ;; (source-syntax-table (syntax-table))
          (source-defaults font-lock-defaults))
      (with-current-buffer (aider-buffer-name)
        (when (not (string-equal (prin1-to-string source-keywords)
                               (prin1-to-string font-lock-keywords)))
            ;; (set-syntax-table source-syntax-table)
            (setq font-lock-defaults
                  (if source-defaults
                      source-defaults
                    `((,source-keywords)
                      nil
                      ,source-keywords-case-fold-search)))
          (setq font-lock-keywords source-keywords
                font-lock-keywords-only source-keywords-only
                font-lock-keywords-case-fold-search source-keywords-case-fold-search)
          (font-lock-mode 1)
          (font-lock-ensure)
          (message "Aider buffer syntax highlighting inherited from %s"
                   (with-current-buffer source-buffer major-mode))
          )
        ))))

;;;###autoload
(defun aider-run-aider (&optional edit-args)
  "Create a comint-based buffer and run \"aider\" for interactive conversation.
With the universal argument, prompt to edit aider-args before running."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (current-args (if edit-args
                           (split-string
                            (read-string "Edit aider arguments: "
                                         (mapconcat 'identity aider-args " ")))
                         aider-args)))
    (unless (comint-check-proc buffer-name)
      (apply 'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
      (with-current-buffer buffer-name
        (aider-comint-mode)))
    (aider-switch-to-buffer)))

(defun aider-input-sender (proc string)
  "Handle multi-line inputs being sent to Aider."
  (comint-simple-send proc (aider--process-message-if-multi-line string)))

(provide 'aider-core)

;;; aider-core.el ends here
