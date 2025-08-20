;;; aider-core.el --- Core functionality for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides core functionality for the Aider package.

;;; Code:

(require 'comint)
(require 'magit)
(require 'savehist)
(require 'cl-lib)

(require 'aider-utils)
(require 'aider-comint-markdown)  ;; for markdown advice & highlighting

(declare-function evil-define-key* "evil" (state map key def))

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

(defcustom aider-auto-trigger-prompt nil
  "Auto-trigger prompt insertion for /ask, /code, etc. if non-nil."
  :type 'boolean
  :group 'aider)

(defcustom aider-use-branch-specific-buffers nil
  "When non-nil, Aider buffer names will include the current git branch.
The format will be *aider:<git-repo-path>:<branch-name>*."
  :type 'boolean
  :group 'aider)

(defcustom aider-confirm-on-main-branch t
  "When non-nil, ask before sending /code, /architect, /commit or \
\"go ahead\" on main, master, or develop branches. Set to nil when \
user picks “yes and do not ask again.”"
  :type 'boolean
  :group 'aider)

(defcustom aider-auto-trigger-command-completion t
  "When non-nil, automatically trigger command completion when typing '/' in aider buffer."
  :type 'boolean
  :group 'aider)

(defcustom aider-auto-trigger-file-path-insertion t
  "When non-nil, automatically trigger file path insertion for file-related commands."
  :type 'boolean
  :group 'aider)

(defcustom aider-enable-markdown-highlighting t
  "When non-nil, automatically highlight markdown in Aider comint buffers."
  :type 'boolean
  :group 'aider)

(defvar aider--switch-to-buffer-other-frame nil
  "Boolean controlling Aider buffer display behavior.
When non-nil, open Aider buffer in a new frame.
When nil, use standard `display-buffer' behavior.")

(defvar aider-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-f") #'aider-prompt-insert-add-file-path)
    (define-key map (kbd "TAB") #'aider-core-insert-prompt)
    (define-key map (kbd "C-c C-y") #'aider-go-ahead)
    map)
  "Keymap for `aider-comint-mode'.")

(eval-after-load 'evil
  '(evil-define-key* 'normal aider-comint-mode-map (kbd "SPC") #'aider-core-insert-prompt))

;;;###autoload
(defun aider-prompt-insert-add-file-path ()
  "Select and insert the relative file path to git repository root."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (file (read-file-name "Select file: " git-root nil t)))
    (if (and file (file-exists-p file))
        (let ((relative-path (if git-root
                                (file-relative-name file git-root)
                              file)))
          (insert relative-path))
      (message "No valid file selected."))))

;;;###autoload
(defun aider-go-ahead ()
  "Send the command \"go ahead\" to the corresponding aider comint buffer."
  (interactive)
  (aider--send-command "/code go ahead" t))

(defconst aider--command-list
  '("/add" "/architect" "/ask" "/code" "/reset" "/undo" "/lint" "/read-only"
    "/drop" "/copy" "/copy-context" "/clear" "/commit" "/exit" "/quit"
    "/paste" "/help" "/chat-mode" "/diff" "/editor" "/git"
    "/load" "/ls" "/map" "/map-refresh" "/think-tokens" "/tokens"
    "/model" "/editor-model" "/weak-model" "/models" "/reasoning-effort"
    "/multiline-mode" "/report" "/run" "/save" "/settings" "/test"
    "/voice" "/web")
  "A list of common Aider commands for completion.")

(define-derived-mode aider-comint-mode comint-mode "Aider Session"
  "Major mode for interacting with Aider.
Inherits from `comint-mode' with some Aider-specific customizations.
\\{aider-comint-mode-map}"
  ;; Set up font-lock
  ;; (setq font-lock-defaults '(nil t))
  ;; (font-lock-add-keywords nil aider-font-lock-keywords t)
  ;; Set up input sender for multi-line handling
  (setq-local comint-input-sender 'aider-input-sender)
  ;; Enable comint's built-in input highlighting
  (setq-local comint-highlight-input t)
  ;; visual-line-mode
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 1)))
  ;; Add command completion hooks
  (add-hook 'completion-at-point-functions #'aider-core--command-completion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-command-completion nil t)
  ;; Automatically trigger file path insertion for file-related commands
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-file-path-insertion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-insert-prompt nil t)
  ;; only apply markdown highlighting if enabled
  (when aider-enable-markdown-highlighting
    (aider--apply-markdown-highlighting))
  ;; Load history from .aider.input.history if available
  (let ((history-file-path (aider--generate-history-file-name))) ; Bind history-file-path here
    (condition-case err ; catch any error during history loading
        (when (and history-file-path (file-readable-p history-file-path)) ; Check history-file-path too
          ;; Initialize comint ring for this buffer
          (setq comint-input-ring (make-ring comint-input-ring-size))
          (let ((parsed-history (aider--parse-aider-cli-history history-file-path)))
            (when parsed-history
              (dolist (item parsed-history)
                (when (and item (stringp item))
                  (comint-add-to-input-history item))))))
      (error (message "Error loading Aider input history from %s: %s. Continuing without loading history."
                      (or history-file-path "its determined location") ; provide file path if available
                      (error-message-string err)))))) ; display the error message


;;;###autoload
(defun aider-plain-read-string (prompt &optional initial-input candidate-list)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
CANDIDATE-LIST provides additional completion options if provided.
This function combines candidate-list with history for better completion."
  ;; Combine candidate-list with history, removing duplicates
  (let ((completion-candidates
         (delete-dups (append candidate-list
                              (when (boundp 'aider-read-string-history)
                                aider-read-string-history)))))
    ;; Use completing-read with the combined candidates
    (completing-read prompt
                     completion-candidates
                     nil nil initial-input
                     'aider-read-string-history)))

;;;###autoload
(defalias 'aider-read-string #'aider-plain-read-string)

(eval-and-compile
  ;; Ensure the alias is always available in both compiled and interpreted modes.
  (defalias 'aider-read-string #'aider-plain-read-string))

(defun aider--comint-send-string-syntax-highlight (buffer text)
  "Send TEXT to the comint BUFFER using comint's standard input mechanism.
Uses comint's built-in highlighting for input text."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      ;; Move to the end of the buffer
      (goto-char (point-max))
      ;; Insert the text - comint will handle the highlighting
      (insert text)
      ;; Use comint's standard input handling which will apply comint-highlight-input
      (comint-send-input))))

;; Shared helper function to send commands to corresponding aider buffer
(defun aider--send-command (command &optional switch-to-buffer log)
  "Send COMMAND to the corresponding aider comint buffer.
after performing necessary checks.
COMMAND should be a string representing the command to send.
Optional SWITCH-TO-BUFFER, when non-nil, switches to the aider buffer.
Optional LOG, when non-nil, logs the command to the message area.
Returns t if command was sent successfully, nil otherwise."
  ;; Check if the corresponding aider buffer exists
  (if-let ((aider-buffer (aider--validate-aider-buffer)))
      (let* ((command (replace-regexp-in-string "\\`[\n\r]+" "" command))
             (command (replace-regexp-in-string "[\n\r]+\\'" "" command))
             (command (aider--process-message-if-multi-line command))
             ;; new: check branch + confirmation
             (continue?
              (let* ((git-root (aider--get-git-repo-root))
                     (branch  (when git-root (aider--get-current-git-branch git-root)))
                     (danger? (and branch
                                   (member branch '("main" "master" "develop"))
                                   aider-confirm-on-main-branch
                                   (or (string-prefix-p "/code"      command)
                                       (string-prefix-p "/architect" command)
                                       (string-prefix-p "/commit"    command)
                                       (string-prefix-p "go ahead"   command)))))
                (if (not danger?) t
                  (let ((choice
                         (read-multiple-choice
                          (format "You are on '%s'.  Changes are normally made on feature branches.\nReally send this command here? " branch)
                          `((?y "Yes, send it")
                            (?n "No, abort")
                            (?a "Yes and do not ask again")))))
                    (cond
                     ((eq (car choice) ?n)
                      (message "Aborted on '%s', you can use C-c a b to create feature branch." branch)
                      nil)
                     ((eq (car choice) ?a)
                      (setq aider-confirm-on-main-branch nil)
                      t)
                     (t t)))))))
        ;; wrap existing send logic in (when continue?)
        (when continue?
          (if (and (get-buffer-process aider-buffer) (comint-check-proc aider-buffer))
              (progn
                (aider--comint-send-string-syntax-highlight aider-buffer command)
                (when log    (message "Sent command: %s" (string-trim command)))
                (when switch-to-buffer (aider-switch-to-buffer))
                (sleep-for 0.2)
                t)
            (message "No active process in %s." (aider-buffer-name))
            nil)))
    (progn
      (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))
      nil))) ; Return nil for failure

;;;###autoload
(defun aider-switch-to-buffer ()
  "Switch to the Aider buffer.
When `aider--switch-to-buffer-other-frame' is non-nil, open in a new frame.
If the current buffer is already the Aider buffer, do nothing."
  (interactive)
  (if (string= (buffer-name) (aider-buffer-name))
      (message "Already in Aider buffer")
    (when-let ((buffer (aider--validate-aider-buffer)))
      (if aider--switch-to-buffer-other-frame
          (switch-to-buffer-other-frame buffer)
        (pop-to-buffer buffer))
      ;; Scroll to the end of the buffer after switching
      (with-current-buffer buffer
        (goto-char (point-max))))))

(defun aider--maybe-prompt-subtree-only-for-special-modes (current-args)
  "Prompt for --subtree-only in dired/eshell/shell if not in git root.
Return potentially modified CURRENT-ARGS."
  (if (and (memq major-mode '(dired-mode eshell-mode shell-mode))
           (not (aider--is-default-directory-git-root))
           (y-or-n-p (format "Current buffer is %s and current directory (%s) is not git root. Use --subtree-only mode?"
                             (cond ((eq major-mode 'dired-mode) "a directory")
                                   ((eq major-mode 'eshell-mode) "eshell")
                                   ((eq major-mode 'shell-mode) "shell")
                                   (t ""))
                             (abbreviate-file-name default-directory))))
      (if (member "--subtree-only" current-args)
          current-args
        (append current-args '("--subtree-only")))
    current-args))

;;;###autoload
(defun aider--prepare-aider-args (edit-args subtree-only)
  "Prepare aider arguments based on EDIT-ARGS and SUBTREE-ONLY flags."
  (let ((current-args (if edit-args
                          (split-string
                           (read-string "Edit aider arguments: "
                                        (mapconcat #'identity aider-args " ")))
                        aider-args)))
    ;; automatically add --no-auto-accept-architect if there is no --auto-accept-architect
    (unless (or (member "--auto-accept-architect" current-args)
                (member "--no-auto-accept-architect" current-args))
      (setq current-args (append current-args '("--no-auto-accept-architect"))))
    ;; Handle --subtree-only prompting for special modes
    (setq current-args (aider--maybe-prompt-subtree-only-for-special-modes current-args))
    ;; Add --subtree-only if the parameter is set and it's not already present
    (when (and subtree-only (not (member "--subtree-only" current-args)))
      (setq current-args (append current-args '("--subtree-only")))
      (message "Adding --subtree-only argument as requested."))
    current-args))

(defun aider--create-aider-buffer (buffer-name current-args)
  "Create and configure aider buffer with BUFFER-NAME and CURRENT-ARGS."
  (apply #'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
  (with-current-buffer buffer-name
    (aider-comint-mode))
  (message "%s" (if current-args
                    (format "Running aider from %s, with args: %s.\nMay the AI force be with you!" 
                            default-directory (mapconcat #'identity current-args " "))
                  (format "Running aider from %s with no args provided.\nMay the AI force be with you!" 
                          default-directory))))

;;;###autoload
(defun aider-run-aider (&optional edit-args subtree-only)
  "Run \"aider\" in a comint buffer for interactive conversation.
With prefix argument (e.g., \\[universal-argument]), prompt to edit `aider-args` (EDIT-ARGS).
If SUBTREE-ONLY is non-nil, add '--subtree-only'.
Prompts for --subtree-only in dired/eshell/shell if needed."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb"))
    (if (comint-check-proc buffer-name)
        (message "Aider session already running in buffer: %s" buffer-name)
      (let ((current-args (aider--prepare-aider-args edit-args subtree-only)))
        (aider--create-aider-buffer buffer-name current-args)))
    (aider-switch-to-buffer)))

(defun aider-input-sender (proc string)
  "Handle multi-line inputs being sent to Aider.
PROC is the process to send the input to.
STRING is the input text to send.
Optional LOG, when non-nil, logs the command to the message area."
  (comint-simple-send proc (aider--process-message-if-multi-line string)))

(defun aider-core--command-completion ()
  "Provide auto completion for common commands in aider buffer.
When the current line starts with '/', this function returns a candidate list
of common commands."
  (save-excursion
    (let* ((line-start (line-beginning-position))
           (line-end (line-end-position))
           (line-str (buffer-substring-no-properties line-start line-end)))
      (when (string-match "^/\\(\\w*\\)" line-str)
        ;; Define the list of commands as a constant
        (let* ((beg (+ line-start (match-beginning 0)))
               (end (+ line-start (match-end 0)))
               (prefix (match-string 0 line-str))
               (candidates (seq-filter (lambda (cmd)
                                         (string-prefix-p prefix cmd))
                                       aider--command-list)))
          (when candidates
            (list beg end candidates :exclusive 'no)))))))

(defun aider-core--auto-trigger-command-completion ()
  "Automatically trigger command completion in aider buffer.
If the last character in the current line is '/', invoke `completion-at-point`."
  (when (and aider-auto-trigger-command-completion
             (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?/))
    (completion-at-point)))

(defun aider-core--auto-trigger-file-path-insertion ()
  "Automatically trigger file path insertion in aider buffer.
If the current line matches one of the file-related commands
followed by a space, and the cursor is at the end of the line,
invoke the appropriate file path insertion function."
  (when (and aider-auto-trigger-file-path-insertion
             (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?\s)  ; Check if last char is space
             (eolp))                 ; Check if cursor is at end of line
    (let ((line-content (buffer-substring-no-properties (line-beginning-position) (point))))
      (cond
       ;; Match commands like /add, /read-only followed by a space at the end of the line
       ((string-match-p "^[ \t]*\\(/add\\|/read-only\\) $" line-content)
        (aider-prompt-insert-add-file-path))
       ;; Match /drop command followed by a space at the end of the line
       ((string-match-p "^[ \t]*/drop $" line-content)
        (aider-prompt-insert-drop-file-path))))))

;;;###autoload
(defun aider-core-insert-prompt ()
  "Get user input via `aider-read-string` and insert it at point."
  (interactive)
  (let ((input (aider-read-string "Enter prompt: ")))
    (when input
      (insert input))))

(defun aider-core--auto-trigger-insert-prompt ()
  "Automatically trigger prompt insertion in aider buffer.
If the current line matches one of the commands (/ask, /code, /architect),
ends with exactly one space, and the cursor is at the end of the line,
invoke `aider-core-insert-prompt`."
  (when (and aider-auto-trigger-prompt
             (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?\s)  ; Check if last char is space
             (eolp))                 ; Check if cursor is at end of line
    (let ((line-content (buffer-substring-no-properties (line-beginning-position) (point))))
      (when (string-match-p "^[ \t]*\\(/ask\\|/code\\|/architect\\) $" line-content)
        (aider-core-insert-prompt)))))

(provide 'aider-core)

;;; aider-core.el ends here
