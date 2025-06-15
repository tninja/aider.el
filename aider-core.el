;;; aider-core.el --- Core functionality for Aider -*- lexical-binding: t; -*-
;;; Commentary: moved markdown highlighting and advice to aider-comint-markdown.el

(require 'comint)
(require 'magit)
(require 'aider-comint-markdown)  ;; for markdown advice & highlighting
(require 'savehist)
(require 'markdown-mode)

(require 'aider-utils)

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

(defvar aider--switch-to-buffer-other-frame nil
  "Boolean controlling Aider buffer display behavior.
When non-nil, open Aider buffer in a new frame.
When nil, use standard `display-buffer' behavior.")

;; Removed: moved to aider-comint-markdown.el

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
  (aider--send-command "go ahead" t))

(defconst aider--command-list
  '("/add" "/architect" "/ask" "/code" "/reset" "/undo" "/lint" "/read-only"
    "/drop" "/copy" "/copy-context" "/clear" "/commit" "/exit" "/quit"
    "/paste" "/help" "/chat-mode" "/diff" "/editor" "/git"
    "/load" "/ls" "/map" "/map-refresh" "/think-tokens" "/tokens"
    "/model" "/editor-model" "/weak-model" "/models" "/reasoning-effort"
    "/multiline-mode" "/report" "/run" "/save" "/settings" "/test"
    "/voice" "/web")
  "A list of common Aider commands for completion.")

;; Removed: moved to aide-comint-markdown.el

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
  ;; Add command completion hooks
  (add-hook 'completion-at-point-functions #'aider-core--command-completion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-command-completion nil t)
  ;; Automatically trigger file path insertion for file-related commands
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-file-path-insertion nil t)
  (add-hook 'post-self-insert-hook #'aider-core--auto-trigger-insert-prompt nil t)
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

;; Move markdown highlighting setup to separate module
(add-hook 'aider-comint-mode-hook #'aider--apply-markdown-highlighting)

;; History functions have been moved to aider-utils.el.

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

;; Function `aider--process-message-if-multi-line` moved to aider-utils.el.

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
    (let* ((command (replace-regexp-in-string "\\`[\n\r]+" "" command))   ;; Remove leading newlines
           (command (replace-regexp-in-string "[\n\r]+\\'" "" command)) ;; Remove trailing newlines
           (command (aider--process-message-if-multi-line command))
           (aider-process (get-buffer-process aider-buffer)))
      ;; Check if the corresponding aider buffer has an active process
      (if (and aider-process (comint-check-proc aider-buffer))
          (progn
            ;; Send the command to the aider process
            (aider--comint-send-string-syntax-highlight aider-buffer command)
            ;; Provide feedback to the user
            (when log
              (message "Sent command to aider buffer: %s" (string-trim command)))
            (when switch-to-buffer
              (aider-switch-to-buffer))
            (sleep-for 0.2)
            t) ; Return t for success
        (progn
          (message "No active process found in buffer %s." (aider-buffer-name))
          nil))) ; Return nil for failure
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
(defun aider-run-aider (&optional edit-args subtree-only)
  "Run \"aider\" in a comint buffer for interactive conversation.
With prefix argument (e.g., \\[universal-argument]), prompt to edit `aider-args` (EDIT-ARGS).
If SUBTREE-ONLY is non-nil, add '--subtree-only'.
Prompts for --subtree-only in dired/eshell/shell if needed."
  (interactive "P")
  (let* ((buffer-name (aider-buffer-name))
         (comint-terminfo-terminal "dumb")
         (current-args (if edit-args
                           (split-string
                            (read-string "Edit aider arguments: "
                                         (mapconcat #'identity aider-args " ")))
                         aider-args)))
    (if (comint-check-proc buffer-name)
        (message "Aider session already running in buffer: %s" buffer-name)
      (progn
        ;; Handle --subtree-only prompting for special modes
        (setq current-args (aider--maybe-prompt-subtree-only-for-special-modes current-args))
        ;; Add --subtree-only if the parameter is set and it's not already present
        (when (and subtree-only (not (member "--subtree-only" current-args)))
          (setq current-args (append current-args '("--subtree-only")))
          (message "Adding --subtree-only argument as requested."))
        (apply #'make-comint-in-buffer "aider" buffer-name aider-program nil current-args)
        (with-current-buffer buffer-name
          (aider-comint-mode))
        (message "%s" (if current-args
                          (format "Running aider from %s, with args: %s.\nMay the AI force be with you!" default-directory (mapconcat #'identity current-args " "))
                        (format "Running aider from %s with no args provided.\nMay the AI force be with you!" default-directory)))))
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
  (when (and (not (minibufferp))
             (not (bolp))
             (eq (char-before) ?/))
    (completion-at-point)))

(defun aider-core--auto-trigger-file-path-insertion ()
  "Automatically trigger file path insertion in aider buffer.
If the current line matches one of the file-related commands
followed by a space, and the cursor is at the end of the line,
invoke the appropriate file path insertion function."
  (when (and (not (minibufferp))
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

;; Validator functions moved to aider-utils.el.

(provide 'aider-core)

;;; aider-core.el ends here
