;;; aider.el --- Aider package for interactive conversation with aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (transient "0.3.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el

;;; Commentary:
;; This package provides an interactive interface to communicate with https://github.com/paul-gauthier/aider.

;;; Code:

(require 'comint)
(require 'transient)
(require 'which-func)
(require 'dired)

(defgroup aider nil
  "Customization group for the Aider package."
  :prefix "aider-"
  :group 'convenience)

(defcustom aider-program "aider"
  "The name or path of the aider program."
  :type 'string
  :group 'aider)

(defcustom aider-args '("--model" "gpt-4o-mini")
  "Arguments to pass to the Aider command."
  :type '(repeat string)
  :group 'aider)

(defun aider-plain-read-string (prompt &optional initial-input)
  "Read a string from the user with PROMPT and optional INITIAL-INPUT.
This function can be customized or redefined by the user."
  (let* ((input (read-string prompt initial-input))
         (processed-input (replace-regexp-in-string "\n" "\\\\n" input)))
    (concat processed-input "\n")))

(defalias 'aider-read-string 'aider-plain-read-string)

;; Transient menu for Aider commands
;; The instruction in the autoload comment is needed, see
;; https://github.com/magit/transient/issues/280.
;;;###autoload (autoload 'aider-transient-menu "aider" "Transient menu for Aider commands." t)
(transient-define-prefix aider-transient-menu ()
  "Transient menu for Aider commands."
  ["Aider: AI pair programming"
   ["Aider process"
    ("a" "Run Aider" aider-run-aider)
    ("z" "Switch to Aider Buffer" aider-switch-to-buffer)
    ("l" "Clear Aider" aider-clear)
    ("s" "Reset Aider" aider-reset)
    ]
   ["Add file to aider"
    ("f" "Add Current File" aider-add-current-file)
    ("F" "Find Files in the Git Repo" aider-repo-find-name-dired)
    ("b" "Batch Add Dired Marked Files" aider-batch-add-dired-marked-files)
    ("R" "Open Git Repo Root Dired" aider-git-repo-root-dired)
    ]
   ["Code change"
    ("c" "Code Change" aider-code-change)
    ("r" "Region Code Refactor" aider-region-refactor)
    ("u" "Undo Last Change" aider-undo-last-change)
    ]
   ["Discussion"
    ("q" "Ask Question" aider-ask-question)
    ("t" "Architect Discussion" aider-architect-discussion)
    ("d" "Debug Exception" aider-debug-exception)
    ]
   ["Other"
    ("g" "General Command" aider-general-command)
    ("h" "Help" aider-help) ;; Menu item for help command
    ]
   ])

;; Removed the default key binding
;; (global-set-key (kbd "C-c a") 'aider-transient-menu)

(defun aider-buffer-name-from-git-repo-path (git-repo-path home-path)
  "Generate the Aider buffer name based on the GIT-REPO-PATH and HOME-PATH.
If not in a git repository, an error is raised."
  (let* ((relative-path (substring git-repo-path (length home-path))))
    (format "*aider:%s*" (concat "~" (replace-regexp-in-string "\n" "" relative-path)))))

(defun aider-buffer-name ()
  "Generate the Aider buffer name based on the path from the home folder to the git repo of the current active buffer using a git command.
If not in a git repository, an error is raised."
  (let* ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel"))
         (home-path (expand-file-name "~")))
    (if (string-match-p "fatal" git-repo-path)
        (error "Not in a git repository")
      (aider-buffer-name-from-git-repo-path git-repo-path home-path))))

;;;###autoload
(defun aider-run-aider ()
  "Create a comint-based buffer and run \"aider\" for interactive conversation."
  (interactive)
  (let* ((buffer-name (aider-buffer-name)))
    ;; Check if the buffer already has a running process
    (unless (comint-check-proc buffer-name)
      ;; Create a new comint buffer and start the process
      (apply 'make-comint-in-buffer "aider" buffer-name aider-program nil aider-args)
      ;; Optionally, you can set the mode or add hooks here
      (with-current-buffer buffer-name
        (comint-mode)
        ))
    ;; Switch to the buffer
    (pop-to-buffer buffer-name)))

;; Function to switch to the Aider buffer
(defun aider-switch-to-buffer ()
  "Switch to the Aider buffer."
  (interactive)
  (let ((buffer (get-buffer (aider-buffer-name))))
    (if buffer
        (pop-to-buffer buffer)
      (message "Aider buffer '%s' does not exist." (aider-buffer-name)))))

;; Function to reset the Aider buffer
(defun aider-clear ()
  "Send the command \"/clear\" to the Aider buffer."
  (interactive)
  (aider--send-command "/clear"))

(defun aider-reset ()
  "Send the command \"/reset\" to the Aider buffer."
  (interactive)
  (aider--send-command "/reset"))

;; Shared helper function to send commands to corresponding aider buffer
(defun aider--send-command (command &optional switch-to-buffer)
  "Send COMMAND to the corresponding aider comint buffer after performing necessary checks.
COMMAND should be a string representing the command to send."
  ;; Check if the corresponding aider buffer exists
  (if-let ((aider-buffer (get-buffer (aider-buffer-name))))
      (let ((aider-process (get-buffer-process aider-buffer)))
        ;; Check if the corresponding aider buffer has an active process
        (if (and aider-process (comint-check-proc aider-buffer))
            (progn
              ;; Ensure the command ends with a newline
              (unless (string-suffix-p "\n" command)
                (setq command (concat command "\n")))
              ;; Send the command to the aider process
              (comint-send-string aider-buffer command)
              ;; Provide feedback to the user
              (message "Sent command to aider buffer: %s" (string-trim command))
              (when switch-to-buffer
                (aider-switch-to-buffer)))
          (message "No active process found in buffer %s." (aider-buffer-name))))
    (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))))

;; Function to send "/add <current buffer file full path>" to corresponding aider buffer
(defun aider-add-current-file ()
  "Send the command \"/add <current buffer file full path>\" to the corresponding aider comint buffer."
  (interactive)
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let ((command (format "/add %s" (expand-file-name buffer-file-name))))
      ;; Use the shared helper function to send the command
      (aider--send-command command))))

;; Function to send a custom command to corresponding aider buffer
(defun aider-general-command ()
  "Prompt the user to input COMMAND and send it to the corresponding aider comint buffer."
  (interactive)
  (let ((command (aider-read-string "Enter command to send to aider: ")))
    ;; Use the shared helper function to send the command
    (aider-add-current-file)
    (aider--send-command command t)))

;; New function to get command from user and send it prefixed with "/code "
(defun aider-code-change ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/code \"."
  (interactive)
  (let ((command (aider-read-string "Enter code change requirement: ")))
    (aider-send-command-with-prefix "/code " command)))

;; New function to get command from user and send it prefixed with "/ask "
(defun aider-ask-question ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/ask \"."
  (interactive)
  (let ((command (aider-read-string "Enter question to ask: ")))
    (aider-send-command-with-prefix "/ask " command)))

;; New function to get command from user and send it prefixed with "/help "
(defun aider-help ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/help \"."
  (interactive)
  (let ((command (aider-read-string "Enter help question: ")))
    (aider-send-command-with-prefix "/help " command)))

;; New function to get command from user and send it prefixed with "/architect "
(defun aider-architect-discussion ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/architect \"."
  (interactive)
  (let ((command (aider-read-string "Enter architect discussion question: ")))
    (aider-send-command-with-prefix "/architect " command)))

;; New function to get command from user and send it prefixed with "/ask ", might be tough for AI at this moment
(defun aider-debug-exception ()
  "Prompt the user for a command and send it to the corresponding aider comint buffer prefixed with \"/debug \",
replacing all newline characters except for the one at the end."
  (interactive)
  (let ((command (aider-plain-read-string "Enter exception, can be multiple lines: ")))
    (aider--send-command (concat "/ask Investigate the following exception, with current added files as context: " command) t)))

;; Modified function to get command from user and send it based on selected region
(defun aider-undo-last-change ()
  "Undo the last change made by Aider."
  (interactive)
  (aider--send-command "/undo"))

(defun aider-region-refactor-generate-command (region-text function-name user-command)
  "Generate the command string based on REGION-TEXT, FUNCTION-NAME, and USER-COMMAND."
  (let ((processed-region-text (replace-regexp-in-string "\n" "\\\\n" region-text)))
    (if function-name
        (format "/architect \"in function %s, for the following code block, %s: %s\"\n"
                function-name user-command processed-region-text)
      (format "/architect \"for the following code block, %s: %s\"\n"
              user-command processed-region-text))))

(defun aider-region-refactor ()
  "Get a command from the user and send it to the corresponding aider comint buffer based on the selected region.
The command will be formatted as \"/architect \" followed by the user command and the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (function-name (which-function))
             (user-command (aider-read-string "Enter your refactor instruction: "))
             (command (aider-region-refactor-generate-command region-text function-name user-command)))
        (aider-add-current-file)
        (aider--send-command command t)
        )
    (message "No region selected.")))

(defun aider-send-command-with-prefix (prefix command)
  "Send COMMAND to the Aider buffer prefixed with PREFIX."
  (aider-add-current-file)
  (aider--send-command (concat prefix command) t))

;;; functions for dired related

;; New function to add multiple Dired marked files to Aider buffer
(defun aider-batch-add-dired-marked-files ()
  "Add multiple Dired marked files to the Aider buffer with the \"/add\" command."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (let ((command (concat "/add " (mapconcat 'expand-file-name files " "))))
          (aider--send-command command t))
      (message "No files marked in Dired."))))

;; New function to run `find-name-dired` from the Git repository root directory
(defun aider-repo-find-name-dired (pattern)
  "Run `find-name-dired` from the Git repository root directory with the given PATTERN."
  (interactive "sFind name (pattern): ")
  (let* ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel"))
         (repo-path (string-trim git-repo-path)))
    (if (string-match-p "fatal" repo-path)
        (message "Not in a git repository")
      (find-name-dired repo-path pattern))))

(defun aider-git-repo-root-dired ()
  "Open a Dired buffer at the root of the current Git repository."
  (interactive)
  (let ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel")))
    (if (string-match-p "fatal" git-repo-path)
        (message "The current buffer is not in a Git repository.")
      (let ((repo-path (string-trim git-repo-path)))
        (dired-other-window repo-path)))))

;;; functions for .aider file related

;; New function to send "<line under cursor>" to the Aider buffer
(defun aider-send-line-under-cursor ()
  "Send the command \"ask <line under cursor>\" to the Aider buffer."
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (aider--send-command (string-trim line) t)))

;;; New function to send the current paragraph to the Aider buffer
(defun aider-send-paragraph ()
  "Send the current paragraph to the Aider buffer."
  (interactive)
  (let ((paragraph (buffer-substring-no-properties
                    (save-excursion
                      (backward-paragraph)
                      (point))
                    (save-excursion
                      (forward-paragraph)
                      (point)))))
    (aider--send-command (string-trim paragraph) t)))

(defun aider-mode-setup ()
  "Setup key bindings for Aider mode."
  (local-set-key (kbd "C-c C-n") 'aider-send-line-under-cursor)
  (local-set-key (kbd "C-c C-c") 'aider-send-paragraph))

(add-hook 'aider-mode-hook 'aider-mode-setup)

(define-derived-mode aider-mode fundamental-mode "Aider"
  "Major mode for editing Aider files."
  ;; Add any additional setup for aider-mode here
  )

(add-to-list 'auto-mode-alist '("\\.aider\\'" . aider-mode))

(provide 'aider)

;;; aider.el ends here
