;;; aider.el --- Aider package for interactive conversation with OpenAI -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (transient "0.3.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el

;;; Commentary:
;; This package provides an interactive interface to communicate with https://github.com/paul-gauthier/aider.

;;; Code:

(require 'transient)

(defgroup aider nil
  "Customization group for the Aider package."
  :prefix "aider-"
  :group 'convenience)

(defcustom aider-args '("--model" "gpt-4o-mini")
  "Arguments to pass to the Aider command."
  :type '(repeat string)
  :group 'aider)

;; Define the keymap for Aider commands
(defvar aider-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'aider-run-aider)         
    (define-key map (kbd "z") 'aider-switch-to-buffer)
    (define-key map (kbd "s") 'aider-add-current-file)
    (define-key map (kbd "c") 'aider-send-command) 
    (define-key map (kbd "q") 'aider-ask-question)   
    (define-key map (kbd "t") 'aider-architect-command)
    (define-key map (kbd "e") 'aider-region-code-command)
    (define-key map (kbd "u") 'aider-undo-last-change)
    (define-key map (kbd "r") 'aider-reset-command)   
    (define-key map (kbd "m") 'aider-transient-menu) 
    map)
  "Global keymap for Aider commands.")

;; Activate the global keymap
(define-key global-map (kbd "C-c a") aider-global-map)

;; Transient menu for Aider commands
(transient-define-prefix aider-transient-menu ()
  "Transient menu for Aider commands."
  ["Aider Menu"
   ["Aider process"
    ("a" "Run Aider" aider-run-aider)
    ("s" "Add Current File" aider-add-current-file)
    ("z" "Switch to Aider Buffer" aider-switch-to-buffer)
    ]
   ["Code change"
    ("e" "Region Code Change" aider-region-code-command)
    ("u" "Undo Last Change" aider-undo-last-change) ;; Menu item for undo last change
    ]
   ["Discussion"
    ("q" "Ask Question" aider-ask-question)
    ("t" "Architect Discussion" aider-architect-command)
    ]
   ["Other"
    ("r" "Reset Aider" aider-reset-command) ;; Menu item for reset command
    ("c" "General Command" aider-send-command)
    ]
   ])

(defun aider-buffer-name ()
  "Generate the Aider buffer name based on the path from the home folder to the git repo of the current active buffer using a git command."
  (let* ((buffer-file-path (buffer-file-name))
         (git-repo-path (shell-command-to-string "git rev-parse --show-toplevel"))
         (home-path (expand-file-name "~"))
         (relative-path (substring git-repo-path (length home-path))))
    (format "*aider:%s*" (concat "~" (replace-regexp-in-string "\n" "" relative-path)))))

(defun aider-run-aider ()
  "Create a comint-based buffer and run 'aider' for interactive conversation."
  (interactive)
  (let* ((buffer-name (aider-buffer-name))
         (command "aider"))
    ;; Check if the buffer already has a running process
    (unless (comint-check-proc buffer-name)
      ;; Create a new comint buffer and start the process
      (apply 'make-comint-in-buffer "aider" buffer-name command nil aider-args)
      ;; Optionally, you can set the mode or add hooks here
      (with-current-buffer buffer-name
        (comint-mode)
        ))
    ;; Switch to the buffer
    (aider-add-current-file)
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
(defun aider-reset-command ()
  "Send the command \"/reset\" to the Aider buffer."
  (interactive)
  (aider--send-command "/reset"))

;; Shared helper function to send commands to *aider* buffer
(defun aider--send-command (command)
  "Send COMMAND to the *aider* comint buffer after performing necessary checks.
COMMAND should be a string representing the command to send."
  ;; Check if the *aider* buffer exists
  (if-let ((aider-buffer (get-buffer (aider-buffer-name))))
      (let ((aider-process (get-buffer-process aider-buffer)))
        ;; Check if the *aider* buffer has an active process
        (if (and aider-process (comint-check-proc aider-buffer))
            (progn
              ;; Ensure the command ends with a newline
              (unless (string-suffix-p "\n" command)
                (setq command (concat command "\n")))
              ;; Send the command to the aider process
              (comint-send-string aider-buffer command)
              ;; Provide feedback to the user
              (message "Sent command to *aider*: %s" (string-trim command))
              (aider-switch-to-buffer))
          (message "No active process found in buffer %s." (aider-buffer-name))))
    (message "Buffer %s does not exist. Please start 'aider' first." (aider-buffer-name))))

;; Function to send "/add <current buffer file full path>" to *aider* buffer
(defun aider-add-current-file ()
  "Send the command \"/add <current buffer file full path>\" to the *aider* comint buffer."
  (interactive)
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let ((file-path (expand-file-name buffer-file-name)))
      ;; Construct the command
      (let ((command (format "/add %s" file-path)))
        ;; Use the shared helper function to send the command
        (aider--send-command command)))))

;; Function to send a custom command to *aider* buffer
(defun aider-send-command (command)
  "Prompt the user to input COMMAND and send it to the *aider* comint buffer.
COMMAND is a string representing the command to send."
  (interactive
   (list (read-string "Enter command to send to aider: ")))
  ;; Use the shared helper function to send the command
  (aider--send-command command))

;; New function to get command from user and send it prefixed with "/code "
(defun aider-code-command ()
  "Prompt the user for a command and send it to the *aider* comint buffer prefixed with \"/code \"."
  (interactive)
  (let ((command (read-string "Enter code command: ")))
    (aider--send-command (concat "/code " command))))

;; New function to get command from user and send it prefixed with "/ask "
(defun aider-ask-question ()
  "Prompt the user for a command and send it to the *aider* comint buffer prefixed with \"/ask \"."
  (interactive)
  (let ((command (read-string "Enter ask command: ")))
    (aider--send-command (concat "/ask " command))))

;; New function to get command from user and send it prefixed with "/architect "
(defun aider-architect-command ()
  "Prompt the user for a command and send it to the *aider* comint buffer prefixed with \"/architect \"."
  (interactive)
  (let ((command (read-string "Enter architect command: ")))
    (aider--send-command (concat "/architect " command))))

;; Modified function to get command from user and send it based on selected region
(defun aider-undo-last-change ()
  "Undo the last change made by Aider."
  (interactive)
  (aider--send-command "/undo"))

(defun aider-region-code-command ()
  "Get a command from the user and send it to the *aider* comint buffer based on the selected region.
The command will be formatted as \"/code \" followed by the user command and the text from the selected region."
  (interactive)
  (if (use-region-p)
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))
             (processed-region-text (replace-regexp-in-string "\n" "\\\\n" region-text))
             (user-command (read-string "Enter your command: "))
             (function-name (or (which-function) "unknown function"))
             (command (format "/code \"in function %s, for the following code block, %s: %s\""
                              function-name user-command processed-region-text)))
        (aider--send-command command))
    (message "No region selected.")))

(provide 'aider)

;;; aider.el ends here
