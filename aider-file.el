;;; aider-file.el --- File operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'dired)

;; Added helper function to get the relative or absolute path of the file
(defun aider--get-file-path (file-path)
  "Get the appropriate path for FILE-PATH.
If the file is in a git repository, return path relative to git root.
Otherwise, return the full local path."
  (let ((git-root (ignore-errors (magit-toplevel)))
        (full-path (expand-file-name file-path)))
    (if git-root
        ;; Get path relative to git root
        (file-relative-name full-path git-root)
      ;; Use full path for non-git files
      (file-local-name full-path))))

;; Added helper function to format file paths (process spaces)
(defun aider--format-file-path (file-path)
  "Format FILE-PATH for use in commands.
Add quotes if the path contains spaces."
  (if (string-match-p " " file-path)
      (format "\"%s\"" file-path)
    file-path))

;; Function to send "/add <current buffer file full path>" to corresponding aider buffer
;;;###autoload
(defun aider-add-current-file ()
  "Add current file to aider session."
  (interactive)
  (aider-action-current-file "/add"))

;;;###autoload
(defun aider-current-file-read-only ()
  "Add current file as read only to aider session."
  (interactive)
  (aider-action-current-file "/read-only"))

;;;###autoload
(defun aider-drop-current-file ()
  "Drop current file from aider session.
If current buffer is the aider comint session and cursor is on a file path,
drop that file instead."
  (interactive)
  (if (derived-mode-p 'aider-comint-mode)
      (if (aider--file-path-under-cursor-is-file)
          (aider--drop-file-under-cursor)
        (message "No file path found under cursor in aider session"))
    (aider-action-current-file "/drop")))

;;;###autoload
(defun aider-action-current-file (command-prefix)
  "Perform the COMMAND-PREFIX to aider session.
If the file is in a git repository, use path relative to git root."
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let* ((local-name (aider--get-file-path buffer-file-name))
           (formatted-path (aider--format-file-path local-name))
           (command (format "%s %s" command-prefix formatted-path)))
      ;; Use the shared helper function to send the command
      (aider--send-command command))))

;; New function to add files in all buffers in current emacs window
;;;###autoload
(defun aider-add-files-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Aider buffer.
If files are in a git repository, use paths relative to git root."
  (interactive)
  (let* ((files (mapcar (lambda (buffer)
                          (with-current-buffer buffer
                            (when buffer-file-name
                              (aider--get-file-path buffer-file-name))))
                        (mapcar #'window-buffer (window-list))))
         (files (delq nil files))
         (formatted-files (mapcar #'aider--format-file-path files)))
    (if files
        (let ((command (concat "/add " (mapconcat #'identity formatted-files " "))))
          (aider--send-command command nil))
      (message "No files found in the current window."))))

;; New function to show the last commit using magit
;;;###autoload
(defun aider-magit-show-last-commit-or-log (&optional log)
  "Show the last commit message using Magit.
With prefix argument (LOG), show commit log instead of single commit.
If Magit is not installed, report that it is required."
  (interactive "P")
  (if log
      (magit-log-current nil)
    (magit-show-commit "HEAD")))

;; Modified function to get command from user and send it based on selected region
;;;###autoload
(defun aider-undo-last-change ()
  "Undo the last change made by Aider."
  (interactive)
  (aider--send-command "/undo"))

;;; functions for dired related

;; New function to add multiple Dired marked files to Aider buffer
(defun aider--batch-add-dired-marked-files-with-command (command-prefix)
  "Add multiple Dired marked files to the Aider buffer with COMMAND-PREFIX.
COMMAND-PREFIX should be either \"/add\" or \"/read-only\".
Uses relative paths if files are in a git repository."
  (let ((absolute-files (dired-get-marked-files)))
    (if absolute-files
        (let* (;; Convert absolute paths to relative paths if needed
               (relative-files (mapcar #'aider--get-file-path absolute-files))
               ;; Format paths (e.g., add quotes for spaces)
               (formatted-files (mapcar #'aider--format-file-path relative-files))
               ;; Construct the command string
               (command (concat command-prefix " " (mapconcat #'identity formatted-files " "))))
          (aider--send-command command t))
      (message "No files marked in Dired."))))

;;;###autoload
(defun aider-batch-add-dired-marked-files ()
  "Add multiple Dired marked files with the \"/add\" command."
  (interactive)
  (aider--batch-add-dired-marked-files-with-command "/add"))

;;;###autoload
(defun aider-batch-add-dired-marked-files-read-only ()
  "Add multiple Dired marked files with the \"/read-only\" command."
  (interactive)
  (aider--batch-add-dired-marked-files-with-command "/read-only"))

;;;###autoload
(defun aider-add-current-file-or-dired-marked-files (&optional read-only)
  "Add files to Aider based on current context.
If current buffer is a Dired buffer, add all marked files.
Otherwise, add the current file.
With prefix argument READ-ONLY, add files as read-only."
  (interactive "P")
  (if read-only
      (aider-add-current-file-or-dired-marked-files-read-only)
    (if (derived-mode-p 'dired-mode)
        (aider-batch-add-dired-marked-files)
      (aider-add-current-file))))

;;;###autoload
(defun aider-add-current-file-or-dired-marked-files-read-only ()
  "Add files to Aider as read-only based on current context.
If current buffer is a Dired buffer, add all marked files as read-only.
Otherwise, add the current file as read-only."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (aider-batch-add-dired-marked-files-read-only)
    (aider-current-file-read-only)))

(defun aider-current-file-command-and-switch (prefix command)
  "Send COMMAND to the Aider buffer prefixed with PREFIX."
  (aider-add-current-file)
  (aider--send-command (concat prefix command) t)
  ;; (when (string-prefix-p "/architect" prefix)
  ;;   (message "Note: Aider v0.77.0 automatically accept changes for /architect command. If you want to review the code change before accepting it like before for many commands in aider.el, you can disable that flag with \"--no-auto-accept-architect\" in aider-args or .aider.conf.yml."))
  )

(defun aider--get-files-in-directory (directory suffixes)
  "Retrieve list of files in DIRECTORY matching SUFFIXES. SUFFIXES is a list of strings without dot."
  (let ((regex (concat "\\.\\(" (mapconcat #'regexp-quote suffixes "\\|") "\\)$")))
    (directory-files-recursively directory regex)))

(defun aider--file-path-under-cursor-is-file ()
  "Check if the file-path under cursor represents an existing file.
Works in both git repositories and regular directories."
  (when-let ((file-path (aider--get-full-file-path-at-point)))
    (let* ((git-root (ignore-errors (magit-toplevel)))
           (base-dir (or git-root default-directory))
           (potential-file-path (expand-file-name file-path base-dir)))
      (message potential-file-path)
      (file-exists-p potential-file-path))))

(defun aider--get-full-file-path-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'filename))
         (start (car bounds))
         (end (cdr bounds))
         (path (and bounds (buffer-substring-no-properties start end))))
    (when path
      (save-excursion
        (goto-char start)
        (while (and (not (bobp))
                    (looking-back "[^ \t\n\"'<>|]" 1))
          (backward-char))
        (let ((new-start (point)))
          (goto-char end)
          (while (and (not (eobp))
                      (looking-at "[^ \t\n\"'<>|]"))
            (forward-char))
          (setq path (buffer-substring-no-properties new-start (point))))))
    path))

(defun aider--drop-file-under-cursor ()
  "Drop the file under cursor from aider session.
Works in both git repositories and regular directories."
  (when-let ((file-path (aider--get-full-file-path-at-point)))
    (let* ((git-root (ignore-errors (magit-toplevel)))
           (base-dir (or git-root default-directory))
           (full-file-path (expand-file-name file-path base-dir))
           (file-path (aider--get-file-path full-file-path))
           (formatted-path (aider--format-file-path file-path))
           (command (format "/drop %s" formatted-path)))
      (aider--send-command command))))

(defun aider--filter-files-by-content-regex (files content-regex)
  "Filter FILES based on CONTENT-REGEX using grep.
Return a list of files whose content matches CONTENT-REGEX.
Return original FILES if CONTENT-REGEX is nil or empty.
Return nil if grep errors or no files match."
  (if (and files content-regex (not (string-empty-p content-regex)))
      (let* ((grep-args (list* "-l" "-E" content-regex files))
             (temp-buffer (generate-new-buffer " *grep-output*"))
             (exit-status
              (apply #'call-process "grep" nil (list temp-buffer nil) nil grep-args)))
        (prog1
            (if (or (zerop exit-status) (= exit-status 1)) ; 0 for match, 1 for no match
                (with-current-buffer temp-buffer
                  (if (zerop exit-status) ; Only parse if matches were found
                      (split-string (buffer-string) "\n" t)
                    nil)) ; No files matched
              (progn ; Handle grep errors (exit status > 1)
                (message "Error running grep: %s"
                         (with-current-buffer temp-buffer (buffer-string)))
                nil)) ; Return nil on error
          (kill-buffer temp-buffer)))
    files)) ; If no regex or no initial files, use original files

;;;###autoload
(defun aider-add-module (&optional read-only directory suffix-input content-regex)
  "Add all files from DIRECTORY with SUFFIX-INPUT to Aider session.
SUFFIX-INPUT is a comma-separated list of file suffixes without dots.
If CONTENT-REGEX is provided, only files whose content matches the regex are added.
With a prefix argument (C-u), files are added read-only (/read-only)."
  (interactive
   (list current-prefix-arg
         (read-directory-name "Module directory: " nil nil t)
         (read-string "File suffixes (comma-separated): "
                      "py,java,scala,el,sql")
         (read-string "Content regex (empty for none): " nil)))
  (let* ((cmd-prefix   (if read-only "/read-only" "/add"))
         (suffixes     (split-string suffix-input "\\s-*,\\s-*" t))
         (files-by-suffix (aider--get-files-in-directory directory suffixes))
         (filtered-files (aider--filter-files-by-content-regex files-by-suffix content-regex))
         (rel-paths    (if filtered-files (mapcar #'aider--get-file-path filtered-files) nil))
         (formatted    (if rel-paths (mapcar #'aider--format-file-path rel-paths) nil)))
    (if filtered-files
        (progn
          (aider--send-command (concat cmd-prefix " " (mapconcat #'identity formatted " ")) t)
         (message (if read-only
                      "Added %d files as read-only from %s%s"
                    "Added %d files from %s%s")
                  (length filtered-files) directory
                  (if (and content-regex (not (string-empty-p content-regex)))
                      (format " (matching content regex '%s')" content-regex)
                    "")))
      (message (format "No files found in %s with suffixes %s%s"
                       directory
                       suffix-input
                       (if (and content-regex (not (string-empty-p content-regex)))
                           (format " that also match content regex '%s'" content-regex)
                         ""))))))

(provide 'aider-file)

;;; aider-file.el ends here
