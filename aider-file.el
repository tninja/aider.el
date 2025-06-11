;;; aider-file.el --- File operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the Aider package.

;;; Code:

(require 'dired)
(require 'magit)
(require 'ffap)

(require 'aider-core)

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

(defun aider--get-full-expanded-file-path-at-point ()
  "Return the full, expanded file path found at point, or nil.
The path is expanded relative to the git repository root if available,
otherwise relative to `default-directory`."
  (when-let ((file-name-at-point (ffap-file-at-point)))
    (let* ((git-root (ignore-errors (magit-toplevel)))
           (base-dir (or git-root default-directory)))
      (expand-file-name file-name-at-point base-dir))))

(defun aider--file-path-under-cursor-is-file ()
  "Check if the file-path under cursor represents an existing file.
Works in both git repositories and regular directories."
  (when-let ((potential-file-path (aider--get-full-expanded-file-path-at-point)))
    (file-exists-p potential-file-path)))

(defun aider--drop-file-under-cursor ()
  "Drop the file under cursor from aider session.
Works in both git repositories and regular directories."
  (when-let ((full-file-path (aider--get-full-expanded-file-path-at-point)))
    (let* ((file-path-for-command (aider--get-file-path full-file-path))
           (formatted-path (aider--format-file-path file-path-for-command))
           (command (format "/drop %s" formatted-path)))
      (aider--send-command command))))

;;;###autoload
(defun aider-action-current-file (command-prefix)
  "Perform the COMMAND-PREFIX to aider session.
If the file is in a git repository, use path relative to git root."
  ;; Ensure the current buffer is associated with a file
  (when (aider--validate-buffer-file)
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
          (when (aider--send-command command t)
            (message "Successfully added %d file(s) to Aider" (length absolute-files))))
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
  "Send COMMAND to the Aider buffer prefixed with PREFIX.
Returns t if command was sent successfully, nil otherwise."
  (aider-add-current-file)
  (let ((result (aider--send-command (concat prefix command) t)))
    ;; (when (string-prefix-p "/architect" prefix)
    ;;   (message "Note: Aider v0.77.0 automatically accept changes for /architect command. If you want to review the code change before accepting it like before for many commands in aider.el, you can disable that flag with \"--no-auto-accept-architect\" in aider-args or .aider.conf.yml."))
    result))

(defun aider--get-files-in-directory (directory suffixes)
  "Retrieve list of files in DIRECTORY matching SUFFIXES. SUFFIXES is a list of strings without dot."
  (let ((regex (concat "\\.\\(" (mapconcat #'regexp-quote suffixes "\\|") "\\)$")))
    (directory-files-recursively directory regex)))

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
          (when (aider--send-command (concat cmd-prefix " " (mapconcat #'identity formatted " ")) t)
            (message (if read-only
                         "Added %d files as read-only from %s%s"
                       "Added %d files from %s%s")
                     (length filtered-files) directory
                     (if (and content-regex (not (string-empty-p content-regex)))
                         (format " (matching content regex '%s')" content-regex)
                       ""))))
      (message (format "No files found in %s with suffixes %s%s"
                       directory
                       suffix-input
                       (if (and content-regex (not (string-empty-p content-regex)))
                           (format " that also match content regex '%s'" content-regex)
                         ""))))))

;;;###autoload
(defun aider-expand-context ()
  "Add current file and its dependencies/dependents to aider session.
Given current buffer source code file, figure out the source code files that depend on it,
and the source code files it depends on."
  (interactive)
  (when (aider--validate-buffer-file)
    (let* ((current-file (buffer-file-name))
           (git-root (ignore-errors (magit-toplevel)))
           (search-root (or git-root default-directory))
           (dependencies (aider--find-file-dependencies current-file search-root))
           (dependents (aider--find-file-dependents current-file search-root))
           (all-files (delete-dups (append (list current-file) dependencies dependents))))
      (if (> (length all-files) 1)
          (progn
            (message "Dependencies: %s; Dependents: %s"
                     (mapconcat #'identity
                                (mapcar #'file-name-nondirectory dependencies)
                                ", ")
                     (mapconcat #'identity
                                (mapcar #'file-name-nondirectory dependents)
                                ", "))
            (let* ((relative-files (mapcar #'aider--get-file-path all-files))
                   (formatted-files (mapcar #'aider--format-file-path relative-files)))
              (dolist (file formatted-files)
                (aider--send-command (concat "/add " file) t))))
        (message "No additional dependencies or dependents found for current file")))))

(defun aider--find-file-dependencies (file-path search-root)
  "Find files that FILE-PATH depends on by searching for filenames mentioned in the file.
Returns list of absolute file paths."
  (let* ((dependencies '())
         (file-ext (file-name-extension file-path))
         (file-patterns (aider--get-source-file-patterns file-ext)))
    (when (file-exists-p file-path)
      ;; Get all source files with same extension in search root
      (let ((source-files (aider--find-files-by-patterns search-root file-patterns)))
        (dolist (source-file source-files)
          (let ((basename (file-name-sans-extension (file-name-nondirectory source-file))))
            ;; Check if current file content mentions this basename
            (when (and (not (string= source-file file-path))
                       (aider--file-mentions-basename file-path basename))
              (push source-file dependencies))))))
    (delete-dups dependencies)))

(defun aider--find-file-dependents (file-path search-root)
  "Find files that depend on FILE-PATH by searching for files that mention this file's basename.
Returns list of absolute file paths."
  (let* ((dependents '())
         (file-ext (file-name-extension file-path))
         (file-basename (file-name-sans-extension (file-name-nondirectory file-path)))
         (file-patterns (aider--get-source-file-patterns file-ext)))
    ;; Get all source files with same extension in search root
    (let ((source-files (aider--find-files-by-patterns search-root file-patterns)))
      (dolist (source-file source-files)
        ;; Check if this source file mentions our basename
        (when (and (not (string= source-file file-path))
                   (aider--file-mentions-basename source-file file-basename))
          (push source-file dependents))))
    (delete-dups dependents)))

(defun aider--get-source-file-patterns (file-ext)
  "Get file patterns for source files with same extension as FILE-EXT."
  (list (concat "*." file-ext)))

(defun aider--find-files-by-patterns (search-root patterns)
  "Find all files matching PATTERNS in SEARCH-ROOT."
  (let ((found-files '()))
    (dolist (pattern patterns)
      (let* ((cmd (list "find" search-root "-name" pattern "-type" "f"))
             (result (with-temp-buffer
                      (when (zerop (apply #'call-process (car cmd) nil t nil (cdr cmd)))
                        (split-string (buffer-string) "\n" t)))))
        (setq found-files (append found-files result))))
    (delete-dups (mapcar #'expand-file-name found-files))))

(defun aider--file-mentions-basename (file-path basename)
  "Check if FILE-PATH content mentions BASENAME with word boundaries."
  (when (and (file-exists-p file-path) (> (length basename) 1))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      ;; Search for basename with non-alphanumeric boundaries
      (re-search-forward (format "\\b%s\\b" (regexp-quote basename)) nil t))))

(provide 'aider-file)

;;; aider-file.el ends here
