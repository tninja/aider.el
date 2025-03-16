;;; aider-file.el --- File operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'dired)

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
  "Drop current file from aider session."
  (interactive)
  (aider-action-current-file "/drop"))

;;;###autoload
(defun aider-action-current-file (command-prefix)
  "Perform the COMMAND-PREFIX to aider session."
  ;; Ensure the current buffer is associated with a file
  (if (not buffer-file-name)
      (message "Current buffer is not associated with a file.")
    (let* ((local-name (file-local-name
                       (expand-file-name buffer-file-name)))
           (formatted-path (if (string-match-p " " local-name)
                             (format "\"%s\"" local-name)
                           local-name))
           (command (format "%s %s" command-prefix formatted-path)))
      ;; Use the shared helper function to send the command
      (aider--send-command command))))

;; New function to add files in all buffers in current emacs window
;;;###autoload
(defun aider-add-files-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Aider buffer."
  (interactive)
  (let ((files (mapcar (lambda (buffer)
                         (with-current-buffer buffer
                           (when buffer-file-name
                             (expand-file-name buffer-file-name))))
                       (mapcar #'window-buffer (window-list)))))
    (setq files (delq nil files))
    (if files
        (let ((command (concat "/add " (mapconcat #'identity files " "))))
          (aider--send-command command nil))
      (message "No files found in the current window."))))

;; New function to add all files with same suffix as current file under current directory
;;;###autoload
(defun aider-add-same-type-files-under-dir ()
  "Add all files with same suffix as current file under current directory to Aider.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files (directory-files dir t
                                   (concat "\\." current-suffix "$")
                                   t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files) max-files current-suffix)
        (let ((command (concat "/add " (mapconcat #'identity files " "))))
          (aider--send-command command t))
        (message "Added %d files with suffix .%s"
                 (length files) current-suffix)))))

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
COMMAND-PREFIX should be either \"/add\" or \"/read-only\"."
  (let ((files (dired-get-marked-files)))
    (if files
        (let ((command (concat command-prefix " " (mapconcat #'expand-file-name files " "))))
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
  (when (string-prefix-p "/architect" prefix)
    (message "Note: Aider v0.77.0 automatically accept changes for /architect command. If you want to review the code change before accepting it like before for many commands in aider.el, you can disable that flag with \"--no-auto-accept-architect\" in aider-args or .aider.conf.yml.")))

;;;###autoload
(defun aider-pull-or-review-diff-file ()
  "Review a diff file with Aider or generate one if not viewing a diff.
If current buffer is a .diff file, ask Aider to review it.
Otherwise, call `aider--magit-generate-feature-branch-diff-file` to generate a diff."
  (interactive)
  (if (and buffer-file-name (string-match-p "\\.diff$" buffer-file-name))
      (let* ((file-name (file-name-nondirectory buffer-file-name))
             (init-prompt (format "Please review this diff file (%s), identify bug, and provide feedback on the changes" 
                             (file-name-nondirectory buffer-file-name)))
             (prompt (aider-read-string "Enter diff review prompt: " init-prompt)))
        (aider-current-file-command-and-switch "/ask " prompt))
    (aider--magit-generate-feature-branch-diff-file)))

(defun aider--magit-generate-feature-branch-diff-file ()
  "Generate a diff file between base and feature branches.
The diff file will be named <feature_branch>.<base_branch>.diff
and placed in the git root directory.
If input doesn't contain '..' it's treated as base branch and diff is generated against HEAD."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (raw-range (read-string "Branch range (base..feature or just base): " "main"))
         (range (string-trim raw-range))
         (branches (if (string-match-p "\\.\\." range)
                       (split-string range "\\.\\.")
                     (list range "HEAD")))
         (base-branch (car branches))
         (feature-branch (or (cadr branches) "HEAD"))
         (diff-file (concat git-root feature-branch "." base-branch ".diff")))
    ;; Verify we're in a git repo
    (unless git-root
      (user-error "Not in a git repository"))
    ;; ;; Check if repo is clean
    (when (magit-anything-modified-p)
      (message "Repository has uncommitted changes. You might want to commit or stash them first")
      (sleep-for 1))
    ;; Store current branch to return to it
    (let ((original-branch (magit-get-current-branch)))
      ;; Git operations
      (magit-run-git "checkout" base-branch) ; Switch to base branch first
      (magit-run-git "pull")      ; Pull latest changes on base branch
      (magit-run-git "checkout" original-branch) ; Return to original branch
      (when (not (string= feature-branch "HEAD"))
        (magit-run-git "checkout" feature-branch)
        (magit-run-git "checkout" original-branch)) ; Return to original branch
      ;; Generate diff file
      (magit-run-git "diff" (concat base-branch ".." feature-branch)
                     (concat "--output=" diff-file))
      ;; Open diff file
      (find-file diff-file)
      (message "Generated diff file: %s" diff-file))))

(provide 'aider-file)

;;; aider-file.el ends here
