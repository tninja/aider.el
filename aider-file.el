;;; aider-file.el --- File operations for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file provides file operation functionality for the Aider package.

;;; Code:

(require 'aider-core)
(require 'dired)

;; 新增辅助函数，用于获取文件的相对路径或绝对路径
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

;; 新增辅助函数，用于格式化文件路径（处理空格）
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
  "Drop current file from aider session."
  (interactive)
  (aider-action-current-file "/drop"))

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
  ;; (when (string-prefix-p "/architect" prefix)
  ;;   (message "Note: Aider v0.77.0 automatically accept changes for /architect command. If you want to review the code change before accepting it like before for many commands in aider.el, you can disable that flag with \"--no-auto-accept-architect\" in aider-args or .aider.conf.yml."))
  )

;;;###autoload
(defun aider-pull-or-review-diff-file ()
  "Review a diff file with Aider or generate one if not viewing a diff.
If current buffer is a .diff file, ask Aider to review it.
Otherwise, generate the diff."
  (interactive)
  (if (and buffer-file-name (string-match-p "\\.diff$" buffer-file-name))
      (let* ((file-name (file-name-nondirectory buffer-file-name))
             (init-prompt (format "Please review this diff file (%s), identify bug, and provide feedback on the changes" file-name))
             (prompt (aider-read-string "Enter diff review prompt: " init-prompt)))
        (aider-current-file-command-and-switch "/ask " prompt))
    (aider--magit-generate-feature-branch-diff-file)))

(defun aider--magit-generate-feature-branch-diff-file ()
  "Generate a diff file between base and feature branches or for a single commit.
The diff file will be named:
- <feature_branch>.<base_branch>.diff for branch comparison
- <hash>.diff for a single commit
and placed in the git root directory.
If input contains '..' it's treated as base..feature branch range.
If input looks like a commit hash, it generates diff for that single commit.
Otherwise, it's treated as base branch and diff is generated against HEAD."
  (interactive)
  (let* ((git-root (magit-toplevel))
         (raw-range (read-string "Branch range (base..feature), commit hash, or base branch: " "main"))
         (range (string-trim raw-range))
         ;; Check if it's a commit hash by verifying:
         ;; 1. It doesn't contain '..'
         ;; 2. It's not a branch name
         ;; 3. It can be verified as a valid revision
         ;; 4. It matches the format of a commit hash (at least 7 hex chars)
         (is-commit-hash (and (not (string-match-p "\\.\\." range))
                              (not (magit-branch-p range))
                              (magit-rev-verify range)
                              (string-match-p "^[0-9a-f]\\{7,\\}$" range)))
         (branches (cond
                    (is-commit-hash
                     (list (concat range "^") range))
                    ((string-match-p "\\.\\." range)
                     (split-string range "\\.\\."))
                    (t
                     (list range "HEAD"))))
         (base-branch (car branches))
         (feature-branch (cadr branches))
         (diff-file (if is-commit-hash
                         (concat git-root range ".diff")
                       (concat git-root base-branch "." feature-branch ".diff"))))
    ;; Verify we're in a git repo
    (unless git-root
      (user-error "Not in a git repository"))
    ;; Display message about what we're doing
    (cond
     (is-commit-hash
      (message "Generating diff for single commit: %s" range))
     ((string-match-p "\\.\\." range)
      (message "Generating diff between branches: %s..%s" base-branch feature-branch))
     (t
      (message "Generating diff between %s and HEAD" base-branch)))
    ;; Check if repo is clean
    (when (magit-anything-modified-p)
      (message "Repository has uncommitted changes. You might want to commit or stash them first")
      (sleep-for 1))
    ;; Store current branch to return to it
    (let ((original-branch (magit-get-current-branch)))
      ;; Git operations
      (unless is-commit-hash
        ;; Only do branch operations for branch comparisons
        (message "Checking out %s..." base-branch)
        (magit-run-git "checkout" base-branch) ; Switch to base branch first
        (magit-run-git "checkout" original-branch) ; Return to original branch
        (when (not (string= feature-branch "HEAD"))
          (message "Checking out %s..." feature-branch)
          (magit-run-git "checkout" feature-branch)
          (magit-run-git "checkout" original-branch))) ; Return to original branch
      ;; Generate diff file
      (message "Generating diff file...")
      (magit-run-git "diff" (concat base-branch ".." feature-branch)
                     (concat "--output=" diff-file))
      ;; Open diff file
      (find-file diff-file)
      (message "Generated diff file: %s" diff-file))))

;;;###autoload
(defun aider-open-history ()
  "Open the Aider history file (.aider.chat-history.md under repo git root).
If the history file does not exist, notify the user."
  (interactive)
  (let ((git-root (magit-toplevel)))
    (unless git-root
      (user-error "Not inside a git repository"))
    (let ((history-file (expand-file-name ".aider.chat.history.md" git-root)))
      (if (file-exists-p history-file)
          (find-file-other-window history-file)
        (message "History file does not exist: %s" history-file)))))

(provide 'aider-file)

;;; aider-file.el ends here
