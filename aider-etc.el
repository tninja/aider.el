
;; New function to run `find-name-dired` from the Git repository root directory
;;;###autoload
(defun aider-repo-find-name-dired (pattern)
  "Run `find-name-dired` from the Git repository root directory with the given PATTERN."
  (interactive "sFind name (pattern): ")
  (let* ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel"))
         (repo-path (string-trim git-repo-path)))
    (if (string-match-p "fatal" repo-path)
        (message "Not in a git repository")
      (find-name-dired repo-path pattern))))

;;;###autoload
(defun aider-git-repo-root-dired ()
  "Open a Dired buffer at the root of the current Git repository."
  (interactive)
  (let ((git-repo-path (shell-command-to-string "git rev-parse --show-toplevel")))
    (if (string-match-p "fatal" git-repo-path)
        (message "The current buffer is not in a Git repository.")
      (let ((repo-path (string-trim git-repo-path)))
        (dired-other-window repo-path)))))
