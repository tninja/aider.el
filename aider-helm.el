;; optional helm based completion, need to be manually loaded when needed

;; helm based aider input

(require 'helm)
(require 'cl-lib)  ; For `cl-subseq`

(defvar aider-helm-read-string-history nil
  "History list for `aider-helm-read-string` inputs.")

(defvar aider-helm-read-string-history-file
  (expand-file-name "aider-helm-read-string-history.el" user-emacs-directory)
  "File to save `aider-helm-read-string-history`.")

(defvar aider-helm-read-string-history-max 1000
  "Maximum number of entries to keep in `aider-helm-read-string-history`.")

(defun save-aider-helm-read-string-history ()
  "Save `aider-helm-read-string-history` to a file."
  ;; Trim history to maximum size
  (setq aider-helm-read-string-history
        (cl-subseq aider-helm-read-string-history
                   0 (min (length aider-helm-read-string-history)
                          aider-helm-read-string-history-max)))
  ;; Save to file
  (with-temp-file aider-helm-read-string-history-file
    (insert (prin1-to-string aider-helm-read-string-history))))

(defun load-aider-helm-read-string-history ()
  "Load `aider-helm-read-string-history` from a file."
  (when (file-exists-p aider-helm-read-string-history-file)
    (with-temp-buffer
      (insert-file-contents aider-helm-read-string-history-file)
      (setq aider-helm-read-string-history (read (buffer-string))))))

(add-hook 'kill-emacs-hook 'save-aider-helm-read-string-history)
(load-aider-helm-read-string-history)

(defun aider-helm-read-string (prompt &optional initial-input default-value)
  "Read a string with Helm completion, showing historical inputs."
  (let* ((input (helm-comp-read
                 prompt
                 aider-helm-read-string-history
                 :must-match nil
                 :name "Helm Read String"
                 :history 'aider-helm-read-string-history
                 :initial-input initial-input
                 :default default-value
                 :fuzzy t)))  ;; fuzzy match
    ;; Add input to history if it's not empty
    (unless (string-empty-p input)
      (add-to-history 'aider-helm-read-string-history input))
    input))

(defalias 'aider-read-string 'aider-helm-read-string)

;; how to copy candidate to mini-buffer? C-c C-y: https://emacs.stackexchange.com/questions/47588/in-a-helm-prompt-how-do-i-copy-a-candidate-for-editing
