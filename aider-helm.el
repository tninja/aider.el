;; optional helm based completion, need to be manually loaded when needed

(require 'helm)
(require 'cl-lib)  ; For `cl-subseq`

(defun helm-read-string-with-history (prompt history-file-name &optional initial-input)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string."
  ;; Load history from file
  (let* ((history-file (expand-file-name history-file-name user-emacs-directory))
         (history (when (file-exists-p history-file)
                    (with-temp-buffer
                      (insert-file-contents history-file)
                      (delete-dups (read (buffer-string))))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 history
                 :must-match nil
                 :name "Helm Read String"
                 :fuzzy t
                 :initial-input initial-input)))
    ;; Add to history if non-empty and save
    (unless (string-empty-p input)
      (push input history)
      (with-temp-file history-file
        (let ((history-entries (cl-subseq history
                                          0 (min (length history)
                                                 10000))))  ; Keep last 10000 entries
          (insert (prin1-to-string history-entries)))))
    input))

(defun aider-helm-read-string (prompt &optional initial-input)
  "Read a string with Helm completion for aider, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string."
  (helm-read-string-with-history prompt "aider-helm-read-string-history.el" initial-input))

(defalias 'aider-read-string 'aider-helm-read-string)

;; how to copy candidate to mini-buffer? C-c C-y: https://emacs.stackexchange.com/questions/47588/in-a-helm-prompt-how-do-i-copy-a-candidate-for-editing
