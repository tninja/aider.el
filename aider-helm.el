;;; aider-helm.el --- Helm completion for aider.el -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>
;; Keywords: convenience, tools
;; URL: https://github.com/tninja/aider.el
;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Optional Helm completion interface for aider.el
;; To use this, ensure both aider.el and helm are installed.

;;; Code:

(require 'cl-lib)  ; For `cl-subseq`

(declare-function helm-comp-read "helm-mode" (prompt collection &rest args))
(declare-function aider--generate-history-file-name "aider-core" ())
(declare-function aider--parse-aider-cli-history "aider-core" (file-path))

(defun aider-helm-read-string-with-history (prompt history-file-name &optional initial-input candidate-list)
  "Read a string with Helm completion using specified history file.
PROMPT is the prompt string.
HISTORY-FILE-NAME is the base name for history file.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  ;; Load history from file
  (let* ((helm-history-file (expand-file-name history-file-name user-emacs-directory))
         (helm-history (if (file-exists-p helm-history-file)
                           (with-temp-buffer
                             (insert-file-contents helm-history-file)
                             (read (buffer-string))) ; Assumed newest first
                         '()))
         (cli-history-file-path (aider--generate-history-file-name))
         (parsed-cli-history (if cli-history-file-path
                                 (aider--parse-aider-cli-history cli-history-file-path) ; Oldest first
                               '()))
         (cli-history-newest-first (reverse parsed-cli-history))
         ;; Combine Helm history and CLI history, then deduplicate. Helm history items take precedence.
         (history (delete-dups (append helm-history cli-history-newest-first)))
         ;; Extract the most recent item from history (if exists)
         (most-recent (when (and history (not (null history)))
                        (car history)))
         ;; Remove the first item to add it back later
         (rest-history (when (and history (not (null history)))
                         (cdr history)))
         ;; Combine completion list: most recent + candidates + separator + rest of history
         (completion-list
          (append
           ;; If most recent item exists, put it at the top
           (when most-recent
             (list most-recent))
           ;; Add candidate list
           (or candidate-list '())
           ;; Add separator and rest of history
           (when rest-history
             (cons "==================== HISTORY ========================================" rest-history))))
         ;; Read input with helm
         (input (helm-comp-read
                 prompt
                 completion-list
                 :must-match nil
                 :name "Helm Read String, Use C-c C-y to edit selected command. C-b and C-f to move cursor during editing"
                 :fuzzy t
                 :initial-input initial-input)))
    ;; Add to history if non-empty, single-line and save
    (unless (or (string-empty-p input) (string-match "\n" input))
      (push input history)
      ;; (setq history (mapcar #'substring-no-properties history))
      (with-temp-file helm-history-file ; Save to the Helm-specific history file
        (let ((history-entries (cl-subseq history
                                          0 (min (length history)
                                                 10000))))  ; Keep last 10000 entries
          (insert (let ((print-circle nil))
                    (prin1-to-string history-entries))))))
    input))

(defun aider-helm-read-string (prompt &optional initial-input candidate-list)
  "Read a string with Helm completion for aider, showing historical inputs.
PROMPT is the prompt string.
INITIAL-INPUT is optional initial input string.
CANDIDATE-LIST is an optional list of candidate strings to show before history."
  (aider-helm-read-string-with-history prompt "aider-helm-read-string-history.el" initial-input candidate-list))

(declare-function aider-read-string "aider")

;;;###autoload
(if (featurep 'helm)
    (defalias 'aider-read-string #'aider-helm-read-string))

(provide 'aider-helm)
;;; aider-helm.el ends here
