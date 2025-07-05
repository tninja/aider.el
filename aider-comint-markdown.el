;;; aider-comint-markdown.el --- Markdown highlighting and advice for Aider -*- lexical-binding: t; -*-

;; Author: Kang Tu <tninja@gmail.com>

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; Provide safe markdown functions and highlighting setup for aider-comint-mode

;;; Code:

(require 'markdown-mode)
(require 'seq)

(defun aider--safe-maybe-funcall-regexp (origfn object &optional arg)
  "Call ORIGFN (`markdown-maybe-funcall-regexp') safely in aider buffers only."
  (if (eq major-mode 'aider-comint-mode)
      (condition-case nil
          (cond ((functionp object)
                 (condition-case nil
                     (if arg (funcall object arg) (funcall object))
                   (error "")))
                ((stringp object) object)
                ((null object) "")
                (t ""))
        (error ""))
    (funcall origfn object arg)))

(defun aider--safe-get-start-fence-regexp (origfn &rest args)
  "Safely call `markdown-get-start-fence-regexp' in aider buffers only."
  (if (eq major-mode 'aider-comint-mode)
      (condition-case nil
          (let ((res (apply origfn args)))
            (if (and res (stringp res) (not (string-empty-p res))) res "\\`never-match\\`"))
        (error "\\`never-match\\`"))
    (apply origfn args)))

(defun aider--safe-syntax-propertize-fenced-block-constructs (origfn start end)
  "Safely call `markdown-syntax-propertize-fenced-block-constructs' in aider buffers only."
  (if (eq major-mode 'aider-comint-mode)
      (condition-case nil
          (funcall origfn start end)
        (error nil))
    (funcall origfn start end)))

(defun aider--jit-lock-defer-fontification-advice (orig-fn)
  "Run `jit-lock-defer-fontification' with `jit-lock-defer-time' set to 0 for `aider-comint-mode'."
  (if (eq major-mode 'aider-comint-mode)
      (let ((jit-lock-defer-time 0))
        (funcall orig-fn))
    (funcall orig-fn)))

(advice-add 'markdown-maybe-funcall-regexp :around #'aider--safe-maybe-funcall-regexp)
(advice-add 'markdown-get-start-fence-regexp :around #'aider--safe-get-start-fence-regexp)
(advice-add 'markdown-syntax-propertize-fenced-block-constructs :around #'aider--safe-syntax-propertize-fenced-block-constructs)
(advice-add 'jit-lock-defer-fontification :around #'aider--jit-lock-defer-fontification-advice)

(defun aider--apply-markdown-highlighting ()
  "Set up markdown highlighting for aider buffer with optimized performance."
  (set-syntax-table (make-syntax-table markdown-mode-syntax-table))
  (setq-local syntax-propertize-function #'markdown-syntax-propertize)
  (setq-local font-lock-defaults
              '(markdown-mode-font-lock-keywords nil nil nil nil
                (font-lock-multiline . t)
                (font-lock-extra-managed-props . (composition display invisible))))
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local markdown-regex-italic "\\`never-match-this-pattern\\'")
  (setq-local markdown-regex-bold "\\`never-match-this-pattern\\'")
  (setq-local font-lock-multiline t)
  (setq-local jit-lock-contextually nil)
  (setq-local font-lock-support-mode 'jit-lock-mode)
  (font-lock-mode 1)
  (font-lock-flush)
  (font-lock-ensure)
  (add-hook 'syntax-propertize-extend-region-functions
            #'markdown-syntax-propertize-extend-region nil t)

  (add-hook 'jit-lock-after-change-extend-region-functions
            #'markdown-font-lock-extend-region-function t t)
  (setq-local markdown-regex-blockquote "^\\_>$")
  (if markdown-hide-markup
      (add-to-invisibility-spec 'markdown-markup)
    (remove-from-invisibility-spec 'markdown-markup)))

(provide 'aider-comint-markdown)
;;; aider-comint-markdown.el ends here
