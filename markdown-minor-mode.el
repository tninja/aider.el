;;; markdown-minor-mode.el --- Minor mode for Markdown syntax highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2023

;; Author: Your Name <your.email@example.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (markdown-mode "2.5"))
;; Keywords: markdown, languages
;; URL: https://github.com/yourusername/markdown-minor-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode for Markdown syntax highlighting
;; that can be used with any major mode.  It leverages the syntax
;; highlighting capabilities of `markdown-mode' but applies them as a
;; minor mode.

;;; Code:

(require 'markdown-mode)

(defvar markdown-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `markdown-minor-mode'.")

;;;###autoload
(define-minor-mode markdown-minor-mode
  "Minor mode for editing Markdown files.
This minor mode provides Markdown syntax highlighting for any major mode."
  :lighter " md"
  :keymap markdown-minor-mode-map
  (if markdown-minor-mode
      (markdown-minor-mode-enable)
    (markdown-minor-mode-disable)))

(defun markdown-minor-mode-enable ()
  "Enable Markdown minor mode."
  ;; Save original font-lock settings
  (set (make-local-variable 'markdown-minor-mode-font-lock-keywords)
       font-lock-keywords)
  (set (make-local-variable 'markdown-minor-mode-font-lock-defaults)
       font-lock-defaults)
  
  ;; Apply markdown font-lock settings
  (setq font-lock-defaults
        '(markdown-mode-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-syntactic-face-function . markdown-syntactic-face)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky
                          keymap help-echo mouse-face))))
  
  ;; Set up syntax properties
  (set (make-local-variable 'syntax-propertize-function) #'markdown-syntax-propertize)
  
  ;; Add hooks for syntax highlighting
  (add-hook 'syntax-propertize-extend-region-functions
            #'markdown-syntax-propertize-extend-region nil t)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'markdown-font-lock-extend-region-function t t)
  
  ;; Apply syntax properties to the buffer
  (syntax-propertize (point-max))
  
  ;; Force font-lock refresh
  (font-lock-flush))

(defun markdown-minor-mode-disable ()
  "Disable Markdown minor mode."
  ;; Restore original font-lock settings
  (when (local-variable-p 'markdown-minor-mode-font-lock-keywords)
    (setq font-lock-keywords markdown-minor-mode-font-lock-keywords)
    (kill-local-variable 'markdown-minor-mode-font-lock-keywords))
  
  (when (local-variable-p 'markdown-minor-mode-font-lock-defaults)
    (setq font-lock-defaults markdown-minor-mode-font-lock-defaults)
    (kill-local-variable 'markdown-minor-mode-font-lock-defaults))
  
  ;; Remove hooks
  (remove-hook 'syntax-propertize-extend-region-functions
               #'markdown-syntax-propertize-extend-region t)
  (remove-hook 'jit-lock-after-change-extend-region-functions
               #'markdown-font-lock-extend-region-function t)
  
  ;; Kill local variables
  (kill-local-variable 'syntax-propertize-function)
  
  ;; Force font-lock refresh
  (font-lock-flush))

(provide 'markdown-minor-mode)
;;; markdown-minor-mode.el ends here
