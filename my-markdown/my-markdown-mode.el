;;; -*- lexical-binding: t; -*-

(require 'font-lock)

(defconst my-markdown-mode-font-lock-keywords
  '("#"))

(defun my-markdown-mode-font-lock-syntactic-face-function (state))

(define-derived-mode my-markdown-mode text-mode "My Markdown"
  "Major mode for editing Markdown files."
  (setq-local comment-start "<!-- "
              comment-end " -->"
              font-lock-defaults '(my-markdown-mode-font-lock-keywords
                                   nil
                                   nil
                                   nil
                                   (font-lock-syntactic-face-function . my-markdown-mode-font-lock-syntactic-face-function))))

(provide 'my-markdown-mode)

(defun my-font-lock-reset ()
  (interactive)
  (setq font-lock-set-defaults nil)
  (font-lock-set-defaults)
  (font-lock-flush))
