;;; -*- lexical-binding: t; -*-

(require 'cam-commands)
(require 'highlight-parentheses)
(require 'paredit)
(require 'paren) ; show-paren-mode
(require 'rainbow-delimiters)

(defvar-local cam/is-lisp-mode-p nil)

(defun cam/lisp-mode-setup ()
  (unless cam/is-lisp-mode-p
    (setq-local cam/is-lisp-mode-p t)
    (highlight-parentheses-mode 1)
    (rainbow-delimiters-mode 1)
    (show-paren-mode 1)
    (paredit-mode 1)
    ;; (cam/switch-to-paredit)
    ;; (when (and evil-mode
    ;;            (not (cl-member evil-state '(emacs insert))))
    ;;   (cam/switch-to-smartparens))
    ;;
    ;; 0 = add function to beginning of hook
    ;; t = set buffer-local value rather than global.
    (add-hook 'before-save-hook #'cam/untabify-current-buffer 0 t)))

(provide 'cam-lisp)
