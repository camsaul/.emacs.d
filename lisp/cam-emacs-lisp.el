;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package)
  (require 'subr-x))

(require 'cam-lisp)
(require 'elisp-mode)

(require 'aggressive-indent)
(require 'auto-compile)
(require 'cam-emacs-lisp-color-code)
(require 'cam-todo-font-lock)
(require 'column-enforce-mode)
(require 'company)
(require 'dash)
(require 'diminish)
(require 'elisp-slime-nav)
(require 'flycheck)
(require 'flyspell)
(require 'ielm)
(require 'macrostep)
(require 'morlock)
(require 'pp)
(require 'wiki-nav)

(defun cam/emacs-lisp-macroexpand-last-sexp ()
  (interactive)
  (call-interactively #'pp-macroexpand-last-sexp)
  (with-current-buffer "*Pp Macroexpand Output*"
    (macrostep-mode 1)))

(defun cam/emacs-lisp-eval-switch-to-ielm ()
  (interactive)
  (eval-buffer)
  (if-let ((ielm-window (get-window-with-predicate
                         (lambda (window)
                           (string-equal (buffer-name (window-buffer window)) "*ielm*")))))
      (select-window ielm-window)
    (let ((new-window (split-window-sensibly)))
      (if new-window
          (select-window new-window)
        (other-window 1))
      (ielm)
      (balance-windows))))

(defun cam/emacs-lisp-insert-message (text)
  (interactive "sprintln: ")
  (if current-prefix-arg
      (insert "(message \" text \")")
    (insert "(message \"" text ": %s\" " text ")")))

(cam/tweak-package elisp-mode
  :mode-name emacs-lisp-mode
  :load ((put 'add-hook 'lisp-indent-function 1))
  :minor-modes (aggressive-indent-mode
                auto-compile-mode
                cam/emacs-lisp-color-code-mode
                cam/todo-font-lock-mode
                column-enforce-mode
                company-mode
                eldoc-mode
                elisp-slime-nav-mode
                flyspell-mode
                morlock-mode
                wiki-nav-mode)
  :setup ((cam/lisp-mode-setup)
          (unless (string-equal user-init-file (buffer-file-name))
            (flycheck-mode 1))
          (with-eval-after-load 'dash
            (global-dash-fontify-mode 1)))
  :local-vars ((fill-column . 118)
               (emacs-lisp-docstring-fill-column . 118))
  :keys (("<C-M-return>" . #'cam/emacs-lisp-eval-switch-to-ielm)
         ("C-c RET"      . #'cam/emacs-lisp-macroexpand-last-sexp)
         ("C-x C-e"      . #'pp-eval-last-sexp)
         ("<f1>" . #'elisp-slime-nav-describe-elisp-thing-at-point)
         ("<f10>" . #'cam/emacs-lisp-insert-message)))

(cam/tweak-package elisp-slime-nav
  :load ((diminish 'elisp-slime-nav-mode))
  :keys (("C-c C-d" . #'elisp-slime-nav-describe-elisp-thing-at-point)))

(cam/tweak-package ielm
  :mode-name inferior-emacs-lisp-mode
  :hook-name ielm-mode-hook
  :minor-modes (aggressive-indent-mode
                company-mode
                elisp-slime-nav-mode
                morlock-mode)
  :setup ((cam/lisp-mode-setup))
  :local-vars ((indent-line-function . #'lisp-indent-line))     ; automatically indent multi-line forms correctly
  :keys (("C-c RET" . #'cam/emacs-lisp-macroexpand-last-sexp)
         ("<f1>" . #'elisp-slime-nav-describe-elisp-thing-at-point)))

(cam/tweak-package nadvice
  :load ((put #'advice-add 'lisp-indent-function 2)))

(provide 'cam-emacs-lisp)
