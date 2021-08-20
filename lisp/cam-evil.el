;;; cam-evil -- Evil-related config -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

(eval-when-compile
  (require 'cl-lib))

(require 'evil)
(require 'cam-theme)

(setq evil-default-state 'emacs)

;; always use emacs state instead of insert state.
(defalias 'evil-insert-state 'evil-emacs-state)

(defun cam/evil-state-color (&optional state)
  (cl-case (or state evil-state)
    ('emacs "#dd0000")
    ('normal "#0072bb")
    ('visual "#ffd034")
    ('insert "#336600")
    ('replace "#663399")
    ('operator "#ff0099")
    ('motion "orange")
    (otherwise "#cc6633")))

(set-cursor-color (cam/evil-state-color))

;; set colors for evil-STATE-state-cursor for various evil states
(dolist (state '(emacs normal visual insert replace operator motion))
  (set (intern (format "evil-%s-state-cursor" state))
       (list (cam/evil-state-color state) 'box)))

(defun cam/evil--enable-relative-line-numbers ()
  (setq-local display-line-numbers 'visual))

(defun cam/evil--disable-relative-line-numbers ()
  (kill-local-variable 'display-line-numbers))

(defun cam/evil--is-lisp-mode-p ()
  (and (boundp 'cam/is-lisp-mode-p)
       cam/is-lisp-mode-p))

(declare-function smartparens-mode "smartparens")
(declare-function paredit-mode "paredit")
(declare-function evil-smartparens-mode "evil-smartparens")
(declare-function evil-cleverparens-mode "evil-cleverparens")

(defun cam/evil--switch-to-paredit ()
  (when (cam/evil--is-lisp-mode-p)
    (ignore-errors
      (paredit-mode 1))
    (smartparens-mode -1)
    (when evil-mode
      (evil-smartparens-mode -1)
      (evil-cleverparens-mode -1))))

(defun cam/evil--switch-to-smartparens ()
  (when (cam/evil--is-lisp-mode-p)
    (paredit-mode -1)
    (smartparens-mode 1)
    (when evil-mode
      (evil-smartparens-mode 1)
      (evil-cleverparens-mode 1))))

(dolist (hook '(evil-normal-state-entry-hook evil-operator-state-entry-hook evil-motion-state-entry-hook))
  (add-hook hook #'cam/evil--enable-relative-line-numbers)
  (add-hook hook #'cam/evil--switch-to-smartparens))

(dolist (hook '(evil-emacs-state-entry-hook evil-insert-state-entry-hook))
  (add-hook hook #'cam/evil--disable-relative-line-numbers)
  (add-hook hook #'cam/evil--switch-to-paredit))


;;; evil-powerline config

(require 'powerline-evil)

(setq powerline-evil-tag-style 'verbose)

(setq cam/theme-mode-tag-function #'powerline-evil-tag)

;; define faces for various evil states in powerline e.g. cam/active-evil-normal-state
(defun cam/evil--active-state-face-symbol (&optional state)
  (let ((state (or state evil-state 'nil)))
    (intern (format "cam/active-evil-%s-state" state))))

(dolist (state '(nil emacs normal visual insert replace operator motion))
  (let ((symb (cam/evil--active-state-face-symbol state))
        (face (list (list t :background (cam/evil-state-color state) :foreground "white"))))
    (face-spec-set symb face)))

(setq cam/theme-mode-face-symbol-function #'cam/evil--active-state-face-symbol)

(provide 'cam-evil)
