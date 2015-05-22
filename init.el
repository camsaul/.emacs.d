;;; -*- lexical-binding: t; comment-column: 50; -*-

;;; ---------------------------------------- Initial Setup ----------------------------------------
;;; (Things that need to happen as soon as this file starts loading)

(setq gc-cons-threshold (* 32 1024 1024))         ; A more reasonable garbage collection threshold

;;; Don't show toolbar, scrollbar, splash screen, startup screen

(mapc (lambda (mode)
        (when (boundp mode)
          (funcall mode -1)))
      '(scroll-bar-mode
        tool-bar-mode))

(setq inhibit-splash-screen t
      inhibit-startup-screen t)

;;; ---------------------------------------- Package Setup ----------------------------------------

;;; Load some packages!

(require 'package)
(package-initialize)

(nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar cam/has-refreshed-package-contents nil)

(mapc (lambda (package)
        (unless (package-installed-p package)
          (unless cam/has-refreshed-package-contents
            (package-refresh-contents)
            (setq cam/has-refreshed-package-contents t))
          (condition-case err
              (package-install package)
            (error (warn (concat "Failed to install package " (symbol-name package) ": " (error-message-string err)))))))
      '(ace-jump-mode
        auto-complete                             ; auto-completion
        guide-key
        helm
        highlight-parentheses                     ; highlight matching parentheses
        magit
        moe-theme
        paredit
        rainbow-delimiters))

;;; ---------------------------------------- Global Setup ----------------------------------------

;;; Theme

(require 'moe-theme)
(moe-light)
(set-frame-font "Source Code Pro-13")

;;; Global Settings

(blink-cursor-mode -1)                            ; disable annoying blinking cursor
(set-fringe-mode -1)                              ; disable displaying the fringes

(delete-selection-mode t)                         ; typing will delete selected text
(global-auto-revert-mode 1)                       ; automatically reload files when they change on disk
(guide-key-mode 1)
(winner-mode 1)


(prefer-coding-system 'utf-8-auto-unix)

(setq echo-keystrokes 0.1                         ; show keystrokes in progress in minibuffer after 0.1 seconds instead of 1 second
      ns-right-command-modifier 'hyper
      ns-right-control-modifier 'hyper
      ns-right-option-modifier 'alt
      require-final-newline t                     ; add final newline on save
      visible-bell t)
;;; Global Fns

(defun cam/untabify-current-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cam/join-next-line ()
  (interactive)
  (join-line -1))


;;; Global Hooks

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
            (set-buffer-file-coding-system 'utf-8-auto-unix)))


;;; Global Keybindings

(mapc (lambda (key-command-pair)
        (global-set-key (kbd (car key-command-pair)) (eval (cdr key-command-pair))))
      '(("<C-M-s-down>"  . #'windmove-down)
        ("<C-M-s-left>"  . #'windmove-left)
        ("<C-M-s-right>" . #'windmove-right)
        ("<C-M-s-up>"    . #'windmove-up)
        ("<escape>"      . #'ace-jump-mode)
        ("C-="           . #'magit-status)
        ("C-x C-f"       . #'helm-find-files)
        ("C-x C-r"       . #'helm-recentf)
        ("C-x f"         . #'helm-find-files)
        ("M-j"           . #'cam/join-next-line)
        ("M-x"           . #'helm-M-x)))


;;; ---------------------------------------- Mode Specific Setup ----------------------------------------

;;; Auto-complete

(eval-after-load "auto-complete"
  '(progn (setq ac-delay 0.05
                ac-auto-show-menu 0.1
                ac-quick-help-delay 0.2)
          (ac-config-default)
          (add-to-list 'ac-modes 'emacs-lisp-mode)))


;;; Emacs Lisp Mode

(defun cam/emacs-lisp-mode-setup ()
  (auto-complete-mode 1)
  (highlight-parentheses-mode 1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)

  (add-hook 'before-save-hook
            (lambda ()
              (cam/untabify-current-buffer))))
(add-hook 'emacs-lisp-mode-hook #'cam/emacs-lisp-mode-setup)


;;; Guide-Key

(setq guide-key/idle-delay 1.0
      guide-key/recursive-key-sequence-flag t
      guide-key/guide-key-sequence '("<f12>" "<f1>"
                                     "<help>" "A-'"
                                     "A-*" "A-,"
                                     "A-/" "A-1"
                                     "A-3" "A-\""
                                     "A-^" "A-_"
                                     "A-`" "A-r"
                                     "A-~" "C-c"
                                     "C-h" "C-x"
                                     "M-g" "M-o"))

;;; Magit

(eval-after-load "magit"
  '(setq magit-last-seen-setup-instructions "1.4.0"))

;;; ---------------------------------------- Final Setup ----------------------------------------
;;; Things that need to happen at the end of setup or they don't work right

(toggle-frame-maximized)                          ; maximize the frame
