;;; -*- lexical-binding: t; comment-column: 50; -*-

;;; ---------------------------------------- Initial Setup ----------------------------------------
;;; (Things that need to happen as soon as this file starts loading)

(setq gc-cons-threshold (* 32 1024 1024)          ; A more reasonable garbage collection threshold
      load-prefer-newer t)                        ; load .el files if they're newer than .elc ones

;;; Don't show toolbar, scrollbar, splash screen, startup screen

(mapc (lambda (mode)
        (when (boundp mode)
          (funcall mode -1)))
      '(menu-bar-mode
        scroll-bar-mode
        tool-bar-mode))

(setq inhibit-splash-screen t
      inhibit-startup-screen t)

;;; ---------------------------------------- Package Setup ----------------------------------------

(require 'package)
(package-initialize)

(nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar cam/has-refreshed-package-contents nil)

(defconst cam/packages
  '(ac-cider                                      ; auto-complete <-> cider
    ace-jump-mode
    aggressive-indent
    anzu                                          ; Show number of matches in mode-line while searching
    auto-complete                                 ; auto-completion
    cider
    clj-refactor
    company                                       ; auto-completion
    editorconfig
    elisp-slime-nav                               ; Make M-. and M-, work in elisp like the do in slime
    find-things-fast
    gitignore-mode                                ; Major mode for editing .gitignore files
    guide-key
    helm
    highlight-parentheses                         ; highlight matching parentheses
    ido-vertical-mode
    loccur
    macrostep                                     ; Interactive macrostepper for Emacs Lisp
    magit
    markdown-mode                                 ; Major mode for editing markdown files
    multiple-cursors
    moe-theme
    paredit
    rainbow-delimiters
    undo-tree))

;;; Install packages as needed
(mapc (lambda (package)
        (unless (package-installed-p package)
          (unless cam/has-refreshed-package-contents
            (ignore-errors
              (package-refresh-contents))
            (setq cam/has-refreshed-package-contents t))
          (condition-case err
              (package-install package)
            (error (warn (concat "Failed to install package " (symbol-name package) ": " (error-message-string err)))))))
      cam/packages)

(eval-when-compile
  (mapc #'require cam/packages))

;;; Declare some functions so byte compiler stops bitching about them possibly not being defined at runtime
(defmacro declare-functions (file fn &rest more)
  `(progn (declare-function ,fn ,file)
          ,(when more
             `(declare-functions ,file ,@more))))
(put #'declare-functions 'lisp-indent-function 1)

(declare-functions "cider-interaction"
  cider-current-ns
  cider-load-buffer
  cider-switch-to-last-clojure-buffer
  cider-switch-to-relevant-repl-buffer)

(declare-functions "cider-repl"
  cider-repl-clear-buffer
  cider-repl-set-ns)

(declare-functions "org"
  org-bookmark-jump-unhide)


;;; ---------------------------------------- Global Setup ----------------------------------------

;;; Theme

(require 'moe-theme)
(moe-light)


;;; Global Requires

(require 'editorconfig)

;;; Global Settings

(blink-cursor-mode -1)                            ; disable annoying blinking cursor
(set-fringe-mode -1)                              ; disable displaying the fringes

(delete-selection-mode 1)                         ; typing will delete selected text
(global-anzu-mode 1)                              ; show number of matches in mode-line while searching
(global-auto-revert-mode 1)                       ; automatically reload files when they change on disk
(global-undo-tree-mode 1)
(guide-key-mode 1)
(ido-mode 1)
(ido-everywhere 1)                                ; use ido for all buffer/file reading
(ido-vertical-mode 1)
(save-place-mode 1)                               ; automatically save last place in files; reopen at that position
(winner-mode 1)

(fset #'yes-or-no-p #'y-or-n-p)                   ; Prompt for 'y' or 'n' instead of 'yes' or 'no'

(prefer-coding-system 'utf-8-auto-unix)

(mapatoms (lambda (atom)                          ; Enable all disabled functions
            (when (get atom 'disabled)
              (put atom 'disabled nil))))

(setq backup-directory-alist                      ; save backup files to ~/.emacs.d/backups
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory
                         "backups"))))

      custom-file (concat user-emacs-directory    ; write customizations to ~/.emacs.d/custom.el instead of init.el
                          "custom.el")
      echo-keystrokes 0.1                         ; show keystrokes in progress in minibuffer after 0.1 seconds instead of 1 second
      global-auto-revert-non-file-buffers t       ; also auto-revert buffers like dired
      indent-tabs-mode nil                        ; disable insertion of tabs
      require-final-newline t                     ; add final newline on save
      save-interprogram-paste-before-kill t       ; Save clipboard strings (from other applications) into kill-ring before replacing them
      savehist-mode t                             ; Periodically save minibuffer history
      select-enable-clipboard t                   ; Cutting and pasting uses the clipboard
      vc-make-backup-files t                      ; Make backups of files even if they're under VC
      visible-bell t)

(setq-default truncate-lines t)                   ; don't display "continuation lines" (don't wrap long lines)

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

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ; if we're saving a script, give it execute permissions


;;; Global Keybindings

;; Unset global and keymap-specific keys that we want to change below
(mapc (lambda (mode-map-and-keys)
        (let ((mode-map (eval (car mode-map-and-keys)))
              (keys     (mapcar #'kbd (cdr mode-map-and-keys))))
          (mapc (if mode-map
                    (lambda (key)
                      (define-key mode-map key nil))
                  #'global-unset-key)
                keys)))
      '((nil "C-b" "C-f" "C-v")
        (winner-mode-map "C-c <left>" "C-c <right>")))

(mapc (lambda (key-command-pair)
        (global-set-key (kbd (car key-command-pair)) (eval (cdr key-command-pair))))
      '(("C-M-y"         . #'helm-show-kill-ring)
        ("C-b SPC"       . #'mc/mark-all-like-this)
        ("C-b a"         . #'mc/mark-previous-like-this)
        ("C-b e"         . #'mc/mark-next-like-this)
        ("C-b <return>"  . #'mc/mark-next-lines)
        ("C-f"           . #'ace-jump-mode)
        ("C-c f"         . #'ftf-grepsource)
        ("C-c l"         . #'(lambda ()
                               (interactive)
                               (require 'loccur)
                               (call-interactively #'loccur)))
        ("C-c o"         . #'ftf-find-file)
        ("C-c w <left>"  . #'winner-undo)
        ("C-c w <right>" . #'winner-redo)
        ("C-c <down>"    . #'windmove-down)
        ("C-c <left>"    . #'windmove-left)
        ("C-c <right>"   . #'windmove-right)
        ("C-c <up>"      . #'windmove-up)
        ("C-x C-b"       . #'helm-buffers-list)
        ("C-x C-f"       . #'helm-find-files)
        ("C-x C-r"       . #'helm-recentf)
        ("C-x b"         . #'helm-buffers-list)
        ("C-x f"         . #'helm-find-files)
        ("C-x k"         . #'kill-this-buffer)
        ("C-x m"         . #'magit-status)
        ("C-z"           . #'undo)
        ("ESC <up>"      . #'windmove-up)
        ("M-j"           . #'cam/join-next-line)
        ("M-x"           . #'helm-M-x)))


;;; ---------------------------------------- Mode/Package Specific Setup ----------------------------------------

;;; Lisp Modes
(defun cam/lisp-mode-setup ()
  (highlight-parentheses-mode 1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)

  (add-hook 'before-save-hook
            (lambda ()
              (cam/untabify-current-buffer))
            nil
            :local))


;;; Auto-complete
(eval-after-load "auto-complete"
  '(progn (setq ac-delay 0.05
                ac-auto-show-menu 0.1
                ac-quick-help-delay 0.2)
          (ac-config-default)
          (nconc ac-modes '(cider-repl-mode
                            emacs-lisp-mode
                            ielm-mode))))


;;; Clojure
(defun cam/clojure-save-load-switch-to-cider ()
  (interactive)
  (save-buffer)
  (cider-load-buffer)
  (cider-repl-set-ns (cider-current-ns))
  (cider-switch-to-relevant-repl-buffer)
  (cider-repl-clear-buffer))

(defun cam/clojure-mode-setup ()
  (cam/lisp-mode-setup)
  (auto-complete-mode 1)
  (ac-cider-setup)
  (clj-refactor-mode 1)


  (define-key clojure-mode-map
    (kbd "M-RET") #'cam/clojure-save-load-switch-to-cider)
  (cljr-add-keybindings-with-prefix "C-c r"))
(add-hook 'clojure-mode-hook #'cam/clojure-mode-setup)

(defun cam/cider-repl-mode-setup ()
  (cam/lisp-mode-setup)
  (auto-complete-mode 1)
  (ac-cider-setup)
  (aggressive-indent-mode 1)

  (setq cider-auto-select-error-buffer nil
        cider-repl-use-pretty-printing t)

  (define-key cider-repl-mode-map
    (kbd "M-RET") #'cider-switch-to-last-clojure-buffer))
(add-hook 'cider-repl-mode-hook #'cam/cider-repl-mode-setup)


;;; Company
(eval-after-load "company"
  '(setq company-idle-delay 0.01
         company-minimum-prefix-length 1))


;;; Emacs Lisp Mode
(defun cam/emacs-lisp-macroexpand-last-sexp ()
  (interactive)
  (call-interactively #'pp-macroexpand-last-sexp)
  (with-current-buffer "*Pp Macroexpand Output*"
    (macrostep-mode 1)))

(defun cam/emacs-lisp-mode-setup ()
  (cam/lisp-mode-setup)
  (aggressive-indent-mode 1)
  (auto-complete-mode 1)
  (elisp-slime-nav-mode 1)

  (define-key emacs-lisp-mode-map
    (kbd "C-c RET") #'cam/emacs-lisp-macroexpand-last-sexp)

  (add-hook 'after-save-hook
            (lambda ()
              (when (buffer-file-name)
                (byte-compile-file (buffer-file-name))))
            nil
            :local))
(add-hook 'emacs-lisp-mode-hook #'cam/emacs-lisp-mode-setup)

(defun cam/ielm-mode-setup ()
  (cam/lisp-mode-setup)
  (aggressive-indent-mode 1)
  (auto-complete-mode 1)
  (ac-emacs-lisp-mode-setup))
(add-hook 'ielm-mode-hook #'cam/ielm-mode-setup)


;;; Eval Expresssion (Minibuffer)
(defun cam/eval-expression-minibuffer-setup ()
  (company-mode 1)
  (paredit-mode 1)
  (setq-local company-echo-delay 10))
(add-hook 'eval-expression-minibuffer-setup-hook #'cam/eval-expression-minibuffer-setup)


;;; Find Things Fast
(eval-after-load "find-things-fast"
  '(nconc ftf-filetypes '("*.clj"
                          "*.css"
                          "*.el"
                          "*.html"
                          "*.js"
                          "*.java"
                          "*.md"
                          "*.yml")))


;;; Guide-Key
(setq guide-key/idle-delay 1.0
      guide-key/recursive-key-sequence-flag t
      guide-key/guide-key-sequence '("<f12>" "<f1>"
                                     "<help>" "A-'"
                                     "A-*"    "A-,"
                                     "A-/"    "A-1"
                                     "A-3"    "A-\""
                                     "A-^"    "A-_"
                                     "A-`"    "A-r"
                                     "A-~"    "C-c"
                                     "C-h"    "C-x"
                                     "M-g"    "M-o"))

;;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;;; ---------------------------------------- Final Setup ----------------------------------------

(ignore-errors
  (load-file custom-file))
