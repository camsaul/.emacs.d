;;; -*- lexical-binding: t; byte-compile-dynamic: t; comment-column: 50; -*-

;;; TOC:
;;; [[Initial Setup]]
;;; [[Package Setup]]
;;; [[Global Setup]]
;;;    [[Theme]]
;;;    [[Global Requires]]
;;;    [[Global Minor Modes]]
;;;    [[Global Settings]]
;;;    [[Global Functions]]
;;;    [[Global Hooks]]
;;;    [[Global Keybindings]]
;;; [[Mode/Package Specific Setup]]
;;;    [[Lisp Modes]]
;;;    [[auto-complete]]
;;;    [[Clojure]]
;;;    [[dired]]
;;;    [[company]]
;;;    [[Emacs Lisp]]
;;;    [[Eval Expresssion (Minibuffer)]]
;;;    [[Find Things Fast]]
;;;    [[Guide Key]]
;;;    [[Magit]]
;;; [[Final Setup]]

;;; ---------------------------------------- [[<Initial Setup]] ----------------------------------------
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

;;; ---------------------------------------- [[<Package Setup]] ----------------------------------------

(require 'package)
(package-initialize)

(nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
(defconst cam/packages
  '(ac-cider                                      ; auto-complete <-> cider
    ace-jump-mode
    aggressive-indent                             ; Minor mode to aggressively keep code always indented
    anzu                                          ; Show number of matches in mode-line while searching
    auto-complete                                 ; auto-completion
    cider                                         ; Clojure Interactive Development Environment that Rocks
    clj-refactor                                  ; Clojure refactoring minor mode
    clojure-mode-extra-font-locking
    company                                       ; auto-completion
    diff-hl                                       ; mark uncommited changes in the fringe
    diminish                                      ; Replace or hide minor modes in mode-line
    dockerfile-mode                               ; Major mode for editing Dockerfiles
    editorconfig                                  ; Read EditorConfig files
    elisp-slime-nav                               ; Make M-. and M-, work in elisp like the do in slime
    find-things-fast
    git-timemachine                               ; Walk through git revisions of a file
    gitignore-mode                                ; Major mode for editing .gitignore files
    guide-key
    helm
    highlight-parentheses                         ; highlight matching parentheses
    ido-vertical-mode
    loccur
    macrostep                                     ; Interactive macrostepper for Emacs Lisp
    magit
    markdown-mode                                 ; Major mode for editing markdown files
    morlock                                       ; Extra font-locking for Emacs Lisp
    multiple-cursors
    moe-theme
    paredit
    projectile
    rainbow-delimiters
    rainbow-mode
    register-list                                 ; dired-like editing of Emacs registers
    rotate                                        ; rotate-window, rotate-layout, etc.
    undo-tree
    wiki-nav                                      ; Navigate a file using [[WikiStrings]]
    yaml-mode))

;;; Install packages as needed
(defvar cam/has-refreshed-package-contents nil)
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
  cider-repl-return
  cider-repl-set-ns)

(declare-functions "dired"
  dired-hide-details-mode)

(declare-functions "loccur"
  loccur)

(declare-functions "org"
  org-bookmark-jump-unhide)


;;; ---------------------------------------- [[<Global Setup]] ----------------------------------------

;;; [[<Theme]]

(require 'moe-theme)
(moe-light)

(defun cam/setup-frame ()
  (set-frame-font "Source Code Pro-12"))
(advice-add #'make-frame-command :after #'cam/setup-frame)

(cam/setup-frame)

(set-face-background 'mode-line-buffer-id nil)    ; Don't show a blue background behind buffer name on modeline for deselected frames

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                " "
                (:eval (cond
                        (buffer-read-only    "[read-only] ")
                        ((buffer-modified-p) "[modified] ")
                        ((:else              " "))))
                mode-line-buffer-identification
                "   L%l/"
                (:eval (int-to-string (line-number-at-pos (point-max))))
                "  C%c  "
                (vc-mode vc-mode)
                "  "
                (:propertize mode-name
                             face mode-line-buffer-id)
                minor-mode-alist
                mode-line-misc-info
                mode-line-end-spaces))


;;; [[<Global Requires]]

(require 'editorconfig)

;;; [[<Global Minor Modes]]

(blink-cursor-mode -1)                            ; disable annoying blinking cursor

(delete-selection-mode 1)                         ; typing will delete selected text
(global-anzu-mode 1)                              ; show number of matches in mode-line while searching
(global-auto-revert-mode 1)                       ; automatically reload files when they change on disk
(global-diff-hl-mode 1)
(global-undo-tree-mode 1)
(global-wiki-nav-mode 1)
(guide-key-mode 1)
(projectile-global-mode 1)
(ido-mode 1)
(ido-everywhere 1)                                ; use ido for all buffer/file reading
(ido-vertical-mode 1)
(rainbow-mode 1)                                  ; Highlight color strings like #8844AA
(save-place-mode 1)                               ; automatically save last place in files; reopen at that position
(winner-mode 1)

(mapc #'diminish
      '(anzu-mode
        button-lock-mode
        diff-hl-mode
        guide-key-mode
        rainbow-mode
        undo-tree-mode
        wiki-nav-mode))

;;; [[<Global Settings]]

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
      ns-right-command-modifier 'hyper
      ns-right-control-modifier 'hyper
      ns-right-option-modifier 'alt
      require-final-newline t                     ; add final newline on save
      revert-without-query '(".*")                ; tell revert-buffer to revert all buffers without confirmation
      save-interprogram-paste-before-kill t       ; Save clipboard strings (from other applications) into kill-ring before replacing them
      savehist-mode t                             ; Periodically save minibuffer history
      select-enable-clipboard t                   ; Cutting and pasting uses the clipboard
      vc-make-backup-files t                      ; Make backups of files even if they're under VC
      visible-bell t)

(setq-default truncate-lines t)                   ; don't display "continuation lines" (don't wrap long lines)


;;; [[<Global Functions]]

(defun cam/untabify-current-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cam/join-next-line ()
  (interactive)
  (join-line -1))

(defun cam/loccur ()
  (interactive)
  (require 'loccur)
  (call-interactively #'loccur))

(defun cam/backward-kill-line ()
  "Kill line from current cursor position to beginning of line."
  (interactive)
  (kill-line 0))


;;; [[<Global Hooks]]

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
            (set-buffer-file-coding-system 'utf-8-auto-unix)))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ; if we're saving a script, give it execute permissions


;;; [[<Global Keybindings]]

(mapc (lambda (key-command-pair)
        (global-set-key (kbd (car key-command-pair)) (eval (cdr key-command-pair))))
      '(("<A-return>"    . #'wiki-nav-ido)
        ("<C-M-s-down>"  . #'windmove-down)
        ("<C-M-s-left>"  . #'windmove-left)
        ("<C-M-s-right>" . #'windmove-right)
        ("<C-M-s-up>"    . #'windmove-up)
        ("<H-SPC>"       . #'mc/mark-all-like-this)
        ("<H-escape>"    . #'ace-jump-line-mode)
        ("<H-return>"    . #'mc/mark-next-lines)
        ("<escape>"      . #'ace-jump-mode)
        ("<f11>"         . nil)                   ; Use <f11> <key> for toggling various minor modes
        ("<f11> p"       . #'paredit-mode)
        ("<f11> w"       . #'whitespace-mode)
        ("A-;"           . #'cam/loccur)
        ("A-r l"         . #'rotate-layout)
        ("A-r w"         . #'rotate-window)
        ("C-="           . #'magit-status)
        ("C-M-y"         . #'helm-show-kill-ring)
        ("C-S-k"         . #'cam/backward-kill-line)
        ("C-x C-b"       . #'helm-buffers-list)
        ("C-x C-f"       . #'helm-find-files)
        ("C-x C-g"       . #'keyboard-quit)
        ("C-x C-r"       . #'helm-recentf)
        ("C-x C-z"       . nil)                   ; instead of suspend-frame
        ("C-x b"         . #'helm-buffers-list)
        ("C-x C-d"       . #'dired)               ; instead of ido-list-directory
        ("C-x f"         . #'helm-find-files)
        ("C-x k"         . #'kill-this-buffer)
        ("C-x r r"       . #'register-list)       ; replaces copy-rectangle-to-register
        ("C-z"           . #'undo)
        ("ESC <up>"      . #'windmove-up)
        ("H-a"           . #'mc/mark-previous-like-this)
        ("H-e"           . #'mc/mark-next-like-this)
        ("M-j"           . #'cam/join-next-line)
        ("M-x"           . #'helm-M-x)
        ("s-Z"           . #'undo-tree-redo)
        ("s-f"           . #'ftf-grepsource)
        ("s-o"           . #'ftf-find-file)))


;;; ---------------------------------------- [[<Mode/Package Specific Setup]] ----------------------------------------

;;; [[<Lisp Modes]]
(defun cam/lisp-mode-setup ()
  (highlight-parentheses-mode 1)
  (diminish 'highlight-parentheses-mode)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)

  (add-hook 'before-save-hook
            (lambda ()
              (cam/untabify-current-buffer))
            nil
            :local))


;;; [[<auto-complete]]
(eval-after-load "auto-complete"
  '(progn (setq ac-delay 0.05
                ac-auto-show-menu 0.1
                ac-quick-help-delay 0.2)
          (ac-config-default)
          (nconc ac-modes '(cider-repl-mode
                            emacs-lisp-mode
                            ielm-mode))))


;;; [[<Clojure]]
(defun cam/clojure-save-load-switch-to-cider ()
  (interactive)
  (save-buffer)

  (condition-case _
      (progn
        (cider-load-buffer)
        (cider-repl-set-ns (cider-current-ns))
        (cider-switch-to-relevant-repl-buffer)
        (cider-repl-clear-buffer))
    (error (cider-jack-in))))

(defun cam/clojure-mode-setup ()
  (cam/lisp-mode-setup)
  (auto-complete-mode 1)
  (ac-cider-setup)
  (clj-refactor-mode 1)
  (require 'clojure-mode-extra-font-locking)

  (define-key clojure-mode-map
    (kbd "<C-M-s-return>") #'cam/clojure-save-load-switch-to-cider)
  (cljr-add-keybindings-with-modifier "A-H-"))
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

;; Delete trailing whitespace that may have been introduced by auto-complete
(eval-after-load "cider"
  '(advice-add #'cider-repl-return :before (lambda ()
                                             (interactive)
                                             (call-interactively #'delete-trailing-whitespace))))


;;; [[<company]]
(eval-after-load "company"
  '(setq company-idle-delay 0.01
         company-minimum-prefix-length 1))


;;; [[<dired]]
(defun cam/dired-mode-setup ()
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook #'cam/dired-mode-setup)


;;; [[<Emacs Lisp]]
(defun cam/emacs-lisp-macroexpand-last-sexp ()
  (interactive)
  (call-interactively #'pp-macroexpand-last-sexp)
  (with-current-buffer "*Pp Macroexpand Output*"
    (macrostep-mode 1)))

(defun cam/emacs-lisp-save-switch-to-ielm-if-visible ()
  (interactive)
  (save-buffer)
  (let ((ielm-window (get-window-with-predicate (lambda (window)
                                                  (string= (buffer-name (window-buffer window)) "*ielm*")))))
    (when ielm-window
      (select-window ielm-window)
      (comint-clear-buffer)
      (comint-kill-input))))

(defun cam/emacs-lisp-mode-setup ()
  (cam/lisp-mode-setup)
  (aggressive-indent-mode 1)
  (auto-complete-mode 1)
  (elisp-slime-nav-mode 1)
  (morlock-mode 1)

  (define-key emacs-lisp-mode-map (kbd "C-c RET")        #'cam/emacs-lisp-macroexpand-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "<C-M-s-return>") #'cam/emacs-lisp-save-switch-to-ielm-if-visible)

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
  (ac-emacs-lisp-mode-setup)
  (morlock-mode 1))
(add-hook 'ielm-mode-hook #'cam/ielm-mode-setup)


;;; [[<Eval Expresssion (Minibuffer)]]
(defun cam/eval-expression-minibuffer-setup ()
  (company-mode 1)
  (paredit-mode 1)
  (setq-local company-echo-delay 10))
(add-hook 'eval-expression-minibuffer-setup-hook #'cam/eval-expression-minibuffer-setup)


;;; [[<Find Things Fast]]
(eval-after-load "find-things-fast"
  '(nconc ftf-filetypes '("*.clj"
                          "*.css"
                          "*.el"
                          "*.html"
                          "*.js"
                          "*.java"
                          "*.md"
                          "*.yml")))


;;; [[<Guide Key]]
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


;;; [[<Magit]]
(setq magit-auto-revert-mode-lighter     ""
      magit-last-seen-setup-instructions "1.4.0")

;;; ---------------------------------------- [[<Final Setup]] ----------------------------------------

(ignore-errors
  (load-file custom-file))
(toggle-frame-maximized)
