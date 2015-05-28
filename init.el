;;; -*- lexical-binding: t; coding: utf-8; byte-compile-dynamic: nil; comment-column: 50; -*-

(cl-eval-when (eval)
  (unless (>= emacs-major-version 25)
    (error "This setup requires Emacs version 25 or newer.")))

;;; TOC:
;;; [[Initial Setup]]
;;; [[Package Setup]]
;;; [[Global Setup]]
;;;    [[Theme]]
;;;    [[Global Requires]]
;;;    [[Global Minor Modes]]
;;;    [[Diminished Minor Modes]]
;;;    [[Global Settings]]
;;;    [[auto-mode-alist]]
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
;;;    [[js2-mode]]
;;;    [[Magit]]
;;;    [[Org]]
;;;    [[Paredit]]
;;;    [[Web Mode]]
;;; [[Final Setup]]

;;; ---------------------------------------- [[<Initial Setup]] ----------------------------------------
;;; (Things that need to happen as soon as this file starts loading)

(setq gc-cons-threshold (* 64 1024 1024)          ; By default GC starts around ~780kB. Since this isn't the 90s GC when we hit 64MB (too much higher and GC becomes slooowww)
      load-prefer-newer t)                        ; load .el files if they're newer than .elc ones

(defvar cam/has-loaded-init nil
  "Have we done a complete load of the init file yet? (Use this to keep track of things we only want to run once, but not again if we call eval-buffer).")

;;; Don't show toolbar, scrollbar, splash screen, startup screen

(mapc (lambda (mode)
        (when (boundp mode)
          (funcall mode -1)))
      '(scroll-bar-mode
        tool-bar-mode))

(unless (eq window-system 'ns)
  (menu-bar-mode -1))

(setq inhibit-splash-screen t
      inhibit-startup-screen t)

;; In an effort to be really annoying you can only suppress the startup echo area message if you set it through customize
(custom-set-variables
 '(inhibit-startup-echo-area-message (user-login-name)))


;;; ---------------------------------------- [[<Package Setup]] ----------------------------------------

(require 'package)
(package-initialize)

(nconc package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))
(defconst cam/packages
  '(ac-cider                                      ; auto-complete <-> cider
    ace-jump-mode
    ace-jump-zap                                  ; ace-jump-mode version of zap-to-char / zap-up-to-char
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
    esup                                          ; Emacs Start-Up Profiler <3
    find-things-fast
    git-timemachine                               ; Walk through git revisions of a file
    gitignore-mode                                ; Major mode for editing .gitignore files
    guide-key
    helm
    highlight-parentheses                         ; highlight matching parentheses
    ido-vertical-mode
    js2-mode                                      ; Javascript
    loccur
    macrostep                                     ; Interactive macrostepper for Emacs Lisp
    magit
    markdown-mode                                 ; Major mode for editing markdown files
    morlock                                       ; Extra font-locking for Emacs Lisp
    multiple-cursors
    moe-theme
    paredit
    pos-tip                                       ; Native tooltips
    projectile
    rainbow-delimiters
    rainbow-mode
    register-list                                 ; dired-like editing of Emacs registers
    rotate                                        ; rotate-window, rotate-layout, etc.
    undo-tree
    web-mode                                      ; major-mode for editing web templates
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
  (mapc #'require cam/packages)

  ;; Declare some functions so byte compiler stops bitching about them possibly not being defined at runtime
  (defmacro declare-functions (file fn &rest more)
    `(progn (declare-function ,fn ,file)
            ,(when more
               `(declare-functions ,file ,@more))))
  (put #'declare-functions 'lisp-indent-function 1)

  (declare-functions "auto-complete"        ac-complete-functions ac-complete-symbols ac-complete-variables)
  (declare-functions "auto-complete-config" ac-emacs-lisp-mode-setup)
  (declare-functions "cider-interaction"    cider-current-ns cider-load-buffer cider-switch-to-last-clojure-buffer cider-switch-to-relevant-repl-buffer)
  (declare-functions "cider-repl"           cider-repl-clear-buffer cider-repl-return cider-repl-set-ns)
  (declare-functions "dired"                dired-hide-details-mode)
  (declare-functions "loccur"               loccur)
  (declare-functions "magit"                magit-get magit-get-current-branch magit-get-current-remote)
  (declare-functions "org"                  org-bookmark-jump-unhide)
  (declare-functions "paredit"              paredit-backward-delete paredit-doublequote paredit-newline paredit-open-round paredit-open-square paredit-forward-delete))


;;; ---------------------------------------- [[<Global Setup]] ----------------------------------------

;;; [[<Theme]]

(require 'moe-theme)

;; Load the theme just once, otherwise the screen will flicker all cray if we try to eval this buffer again
(unless cam/has-loaded-init
  (moe-dark))

(defconst cam/mode-line-color "#fce94f")

(defun cam/setup-frame ()
  (set-frame-font "Source Code Pro-12")

  (set-fringe-style '(6 . 0))                     ; ¾ width fringe on the left and none on the right

  ;; (moe-theme-random-color)
  (set-face-foreground 'mode-line "#111111")
  (set-face-background 'mode-line cam/mode-line-color)
  (set-cursor-color (face-background 'mode-line))
  (set-face-background 'mode-line-buffer-id nil)) ; Don't show a blue background behind buffer name on modeline for deselected frames
(advice-add #'make-frame-command :after #'cam/setup-frame)

(unless cam/has-loaded-init
  (cam/setup-frame))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                " "
                (:eval (cond
                        (buffer-read-only    "[read-only] ")
                        ((buffer-modified-p) "[modified] ")
                        (:else                " ")))
                mode-line-buffer-identification
                (:propertize " %n "               ; %n = 'Narrow' when narrowing is in effect
                             face mode-line-buffer-id)
                " L%l/"                           ; %l = line-number
                (:eval (int-to-string (line-number-at-pos (point-max))))
                "  C%c  "                         ; %c = column number
                (vc-mode vc-mode)
                "  "
                (:propertize mode-name
                             face mode-line-buffer-id)
                minor-mode-alist
                mode-line-misc-info
                mode-line-end-spaces))


;;; [[<Global Requires]]

(require 'editorconfig)
(eval-when-compile
  (require 'subr-x))                              ; when-let, thread-last, string-remove-prefix, etc.


;;; [[<Global Minor Modes]]

;; Modes to disable
(blink-cursor-mode -1)                            ; disable annoying blinking cursor

;; Modes to enable
(mapc (lambda (mode)
        (funcall mode 1))
      '(delete-selection-mode                     ; typing will delete selected text
        global-anzu-mode                          ; show number of matches in mode-line while searching
        global-auto-revert-mode                   ; automatically reload files when they change on disk
        global-diff-hl-mode
        global-undo-tree-mode
        guide-key-mode
        projectile-global-mode
        ido-mode
        ido-everywhere                            ; use ido for all buffer/file reading
        ido-vertical-mode
        rainbow-mode                              ; colorize strings like #224499
        save-place-mode                           ; automatically save last place in files; reopen at that position
        winner-mode))

;; for some obnoxious reason there's no global-rainbow-mode so this will have to suffice
(add-hook 'find-file-hook (lambda ()
                            (rainbow-mode 1)))


;;; [[<Diminished Minor Modes]]

;; hide minor modes that are always going to be loaded
(mapc #'diminish
      '(anzu-mode
        diff-hl-mode
        guide-key-mode
        rainbow-mode
        undo-tree-mode))

;; for minor modes that get selectively loaded add a hook to diminish them after they're enabled
(mapc (lambda (hook.mode)
        (add-hook (car hook.mode) (lambda ()
                                    (diminish (cdr hook.mode)))))
      '((button-lock-mode-hook           . button-lock-mode)
        (highlight-parentheses-mode-hook . highlight-parentheses-mode)
        (wiki-nav-mode-hook              . wiki-nav-mode)))


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

      custom-file (expand-file-name               ; write customizations to ~/.emacs.d/custom.el instead of init.el
                   (concat user-emacs-directory
                           "custom.el"))
      echo-keystrokes 0.1                         ; show keystrokes in progress in minibuffer after 0.1 seconds instead of 1 second
      global-auto-revert-non-file-buffers t       ; also auto-revert buffers like dired
      ns-right-command-modifier 'hyper
      ns-right-control-modifier 'hyper
      ns-right-option-modifier 'alt
      require-final-newline t                     ; add final newline on save
      revert-without-query '(".*")                ; tell revert-buffer to revert all buffers without confirmation
      save-interprogram-paste-before-kill t       ; Save clipboard strings (from other applications) into kill-ring before replacing them
      savehist-mode t                             ; Periodically save minibuffer history
      select-enable-clipboard t                   ; Cutting and pasting uses the clipboard
      shift-select-mode nil                       ; real Emacs users don't use shift-selection
      vc-make-backup-files t                      ; Make backups of files even if they're under VC
      visible-bell t)

(setq-default indent-tabs-mode nil                ; disable insertion of tabs
              truncate-lines t)                   ; don't display "continuation lines" (don't wrap long lines)


;;; [[<auto-mode-alist]]
;;; Tell Emacs how to open files with certain extensions
(mapc (apply-partially #'add-to-list 'auto-mode-alist)
      '(("\.html$" . web-mode)
        ("\.js$"   . js2-mode)))


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

(defmacro cam/suppress-messages (&rest body)
  `(cl-letf (((symbol-function 'message) (lambda (&rest _))))
     ,@body))
(put #'cam/suppress-messages 'lisp-indent-function 0)


;;; [[<Global Hooks]]

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)
            (set-buffer-file-coding-system 'utf-8-auto-unix)))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ; if we're saving a script, give it execute permissions



;;; [[<Global Keybindings]]

(mapc (lambda (key-command-pair)
        (global-set-key (kbd (car key-command-pair)) (eval (cdr key-command-pair))))
      '(("<A-escape>"    . #'helm-mark-ring)
        ("<A-return>"    . #'wiki-nav-ido)
        ("<C-M-s-down>"  . #'windmove-down)
        ("<C-M-s-left>"  . #'windmove-left)
        ("<C-M-s-right>" . #'windmove-right)
        ("<C-M-s-up>"    . #'windmove-up)
        ("<H-SPC>"       . #'mc/mark-all-like-this)
        ("<H-escape>"    . #'ace-jump-line-mode)
        ("<H-return>"    . #'mc/mark-next-lines)
        ("<escape>"      . #'ace-jump-mode)
        ("<f11>"         . nil)                   ; Use <f11> <key> for toggling various minor modes
        ("<f11> a"       . #'aggressive-indent-mode)
        ("<f11> p"       . #'paredit-mode)
        ("<f11> w"       . #'whitespace-mode)
        ("A-;"           . #'cam/loccur)
        ("A-r l"         . #'rotate-layout)
        ("A-r w"         . #'rotate-window)
        ("C-="           . #'magit-status)
        ("C-M-y"         . #'helm-show-kill-ring)
        ("C-M-S-k"       . #'backward-kill-sexp)
        ("C-S-k"         . #'cam/backward-kill-line)
        ("C-c C-g"       . #'keyboard-quit)
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
        ("H-M-a"         . #'mc/skip-to-previous-like-this)
        ("H-M-e"         . #'mc/skip-to-next-like-this)
        ("H-a"           . #'mc/mark-previous-like-this)
        ("H-e"           . #'mc/mark-next-like-this)
        ("M-g"           . #'goto-line)           ; Instead of 'M-g g' for goto-line, since I don't really use anything else with the M-g prefix
        ("M-j"           . #'cam/join-next-line)
        ("M-x"           . #'helm-M-x)
        ("M-z"           . #'ace-jump-zap-up-to-char)
        ("s-Z"           . #'undo-tree-redo)
        ("s-f"           . #'ftf-grepsource)
        ("s-o"           . #'ftf-find-file)))


;;; ---------------------------------------- [[<Mode/Package Specific Setup]] ----------------------------------------

;;; [[<Lisp Modes]]
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


;;; [[<auto-complete]]
(eval-after-load 'auto-complete
  '(cam/suppress-messages
     (require 'pos-tip)

     (setq ac-delay 0.05
           ac-auto-show-menu 0.1
           ac-candidate-menu-height 30
           ac-menu-height 30                      ; show 20 results instead of 10
           ac-quick-help-prefer-pos-tip t         ; use native tooltips provided by pos-tip
           ac-quick-help-delay 0.2
           ac-use-menu-map t)                     ; enable mode-map when AC menu is visible

     (ac-config-default)

     (nconc ac-modes '(cider-repl-mode
                       ielm-mode))

     ;; Define some keybindings that will allow use to filter ac results by type
     (define-key ac-menu-map (kbd "A-f") #'ac-complete-functions)
     (define-key ac-menu-map (kbd "A-s") #'ac-complete-symbols)
     (define-key ac-menu-map (kbd "A-v") #'ac-complete-variables)))


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
(eval-after-load 'cider
  '(advice-add #'cider-repl-return :before (lambda ()
                                             (interactive)
                                             (call-interactively #'delete-trailing-whitespace))))

(defun cam/cider-repl-messages-buffer ()
  (let ((messages-buffer nil))
    (mapc (lambda (buf)
            (unless messages-buffer
              (when (string-match-p "^\*nrepl-server .*\*$" (buffer-name buf))
                (setq messages-buffer buf))))
          (buffer-list))
    messages-buffer))


;;; [[<company]]
(eval-after-load 'company
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
  (eval-buffer)
  (when-let ((ielm-window (get-window-with-predicate (lambda (window)
                                                       (string= (buffer-name (window-buffer window)) "*ielm*")))))
    (select-window ielm-window)
    (comint-clear-buffer)
    (comint-kill-input)))

(defun cam/emacs-lisp-mode-setup ()
  (require 'subr-x) ; when-let, etc.
  (cam/lisp-mode-setup)
  (aggressive-indent-mode 1)
  (cam/suppress-messages
    (auto-complete-mode 1))
  (elisp-slime-nav-mode 1)
  (morlock-mode 1)
  (wiki-nav-mode 1)

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
  (elisp-slime-nav-mode 1)
  (morlock-mode 1)

  (setq-local indent-line-function #'lisp-indent-line)) ; automatically indent multi-line forms correctly
(add-hook 'ielm-mode-hook #'cam/ielm-mode-setup)

(eval-after-load 'lisp-mode
  '(progn (put #'advice-add 'lisp-indent-function 2)))


;;; [[<Eval Expresssion (Minibuffer)]]
(defun cam/eval-expression-minibuffer-setup ()
  (company-mode 1)
  (paredit-mode 1)
  (setq-local company-echo-delay 10))
(add-hook 'eval-expression-minibuffer-setup-hook #'cam/eval-expression-minibuffer-setup)


;;; [[<Find Things Fast]]
(eval-after-load 'find-things-fast
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
                                     "M-o"))


;;; [[<Helm]]
(eval-after-load 'helm-command
  '(setq helm-M-x-fuzzy-match t))

(eval-after-load 'helm-files
  '(setq helm-recentf-fuzzy-match t))

(eval-after-load 'helm-buffers
  '(setq helm-buffers-fuzzy-matching t))


;;; [[<js2-mode]]
(defun cam/js2-mode-setup ()
  (electric-pair-mode 1)
  (rainbow-delimiters-mode 1)
  (define-key js2-mode-map
    (kbd "C-j") #'newline))                       ; instead of electrict-newline-maybe-indent, which doesn't indent
(add-hook 'js2-mode-hook #'cam/js2-mode-setup)


;;; [[<Magit]]
(defun cam/magit-visit-pull-request-url ()
  "Visit the current git branch's PR on GitHub."
  (interactive)
  (browse-url (concat "http://github.com/"
                      (thread-last (magit-get "remote" (magit-get-current-remote) "url")
                        (string-remove-suffix ".git")
                        (string-remove-prefix "git@github.com:"))
                      "/pull/"
                      (magit-get-current-branch))))

(defun cam/magit-status-mode-setup ()
  (define-key magit-status-mode-map
    (kbd "V") #'cam/magit-visit-pull-request-url))
(add-hook 'magit-status-mode-hook #'cam/magit-status-mode-setup)

(setq magit-auto-revert-mode-lighter     ""
      magit-last-seen-setup-instructions "1.4.0")


;;; [[<Org]]
(defun cam/org-mode-setup ()
  (setq-local truncate-lines nil))
(add-hook 'org-mode-hook #'cam/org-mode-setup)

(eval-after-load 'org
  '(progn
     (setq org-support-shift-select nil)))


;;; [[<Paredit]]
(eval-after-load 'paredit
  ;; Tell paredit it's ok to delete selection in these contexts. Otherwise delete-selection-mode doesn't work with paredit
  '(progn (put #'paredit-forward-delete  'delete-selection 'supersede)
          (put #'paredit-backward-delete 'delete-selection 'supersede)
          (put #'paredit-open-round      'delete-selection t)
          (put #'paredit-open-square     'delete-selection t)
          (put #'paredit-doublequote     'delete-selection t)
          (put #'paredit-newline         'delete-selection t)))


;;; [[<Web Mode]]
(defun cam/web-mode-setup ()
  (aggressive-indent-mode 1)
  (electric-pair-local-mode 1)
  (rainbow-delimiters-mode 1)

  (define-key web-mode-map
    (kbd "C-j") #'newline))                       ; instead of electric-newline-and-maybe-indent which doesn't indent :/
(add-hook 'web-mode-hook #'cam/web-mode-setup)


;;; ---------------------------------------- [[<Final Setup]] ----------------------------------------

;; Byte-compile init.el if needed for next time around
(let* ((init-file (expand-file-name (concat user-emacs-directory "init.el"))) ; don't use var user-init-file because it will be set to the .elc file while loading
       (compiled-init-file (concat init-file "c")))
  (when (or (not compiled-init-file)
            (file-newer-than-file-p init-file compiled-init-file))
    (byte-compile-file init-file)))

(ignore-errors
  (load-file custom-file))

(unless cam/has-loaded-init
  (toggle-frame-maximized))

(setq cam/has-loaded-init t)

(message "Loaded init.el in %.0f ms." (* (float-time (time-subtract after-init-time before-init-time)) 1000.0))
