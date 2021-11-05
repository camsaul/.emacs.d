;;; -*- lexical-binding: t; coding: utf-8; comment-column: 60; no-byte-compile: nil; -*-

;;; TOC:
;;; [[Initial Setup]]
;;;    [[Additional Init File Setup & Compiliation]]
;;; [[Package Setup]]
;;; [[Global Setup]]
;;;    [[Theme]]
;;;    [[Autoloads]]
;;;    [[Global Settings]]
;;;    [[Global Macros]]
;;;    [[Global Functions]]
;;;    [[Global Hooks]]
;;;    [[Global Keybindings]]
;;; [[Mode/Package Specific Setup]]
;;;    [[eval-after-load]]
;;;    [[auto-mode-alist]]
;;;    [[magic-mode-alist]]
;;;    [[interpreter-mode-alist]]
;;; [[Global Minor Modes]]
;;; [[Diminished Minor Modes]]
;;; [[Final Setup]]

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)                                         ; when-let, thread-last, string-remove-prefix, etc.
  (require 'cam-macros))

;;; ---------------------------------------- [[<Initial Setup]] ----------------------------------------

(defvar cam/has-loaded-init-p nil
  "Have we done a complete load of the init file yet? (Use this to
  keep track of things we only want to run once, but not again if
  we call eval-buffer).")

(setq inhibit-splash-screen t
      inhibit-startup-screen t)

;; In an effort to be really annoying you can only suppress the startup echo area message if you set it through
;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message (user-login-name)))

;;; [[<Additional Init File Setup & Compiliation]]

(defconst cam/-user-source-directory
  (expand-file-name (concat user-emacs-directory "lisp/"))
  "Directory where other init files live. ~/.emacs.d/lisp")

(defconst cam/autoloads-file
  (expand-file-name (concat cam/-user-source-directory "loaddefs.el")))

(add-to-list 'load-path cam/-user-source-directory t)       ; t = append

;; compile init files if they haven't already been compiled.
(defun cam/-init-files ()
  "Paths of the misc .el init files. init.el and ~/.emacs.d/lisp"
  (cons
   (expand-file-name (concat user-emacs-directory "init.el"))
   (mapcar (lambda (file)
             (expand-file-name (concat cam/-user-source-directory file)))
           (cl-remove-if (lambda (file)
                           (or (not (string-suffix-p ".el" file))
                               (string-prefix-p ".#" file)
                               (string-equal file "loaddefs.el")))
                         (directory-files cam/-user-source-directory)))))

(defun cam/-byte-compile-init-files-and-generate-autoloads (&optional force-update-autoloads)
  (unless (file-exists-p cam/autoloads-file)
    (setq force-update-autoloads t))
  (dolist (file (cam/-init-files))
    (message "Compile %s if needed..." file)
    ;; nil = don't force recompilation if .elc is newer
    ;; 0 = compile file if .elc didn't already exist
    (when (or (not (eq (byte-recompile-file file nil 0) 'no-byte-compile))
              force-update-autoloads)
      (message "Updating autoloads for %s" file)
      ;; t = save the autoloads file when finished.
      (update-file-autoloads file t cam/autoloads-file))))

;; compile all the init files as needed and generate autoloads
(cam/-byte-compile-init-files-and-generate-autoloads)

(load-file cam/autoloads-file)


;;; ---------------------------------------- [[<Package Setup]] ----------------------------------------

(require 'package)
(package-initialize)

(defconst cam/-package-archives
  '(("gnu"          . "https://elpa.gnu.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa"        . "https://melpa.org/packages/")
    ("org"          . "https://orgmode.org/elpa/")))

(setq package-archives cam/-package-archives)

(defconst cam/-packages
  '(ace-jump-mode
    ace-window                                              ; Ace jump to other windows
    aggressive-indent                                       ; Minor mode to aggressively keep code always indented
    anzu                                                    ; Show number of matches in mode-line while searching
    auto-complete                                           ; auto-completion
    auto-compile                                            ; automatically byte-compile Emacs Lisp files
    cider                                                   ; Clojure Interactive Development Environment that Rocks
    column-enforce-mode                                     ; Highlight text that goes past a certain column limit
    clj-refactor                                            ; Clojure refactoring minor mode
    clojure-mode-extra-font-locking
    cmake-mode                                              ; Major mode for editing CMake files
    company                                                 ; auto-completion
    cperl-mode                                              ; Better than perl-mode
    dash
    diff-hl                                                 ; mark uncommited changes in the fringe
    diminish                                                ; Replace or hide minor modes in mode-line
    dockerfile-mode                                         ; Major mode for editing Dockerfiles
    editorconfig                                            ; Read EditorConfig files
    elisp-slime-nav                                         ; Make M-. and M-, work in elisp like the do in slime
    ert                                                     ; Emacs Lisp Regression Testing
    esup                                                    ; Emacs Start-Up Profiler <3
    find-things-fast
    flycheck                                                ; on-the-fly syntax checking
    flyspell                                                ; spell checking
    git-timemachine                                         ; Walk through git revisions of a file
    gitconfig-mode
    gitignore-mode                                          ; Major mode for editing .gitignore files
    guide-key
    helm
    highlight-parentheses                                   ; highlight matching parentheses
    ido-vertical-mode
    loccur
    macrostep                                               ; Interactive macrostepper for Emacs Lisp
    magit
    markdown-mode                                           ; Major mode for editing markdown files
    morlock                                                 ; Extra font-locking for Emacs Lisp
    multiple-cursors
    moe-theme
    org                                                     ; Get latest version of org from Org package archive
    paredit
    projectile
    rainbow-delimiters
    rainbow-mode
    register-list                                           ; dired-like editing of Emacs registers
    saveplace                                               ; save position of point when killing a buffer
    sly                                                     ; Common Lisp
    rotate                                                  ; rotate-window, rotate-layout, etc.
    undo-tree
    web-mode                                                ; major-mode for editing web templates
    wiki-nav                                                ; Navigate a file using [[WikiStrings]]
    yaml-mode

    evil
    evil-matchit
    powerline
    powerline-evil
    smartparens
    evil-smartparens
    evil-cleverparens

    ;; new JS stuff
    lsp-mode
    company-lsp
    tide

    ;; new C++ stuff
    rtags
    cmake-ide
    ;; I'm not sure how many of these we actually need/want
    company-rtags
    disaster
    eglot
    eldoc-box
    cpp-auto-include

    racket-mode

    unicode-fonts

    ;; more stuff that's experimental
    company-posframe
    company-quickhelp
    vterm
    ))

;;; Install packages as needed
(defvar cam/has-refreshed-package-contents-p nil)
(dolist (package cam/-packages)
  (unless (package-installed-p package)
    (unless cam/has-refreshed-package-contents-p
      (ignore-errors
        (package-refresh-contents))
      (setq cam/has-refreshed-package-contents-p t))
    (condition-case err
        (package-install package)
      (error (warn (concat "Failed to install package " (symbol-name package) ": " (error-message-string err)))))))

;;; ---------------------------------------- [[<Global Setup]] ----------------------------------------

;;; [[<Theme]]

;; Load the theme just once, otherwise the screen will flicker all cray if we try to eval this buffer again
(require 'cam-theme)
(unless cam/has-loaded-init-p
  (cam/setup-frame))

;;; [[<Autoloads]]

(autoload 'describe-minor-mode "help")
(autoload 'evil-normal-state   "evil")
(autoload 'loccur              "loccur")

;;; [[<Global Settings]]

(fset #'yes-or-no-p #'y-or-n-p)                             ; Prompt for 'y' or 'n' instead of 'yes' or 'no'

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8-auto-unix)

(mapatoms (lambda (atom)                                    ; Enable all disabled functions
            (when (get atom 'disabled)
              (put atom 'disabled nil))))

(setq backup-directory-alist                                ; save backup files to ~/.emacs.d/backups
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory
                         "backups"))))
      create-lockfiles nil                                  ; don't create .#<filename> lockfiles -- we're not in a shared system

      custom-file (expand-file-name
                   (concat user-emacs-directory
                           "custom.el"))

      echo-keystrokes 0.1                                   ; show keystrokes in progress in minibuffer after 0.1 seconds instead of 1 second
      frame-resize-pixelwise t                              ; maximize as much as possible rather than rounding to closest whole line
      garbage-collection-messages t                         ; Show messages when garbage collection occurs so we don't set the GC threshold too high and make Emacs laggy
      global-auto-revert-non-file-buffers t                 ; also auto-revert buffers like dired
      next-line-add-newlines t                              ; C-n (#'next-line) will add a newline at the end of the buffer instead of giving you an error
      read-process-output-max (* 1024 1024)                 ; read data in up to 1MB chunks from subprocesses rather than 4kb chunks.
      recentf-max-menu-items 50                             ; show more recent files in [Helm]recentf
      recentf-max-saved-items 50
      require-final-newline t                               ; add final newline on save
      revert-without-query '(".*")                          ; tell revert-buffer to revert all buffers without confirmation
      save-interprogram-paste-before-kill t                 ; Save clipboard strings (from other applications) into kill-ring before replacing them
      select-enable-clipboard t                             ; Cutting and pasting uses the clipboard
      sentence-end-double-space nil                         ; A single space should be considered finished even if there's only one space after the period for filling purposes.
      shift-select-mode nil                                 ; this is not an actual minor mode despite the name
      vc-make-backup-files t                                ; Make backups of files even if they're under VC
      visible-bell t                                        ; Show a visual overlay instead of beeping when doing something like trying to scroll up at top of file

      ;; EXPERIMENTAL !
      ;; case-fold-search t                                    ; cases and matches should ignore case (TODO -- I think this is the default)
      )

(cond
 ;; windows only
 ((eq window-system 'w32)
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-apps-modifier 'alt
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'hyper))
 ;; macOS only
 ((eq window-system 'ns)
  (setq ns-right-command-modifier 'hyper
        ns-right-control-modifier 'hyper
        ns-right-option-modifier 'alt)))

(setq-default display-line-numbers nil                      ; don't show line numbers.
              display-line-numbers-widen t                  ; displaying line numbers should disregard narrowing.
              fill-column 118                               ; My screen can handle more than 70 characters; use 118 so GH won't cut it off
              indent-tabs-mode nil                          ; disable insertion of tabs
              truncate-lines t)                             ; don't display "continuation lines" (don't wrap long lines)


;; (add-to-list 'projectile-globally-ignored-files   ; Tell projectile to always ignore uberdoc.html
;;              "uberdoc.html")

;;; [[<Global Hooks]]

(add-hook 'before-save-hook
  (lambda ()
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8-auto-unix)))

;; if we're saving a script, give it execute permissions
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; [[<Global Keybindings]]

(cam/global-set-keys
  ("<A-escape>"    . #'helm-mark-ring)
  ("<A-return>"    . #'wiki-nav-ido)
  ("<f2>"          . #'mc/mark-next-like-this)
  ("<M-f2>"        . #'mc/mark-next-lines)
  ("<C-f2>"        . #'mc/mark-all-like-this)
  ;; ("<C-M-return>"  . #'ace-jump-line-mode)
  ("<C-return>"    . #'ace-jump-mode)
  ("<H-SPC>"       . #'mc/mark-all-like-this)
  ("<H-return>"    . #'mc/mark-next-lines)
  ("<M-return>"    . #'ace-window)
  ("<S-backspace>" . #'cam/hungry-delete-backward)
  ("<S-delete>"    . #'cam/hungry-delete-forward)
  ("<S-down>"      . #'windmove-down)
  ("<S-left>"      . #'cam/windmove-left-or-other-frame)
  ("<S-right>"     . #'cam/windmove-right-or-other-frame)   ; Use <f11> <key> for toggling various minor modes
  ("<S-up>"        . #'windmove-up)
  ("<escape>"      . #'evil-normal-state)
  ("<f11>"         . nil)
  ("<f11> a"       . #'aggressive-indent-mode)
  ("<f11> e"       . #'evil-mode)
  ("<f11> p"       . #'paredit-mode)
  ("<f11> r"       . #'read-only-mode)
  ("<f11> w"       . #'whitespace-mode)
  ("<f12> d"       . #'cam/duckduckgo-search)
  ("<f5>"          . #'ftf-find-file)                       ; alternate bindings since super modifier doesn't work well on Windows
  ("<f6>"          . #'ftf-grepsource)
  ("<insert>"      . nil)
  ("<scroll>"      . #'ftf-find-file)                       ; for Windows use scroll to open file since s-o doesn't work
  ("A-;"           . #'loccur)
  ("A-e"           . #'cam/insert-em-dash)
  ("A-r l"         . #'rotate-layout)
  ("A-r w"         . #'rotate-window)
  ("C-="           . #'magit-status)
  ("C-M-S-k"       . #'backward-kill-sexp)
  ("C-M-y"         . #'helm-show-kill-ring)
  ("C-S-k"         . #'cam/backward-kill-line)
  ("C-c C-g"       . #'keyboard-quit)
  ("C-h M"         . #'describe-minor-mode)
  ("C-x C-b"       . #'helm-buffers-list)
  ("C-x C-d"       . #'dired)                               ; instead of ido-list-directory
  ("C-x C-f"       . #'helm-find-files)
  ("C-x C-g"       . #'keyboard-quit)
  ("C-x C-q"       . nil)                                   ; remove keybinding for read-only-mode since I almost never press it on purpose
  ("C-x C-r"       . #'helm-recentf)
  ("C-x C-z"       . nil)                                   ; instead of suspend-frame
  ("C-x b"         . #'helm-buffers-list)
  ("C-x f"         . #'helm-find-files)
  ("C-x k"         . #'kill-this-buffer)
  ("C-x r r"       . #'register-list)                       ; replaces copy-rectangle-to-register
  ("C-x w"         . nil)
  ("C-x w ."       . #'highlight-symbol-at-point)           ; this is the normal binding for this function but isn't added until `hi-lock.el` is loaded
  ("C-z"           . #'evil-normal-state)
  ;; ("ESC <up>"      . #'windmove-up)
  ("H-;"           . #'cam/realign-eol-comments)
  ("H-M-a"         . #'mc/skip-to-previous-like-this)
  ("H-M-e"         . #'mc/skip-to-next-like-this)
  ("H-a"           . #'mc/mark-previous-like-this)
  ("H-e"           . #'mc/mark-next-like-this)
  ("M-/"           . #'hippie-expand)                       ; Instead of dabbrev-expand
  ("M-:"           . #'pp-eval-expression)                  ; Instead of regular eval-expression
  ("M-g"           . #'goto-line)                           ; Instead of 'M-g g' for goto-line, since I don't really use anything else with the M-g prefix
  ("M-j"           . #'cam/join-next-line)
  ("M-x"           . #'helm-M-x)
  ("s-;"           . #'cam/insert-spaces-to-goal-column)
  ("s-Z"           . #'undo-tree-redo)
  ("s-f"           . #'ftf-grepsource)
  ("s-o"           . #'ftf-find-file))

;;; ---------------------------------------- [[<Mode/Package Specific Setup]] ----------------------------------------

;;; [[<eval-after-load]]
(defconst cam/-require-after-loads
  '((auto-complete       cam-auto-complete)
    (cc-mode             cam-c cam-cplusplus cam-java cam-objective-c)
    (clojure-mode        cam-clojure)
    (column-enforce-mode cam-column-enforce)
    (company             cam-company)
    (cperl-mode          cam-perl)
    (css-mode            cam-css)
    (dired               cam-dired)
    (elisp-mode          cam-emacs-lisp)
    (evil                cam-evil)
    (find-things-fast    cam-ftf)
    (guide-key           cam-guide-key)
    (helm                cam-helm)
    (ielm                cam-emacs-lisp)
    (magit               cam-magit)
    (nxml-mode           cam-nxml)
    (org                 cam-org)
    (paredit             cam-paredit)
    (racket-mode         cam-racket)
    (simple              cam-eval-expr cam-messages-mode)
    (sly                 cam-common-lisp)
    (web-mode            cam-web)))

(dolist (spec cam/-require-after-loads)
  (cl-destructuring-bind (file . requires) spec
    (eval-after-load file
      `(progn ,@(mapcar (lambda (file-to-require)
                          `(require ',file-to-require))
                        requires)))))

;;; [[<auto-mode-alist]]
(defconst cam/-auto-mode-patterns
  '((clojurescript-mode "\\.cljs$")
    (cperl-mode         "\\.pl$" "\\.pm$")
    (emacs-lisp-mode    "\\.el$")
    (nxml-mode          "\\.pml$")
    (web-mode           "\\.html$" "\\.js$" "\\.json$" "\\.jsx$" "\\.mustache$")))

(dolist (spec cam/-auto-mode-patterns)
  (cl-destructuring-bind (mode . patterns) spec
    (dolist (pattern patterns)
      (add-to-list 'auto-mode-alist (cons pattern mode)))))

;; [[<magic-mode-alist]]
;; Automatically open .h files with @interface declarations as obj-c rather than c
(add-to-list 'magic-mode-alist
             (cons (lambda ()
                     (and buffer-file-name
                          (string-equal (file-name-extension buffer-file-name) "h")
                          (re-search-forward "@\\<interface\\>"
                                             magic-mode-regexp-match-limit t)))
                   'objc-mode))

;;; [[<interpreter-mode-alist]]
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))


;; TODO FIXME -- these should all be put in their own files under lisp/
(eval-when-compile
  (require 'cam-tweak-package))

;; (cam/tweak-package git-commit
;;   :mode-name git-commit
;;   :minor-modes (flyspell-mode))

(cam/tweak-package markdown-mode
  :mode-name markdown-mode
  :minor-modes (flyspell-mode))

(cam/tweak-package python
  :minor-modes (electric-pair-local-mode))

(cam/tweak-package sh-script
  :mode-name sh-mode
  :minor-modes (electric-pair-local-mode
                cam/todo-font-lock-mode)
  :keys (("C-j" . #'newline)))

(cam/tweak-package text-mode
  :mode-name text-mode
  :minor-modes (flyspell-mode))

(cam/tweak-package yaml-mode
  :mode-name yaml-mode
  :keys (("C-j" . #'newline-and-indent)
         ("C-m" . #'newline-and-indent)))

(cam/tweak-package help-mode
  :mode-name help-mode
  :minor-modes (rainbow-mode))

(cam/tweak-package prog-mode
  :mode-name prog-mode
  :minor-modes (hl-line-mode)
  :local-vars ((show-trailing-whitespace . t))
  )

(with-eval-after-load 'help-mode
  (add-hook 'help-mode-hook #'rainbow-mode))

;;; [[<Global Minor Modes]]

(defconst cam/-global-disabled-minor-modes
  '(blink-cursor-mode
    ;; Don't show toolbar, scrollbar, splash screen, startup screen
    scroll-bar-mode
    tool-bar-mode))

;; don't disable menu bar more on macOS.
(unless (eq window-system 'ns)
  (add-to-list 'cam/-global-disabled-minor-modes 'menu-bar-mode))

(dolist (mode cam/-global-disabled-minor-modes)
  ;; can't do (funcall (symbol-function mode) -1) here because it doesn't work for autoloads.
  (eval `(,mode -1)))

(defconst cam/-global-enabled-minor-modes
  '(delete-selection-mode                                   ; typing will delete selected text
    editorconfig-mode                                       ; parse .editorconfig files and apply settings for things like indentation
    global-anzu-mode                                        ; show number of matches in mode line while searching
    global-auto-revert-mode                                 ; automatically reload files when they change on disk
    global-diff-hl-mode                                     ; Show which lines have changed since last git commit in the fringe
    global-eldoc-mode                                       ; Automatically enable eldoc-mode in any buffers possible. Display fn arglists / variable dox in minibuffer
    global-so-long-mode                                     ; don't die when handling files with really long lines
    global-undo-tree-mode
    guide-key-mode                                          ; Show list of completions for keystrokes after a delay
    ido-mode
    ido-everywhere
    ido-vertical-mode
    ;; projectile-mode
    rainbow-mode                                            ; Colorize strings like #FCE94F
    recentf-mode                                            ; Track recently visited files
    savehist-mode                                           ; Periodically save minibuffer history
    save-place-mode                                         ; automatically save position in files & start at that position next time you open them
    winner-mode))

(dolist (mode cam/-global-enabled-minor-modes)
  (eval `(,mode 1)))

;; for some obnoxious reason there's no global-rainbow-mode so this will have to suffice
(add-hook 'find-file-hook #'rainbow-mode)

;;; [[<Diminished Minor Modes]]

(require 'diminish)

(defconst cam/-diminished-minor-modes
  '(anzu-mode
    button-lock-mode
    diff-hl-mode
    editorconfig-mode
    global-auto-revert-mode
    guide-key-mode
    highlight-parentheses-mode
    projectile-mode
    rainbow-mode
    undo-tree-mode
    wiki-nav-mode))

(dolist (mode cam/-diminished-minor-modes)
  (diminish mode))

;;; ---------------------------------------- [[<Final Setup]] ----------------------------------------

(ignore-errors
  (load-file custom-file))

(setq cam/has-loaded-init-p t)

;; TODO -- consider whether this should be in an `emacs-startup-hook'
(message "Loaded init.el in %.0f ms." (* (float-time (time-subtract after-init-time before-init-time)) 1000.0))
