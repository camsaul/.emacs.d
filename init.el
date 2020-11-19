;;; -*- lexical-binding: t; coding: utf-8; cam/byte-compile: t; comment-column: 50; -*-

;;; TOC:
;;; [[Initial Setup]]
;;; [[Package Setup]]
;;; [[Global Setup]]
;;;    [[Theme]]
;;;    [[Global Requires]]
;;;    [[Autoloads]]
;;;    [[Global Settings]]
;;;    [[Global Macros]]
;;;    [[Global Functions]]
;;;    [[Global Hooks]]
;;;    [[Global Keybindings]]
;;; [[Mode/Package Specific Setup]]
;;;    [[etc]]
;;;    [[Lisp Modes]]
;;;    [[auto-complete]]
;;;    [[C/C++]]
;;;    [[Clojure]]
;;;    [[dired]]
;;;    [[company]]
;;;    [[column-enforce-mode]]
;;;    [[css-mode]]
;;;    [[Emacs Lisp]]
;;;    [[Eval Expresssion (Minibuffer)]]
;;;    [[Find Things Fast]]
;;;    [[Git Commit Mode]]
;;;    [[Guide Key]]
;;;    [[Helm]]
;;;    [[Java]]
;;;    [[loccur]]
;;;    [[Magit]]
;;;    [[markdown]]
;;;    [[Messages]]
;;;    [[Objective-C]]
;;;    [[Org]]
;;;    [[Paredit]]
;;;    [[Perl]]
;;;    [[Python]]
;;;    [[Racket]]
;;;    [[Shell]]
;;;    [[Sly]]
;;;    [[(Common) Lisp Mode]]
;;;    [[text-mode]]
;;;    [[Web Mode]]
;;;    [[(n)xml Mode]]
;;;    [[YAML Mode]]
;;; [[Global Minor Modes]]
;;; [[Diminished Minor Modes]]
;;; [[Powerline & Evil Mode]]
;;; [[Experimental]]
;;; [[Final Setup]]

;;; ---------------------------------------- [[<Initial Setup]] ----------------------------------------
;;; (Things that need to happen as soon as this file starts loading)

(setq
 ;; By default GC starts around ~780kB. Since this isn't the 90s GC when we hit 128MB
 gc-cons-threshold (* 128 1024 1024)
 ;; load .el files if they're newer than .elc ones
 load-prefer-newer t)

;;; Don't show toolbar, scrollbar, splash screen, startup screen

(dolist (mode '(scroll-bar-mode tool-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(unless (eq window-system 'ns)
  (menu-bar-mode -1))

(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(let ((scratch-buffer (get-buffer "*scratch")))
  (when scratch-buffer
    (kill-buffer scratch-buffer)))

;; In an effort to be really annoying you can only suppress the startup echo area message if you set it through
;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message (user-login-name)))

(add-to-list 'safe-local-variable-values '(cam/byte-compile . t))
(add-to-list 'safe-local-variable-values '(cam/generate-autoloads . t))

(defvar cam/has-loaded-init-p nil
  "Have we done a complete load of the init file yet? (Use this to keep track of things we only want to run once, but
  not again if we call eval-buffer).")

;;; [[<Auxilary Init File Setup]]

;; Check to make sure init file is up-to-date
;; user-init-file is the .elc file when LOADING
(when (string-suffix-p "elc" user-init-file)
  (let ((init-file-source (concat user-emacs-directory "init.el")))
    (when (file-newer-than-file-p init-file-source user-init-file)
      ;; If not, trash .elc file and kill Emacs. We'll recompile on next launch
      (delete-file user-init-file)
      (while (not (y-or-n-p "init.el is out of date. We need to restart Emacs. Ready? ")))
      (kill-emacs))))

(add-to-list 'load-path (expand-file-name user-emacs-directory) :append)

(defconst cam/autoloads-file (concat user-emacs-directory "autoloads.el"))

;; byte recompile the other files in this dir if needed
(defconst cam/auxilary-init-files
  (eval-when-compile
    (let (files)
      (dolist (file (directory-files user-emacs-directory))
        (when (and (string-match "^[-[:alpha:]]+\\.el$" file)
                   (not (member file '("autoloads.el" "custom.el" "init.el"))))
          (push (concat user-emacs-directory file) files)))
      files)))

(eval-when-compile
  (dolist (file cam/auxilary-init-files)
    (unless (file-exists-p (concat file "c"))
      (byte-compile-file file :load)
      (update-file-autoloads file :save-after cam/autoloads-file))))

(ignore-errors
  (load-file cam/autoloads-file))


;;; ---------------------------------------- [[<Package Setup]] ----------------------------------------

(package-initialize)


(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")))

(defconst cam/packages
  '(ac-cider                                      ; auto-complete <-> cider
    ac-sly                                        ; auto-complete <-> sly
    ace-jump-mode
    ace-window                                    ; Ace jump to other windows
    ;; ace-jump-zap                                  ; ace-jump-mode version of zap-to-char / zap-up-to-char
    aggressive-indent                             ; Minor mode to aggressively keep code always indented
    anzu                                          ; Show number of matches in mode-line while searching
    ;; anything                                      ; prereq for perl-completion
    auto-complete                                 ; auto-completion
    cider                                         ; Clojure Interactive Development Environment that Rocks
    column-enforce-mode                           ; Highlight text that goes past a certain column limit
    clj-refactor                                  ; Clojure refactoring minor mode
    clojure-mode-extra-font-locking
    cmake-mode                                    ; Major mode for editing CMake files
    company                                       ; auto-completion
    cperl-mode                                    ; Better than perl-mode
    dash
    diff-hl                                       ; mark uncommited changes in the fringe
    diminish                                      ; Replace or hide minor modes in mode-line
    dockerfile-mode                               ; Major mode for editing Dockerfiles
    editorconfig                                  ; Read EditorConfig files
    elisp-slime-nav                               ; Make M-. and M-, work in elisp like the do in slime
    ert                                           ; Emacs Lisp Regression Testing
    esup                                          ; Emacs Start-Up Profiler <3
    ;; everything                                    ; Required by perl-completion-mode
    find-things-fast
    flycheck                                      ; on-the-fly syntax checking
    flyspell                                      ; spell checking
    git-timemachine                               ; Walk through git revisions of a file
    gitconfig-mode
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
    org                                           ; Get latest version of org from Org package archive
    paredit
    ;; perl-completion                               ; Auto-complete for Perl
    projectile
    rainbow-delimiters
    rainbow-mode
    register-list                                 ; dired-like editing of Emacs registers
    saveplace                                     ; save position of point when killing a buffer
    sly                                           ; Common Lisp
    rotate                                        ; rotate-window, rotate-layout, etc.
    undo-tree
    web-mode                                      ; major-mode for editing web templates
    wiki-nav                                      ; Navigate a file using [[WikiStrings]]
    yaml-mode

    evil
    evil-matchit
    powerline
    powerline-evil
    smartparens
    evil-smartparens
    evil-cleverparens

    rtags
    cmake-ide

    racket-mode

    unicode-fonts
    ))

;;; Install packages as needed
(defvar cam/has-refreshed-package-contents-p nil)
(dolist (package cam/packages)
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

(eval-when-compile
  (require 'moe-theme))
(require 'moe-theme)

;; Load the theme just once, otherwise the screen will flicker all cray if we try to eval this buffer again
(unless cam/has-loaded-init-p
  (moe-dark)
  (ignore-errors
    ;; t = apply font to all frames going forward & save setting to custom.el (supposedly)
    (set-frame-font "DejaVuSansMono-18" (not :keep-size) t)))

(defun cam/setup-frame ()
  (set-fringe-style '(6 . 0))                     ; ¾ width fringe on the left and none on the right
  (moe-theme-random-color)
  (set-face-foreground 'mode-line "#111111")
  (set-cursor-color (face-background 'mode-line))
  ;;  Don't show a blue background behind buffer name on modeline for deselected frames
  (set-face-background 'mode-line-buffer-id nil))

(advice-add #'make-frame-command :after #'cam/setup-frame)

(unless cam/has-loaded-init-p
  (cam/setup-frame))


;;; [[<Global Requires]]

(require 'editorconfig)
(require 'projectile)

(eval-when-compile
  (require 'cl-lib)
  (require 'dash)
  (require 'subr-x)) ; when-let, thread-last, string-remove-prefix, etc.


;;; [[<Autoloads]]

(autoload #'describe-minor-mode "help")
;; (autoload #'perl-completion-mode "perl-completion")


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
      create-lockfiles nil                       ; don't create .#<filename> lockfiles -- we're not in a shared system

      custom-file (expand-file-name
                   (concat user-emacs-directory
                           "custom.el"))

      echo-keystrokes 0.1                         ; show keystrokes in progress in minibuffer after 0.1 seconds instead of 1 second
      frame-resize-pixelwise t                    ; maximize as much as possible rather than rounding to closest whole line
      garbage-collection-messages t               ; Show messages when garbage collection occurs so we don't set the GC threshold too high and make Emacs laggy
      global-auto-revert-non-file-buffers t       ; also auto-revert buffers like dired
      helm-ff-skip-git-ignored-files t            ; Have C-x C-f skip files in .gitignore
      helm-ff-skip-boring-files t                 ; Have C-x C-f skip "boring" files matching the regex below
      helm-ff--boring-regexp (rx (or (and "." (or "d" "o" "pch" "class" "elc") eol) ; TODO -- this skips .emacs.d ??
                                     (and "~" eol)
                                     (and bol "#" (1+ anything) "#" eol)
                                     (and bol ".#")))
      next-line-add-newlines t                    ; C-n (#'next-line) will add a newline at the end of the buffer instead of giving you an error
      ns-right-command-modifier 'hyper
      ns-right-control-modifier 'hyper
      ns-right-option-modifier 'alt
      print-gensym t                              ; print uninterned symbols with prefixes to differentiate them from interned ones
      recentf-max-menu-items 50                   ; show more recent files in [Helm]recentf
      recentf-max-saved-items 50
      require-final-newline t                     ; add final newline on save
      revert-without-query '(".*")                ; tell revert-buffer to revert all buffers without confirmation
      save-interprogram-paste-before-kill t       ; Save clipboard strings (from other applications) into kill-ring before replacing them
      savehist-mode t                             ; Periodically save minibuffer history
      select-enable-clipboard t                   ; Cutting and pasting uses the clipboard
      sentence-end-double-space nil               ; A single space should be considered finished even if there's only one space after the period for filling purposes.
      shift-select-mode nil                       ; real Emacs users don't use shift-selection
      vc-make-backup-files t                      ; Make backups of files even if they're under VC
      visible-bell t                              ; Show a visual overlay instead of beeping when doing something like trying to scroll up at top of file
      ;; w32-pass-lwindow-to-system nil
      ;; w32-pass-rwindow-to-system nil
      ;; w32-apps-modifier 'alt
      ;; w32-lwindow-modifier 'super
      ;; w32-rwindow-modifier 'hyper

      ;; EXPERIMENTAL !
      case-fold-search t                          ; cases and matches should ignore case (TODO -- I think this is the default)

      )

(setq-default fill-column 118                     ; My screen can handle more than 70 characters; use 118 so GH won't cut it off
              indent-tabs-mode nil                ; disable insertion of tabs
              save-place t                        ; Automatically save place in each file
              truncate-lines t)                   ; don't display "continuation lines" (don't wrap long lines)

(add-to-list 'projectile-globally-ignored-files   ; Tell projectile to always ignore uberdoc.html
             "uberdoc.html")


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
  ("<S-right>"     . #'cam/windmove-right-or-other-frame)           ; Use <f11> <key> for toggling various minor modes
  ("<S-up>"        . #'windmove-up)
  ("<escape>"      . #'evil-normal-state)
  ("<f11>"         . nil)
  ("<f11> a"       . #'aggressive-indent-mode)
  ("<f11> e"       . #'evil-mode)
  ("<f11> p"       . #'paredit-mode)
  ("<f11> r"       . #'read-only-mode)
  ("<f11> w"       . #'whitespace-mode)
  ("<f12> d"       . #'cam/duckduckgo-search)
  ("<f12> i"       . #'cam/instant-clojure-cheatsheet-search)
  ("<f12> j"       . #'cam/javadocs-search)
  ("<f5>"          . #'ftf-find-file)                               ; alternate bindings since super modifier doesn't work well on Windows
  ("<f6>"          . #'ftf-grepsource)
  ("<insert>"      . nil)
  ("<scroll>"      . #'ftf-find-file)                               ; for Windows use scroll to open file since s-o doesn't work
  ("A-;"           . #'cam/loccur)
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
  ("C-x C-d"       . #'dired)                                       ; instead of ido-list-directory
  ("C-x C-f"       . #'helm-find-files)
  ("C-x C-g"       . #'keyboard-quit)
  ("C-x C-q"       . nil)                                           ; remove keybinding for read-only-mode since I almost never press it on purpose
  ("C-x C-r"       . #'helm-recentf)
  ("C-x C-z"       . nil)                                           ; instead of suspend-frame
  ("C-x b"         . #'helm-buffers-list)
  ("C-x f"         . #'helm-find-files)
  ("C-x k"         . #'kill-this-buffer)
  ("C-x r r"       . #'register-list)                               ; replaces copy-rectangle-to-register
  ("C-x w"         . nil)
  ("C-x w ."       . #'highlight-symbol-at-point)                   ; this is the normal binding for this function but isn't added until `hi-lock.el` is loaded
  ("C-z"           . #'evil-normal-state)
  ;; ("ESC <up>"      . #'windmove-up)
  ("H-;"           . #'cam/realign-eol-comments)
  ("H-M-a"         . #'mc/skip-to-previous-like-this)
  ("H-M-e"         . #'mc/skip-to-next-like-this)
  ("H-a"           . #'mc/mark-previous-like-this)
  ("H-e"           . #'mc/mark-next-like-this)
  ("M-/"           . #'hippie-expand)                               ; Instead of dabbrev-expand
  ("M-:"           . #'pp-eval-expression)                          ; Instead of regular eval-expression
  ("M-g"           . #'goto-line)                                   ; Instead of 'M-g g' for goto-line, since I don't really use anything else with the M-g prefix
  ("M-j"           . #'cam/join-next-line)
  ("M-x"           . #'helm-M-x)
  ("s-;"           . #'cam/insert-spaces-to-goal-column)
  ("s-Z"           . #'undo-tree-redo)
  ("s-f"           . #'ftf-grepsource)
  ("s-o"           . #'ftf-find-file))


;;; ---------------------------------------- [[<Mode/Package Specific Setup]] ----------------------------------------

;;; [[<etc]]

(tweak-package button-lock
  :load ((diminish 'button-lock-mode)))

(tweak-package wiki-nav
  :load ((diminish 'wiki-nav-mode)))

(tweak-package highlight-parentheses
  :load ((diminish 'highlight-parentheses-mode)))



;;; [[<Lisp Modes]]

;; (declare-function cam/evil-mode-lisp-setup "init.el")

(setq-default cam/is-lisp-mode-p nil)

(defun cam/switch-to-paredit ()
  (when cam/is-lisp-mode-p
    (ignore-errors
      (paredit-mode 1))
    (smartparens-mode -1)
    (when evil-mode
      (evil-smartparens-mode -1)
      (evil-cleverparens-mode -1))))

(defun cam/switch-to-smartparens ()
  (when cam/is-lisp-mode-p
    (paredit-mode -1)
    (smartparens-mode 1)
    (when evil-mode
      (evil-smartparens-mode 1)
      (evil-cleverparens-mode 1))))

(defun cam/lisp-mode-setup ()
  (unless cam/is-lisp-mode-p
    (setq-local cam/is-lisp-mode-p t)
    (highlight-parentheses-mode 1)
    (rainbow-delimiters-mode 1)
    (show-paren-mode 1)
    (cam/switch-to-paredit)
    (when (and evil-mode
               (not (cl-member evil-state '(emacs insert))))
      (cam/switch-to-smartparens))
    (add-hook 'before-save-hook
      #'cam/untabify-current-buffer
      (not :append)
      :local)))


;;; [[<auto-complete]]
(tweak-package auto-complete
  :declare (ac-complete-functions ac-complete-symbols ac-complete-variables)
  :vars ((ac-delay . 0.05) ; 0.2
         (ac-auto-show-menu . 0.1) ; 0.5
         (ac-candidate-menu-height . 20)
         ;; (ac-candidate-limit . 20)
         (ac-menu-height . 20)         ; number of results to show
         (ac-quick-help-height . 50)   ; increase max height of quick help from 20 lines to 50
         (ac-use-menu-map . t))        ; use special completion keymap when showing completion menu
  :load ((cam/suppress-messages
           (ac-config-default)
           (add-to-list 'ac-modes 'cider-repl-mode)
           (add-to-list 'ac-modes 'emacs-lisp-mode)
           (add-to-list 'ac-modes 'ielm-mode)))
  :keymap ac-menu-map
  :keys (("A-f" . #'ac-complete-functions)
         ("A-s" . #'ac-complete-symbols)
         ("A-v" . #'ac-complete-variables)))

(tweak-package auto-complete-config
  :declare (ac-emacs-lisp-mode-setup))

;;; [[<C/C++]]

(tweak-package cc-mode
  :mode-name c-mode
  :minor-modes (auto-complete-mode
                electric-pair-local-mode)
  :local-vars ((tab-width . 4)
               (c-basic-indent . "k&r")
               (c-basic-offset . 4)
               (c-default-style . "linux"))
  :keys (("C-j" . #'newline)))

(defun cam/c++-switch-between-header-and-impl ()
  "Toggle between the corresponding C++ header or implementation file for the current buffer. By default, raises an
error if the corresponding file does not exist; pass the prefix arg to suppress this error and visit the (new) file."
  (interactive)
  (let* ((header-extensions '("h" "hpp" "hxx" "hh" "H" "h++"))
         (impl-extensions '("cpp" "cxx" "cc" "C" "c++"))
         (possible-extensions (if (member (file-name-extension buffer-file-name) header-extensions)
                                  impl-extensions
                                header-extensions))
         found)
    (cl-labels ((file-name-with-extension
                 (extension)
                 (concat (file-name-sans-extension buffer-file-name) "." extension)))
      (cl-dolist (extension possible-extensions)
        (let ((new-file-name (file-name-with-extension extension)))
          (message "Trying %s..." new-file-name)
          (when (file-exists-p new-file-name)
            (find-file new-file-name)
            (setq found t))))
      (unless found
        (if current-prefix-arg
            (let ((default-extension (car possible-extensions)))
              (find-file (file-name-with-extension default-extension)))
          (user-error "Could not find related file for %s. Try again with C-u to create it" buffer-file-name))))))

(defun cam/insert-c++-log-message (text)
  (interactive "sstd::cout << ")
  (insert
   (concat
    "std::cout << "
    (if current-prefix-arg
        text
      (concat "\"" text " = \" << " text))
    " << std::endl;")))

;; company-rtags
;; helm-rtags
;; disaster
;; lsp-mode
;; company-lsp
;; lsp-ui
;; eglot
;; eldoc-box

;; MAYBE

;; cpp-auto-include

(tweak-package cc-mode
  :mode-name c++-mode
  :minor-modes (column-enforce-mode
                flycheck-mode
                ;; smartparens-strict-mode
                company-mode
                eldoc-box-hover-mode
                eldoc-box-hover-at-point-mode
                todo-font-lock-mode)
  :require (helm-rtags
            company-lsp
            lsp-mode
            lsp-ui
            lsp-clangd
            eglot)
  :vars ((company-lsp-enable-snippet . nil)
         (rtags-completions-enabled . t)
         (rtags-display-result-backend . 'helm))
  :local-vars ((flycheck-highlighting-mode . nil))
  :setup ((cam/c-mode-setup)
          (rtags-start-process-unless-running)
          (dolist (backend '(company-rtags company-lsp))
            (add-to-list 'company-backends backend))
          (add-to-list 'eglot-server-programs '((c++-mode) "clangd"))
          (eglot-ensure)
          (auto-complete-mode 0)
          (flyspell-prog-mode))
  :keys (("<S-tab>" . #'company-complete)
         ("<backtab>" . #'company-complete)
         ("<f1>" . #'eldoc-doc-buffer)
         ("<f7>" . #'cam/c++-switch-between-header-and-impl)
         ("C-." . #'disaster)                     ; disassemble code at point
         ("C-j" . #'newline)
         ("M-." . #'rtags-find-symbol-at-point)
         ("<f10>" .  #'cam/insert-c++-log-message)))


;;; [[<Clojure]]

(tweak-package clojure-mode
  :mode-name clojure-mode
  :require (clojure-mode-extra-font-locking)
  :minor-modes (auto-complete-mode
                cider-mode
                clj-refactor-mode
                column-enforce-mode
                eldoc-mode
                todo-font-lock-mode)
  :setup ((cam/lisp-mode-setup)
          (flyspell-prog-mode)
          (ac-cider-setup)
          (cljr-add-keybindings-with-modifier "A-H-"))
  :local-vars ((clojure-align-forms-automatically . t) ; vertically aligns some forms automatically (supposedly)
               (ac-delay . 1.0)                        ; use slightly longer delays for AC because CIDER is slow
               (ac-auto-show-menu . 1.0)
               (ac-quick-help-delay . 1.5)
               (fill-column . 118)                    ; non-docstring column width of 117, which fits nicely on GH
               (clojure-docstring-fill-column . 118)) ; docstring column width of 117
  :local-hooks ((after-save-hook . (lambda ()
                                     (add-hook 'after-save-hook #'cam/clj-load-buffer-clean-namespace nil :local))))
  :keys (("<C-M-return>" . #'cam/clj-save-load-switch-to-cider)
         ("<f1>"         . #'ac-cider-popup-doc)
         ("<f7>"         . #'cam/clj-switch-to-test-namespace)
         ("<f8>"         . #'cam/clj-switch-between-model-and-api-namespaces)
         ("<f9>"         . #'cam/clj-insert-header)
         ("<f10>"        . #'cam/clj-insert-println)
         ("<S-tab>"      . #'auto-complete)
         ("<backtab>"    . #'auto-complete)))

(tweak-package clj-refactor
  :load ((diminish 'clj-refactor-mode))
  :vars ((cljr-favor-prefix-notation . nil)))

(tweak-package cider
  :mode-name cider-repl-mode
  :declare (cider-jack-in)
  :vars ((cider-auto-select-error-buffer . nil)
         (cider-repl-use-pretty-printing . t))
  :require (ansi-color)
  :advice ((#'cider-repl-return :before (lambda ()
                                          "Delete trailing whitespace that may have been introduced by `auto-complete'."
                                          (interactive)
                                          (call-interactively #'delete-trailing-whitespace)))
           (#'nrepl-server-filter :around #'cam/clj-ansi-colorize-nrepl-output-buffer-if-needed))
  :minor-modes (auto-complete-mode
                eldoc-mode)
  :setup ((cam/lisp-mode-setup)
          (ac-cider-setup))
  :keys (("M-RET" . #'cider-switch-to-last-clojure-buffer)
         ("{" . #'paredit-open-curly)
         ("<f1>" . #'ac-cider-popup-doc)
         ("<S-tab>" . #'auto-complete)
         ("<backtab>" . #'auto-complete)))

(tweak-package cider-macroexpansion
  :setup ((read-only-mode -1))
  :keys  (("C-c RET" . #'cider-macroexpand-1)))

(tweak-package cider-eval
  :declare (cider-connected-p cider-current-ns cider-load-buffer cider-switch-to-last-clojure-buffer cider-switch-to-relevant-repl-buffer))

(tweak-package cider-repl
  :declare (cider-repl-clear-buffer cider-repl-return cider-repl-set-ns))


;;; [[<column-enforce-mode]]
(tweak-package column-enforce-mode
  :mode-name column-enforce-mode
  :vars ((column-enforce-column . 120)))


;;; [[<css-mode]]
(tweak-package css-mode
  :mode-name css-mode
  :minor-modes (electric-pair-local-mode))


;;; [[<company]]
(tweak-package company
  :vars ((company-idle-delay . 0.01)
         (company-minimum-prefix-length . 1)))


;;; [[<dired]]
(defun cam/after-dired-find-file ()
  "After-advice for `dired-find-file'. Kill all `dired' buffers
unless they are the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (with-current-buffer buf
        (when (eq major-mode 'dired-mode)
          (kill-buffer buf))))))

(defun cam/revert-dired-buffers ()
  "Revert all `dired' buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'dired-mode)
        (revert-buffer)))))

(defun cam/around-dired-do-delete (fun &optional arg)
  "Around-advice for `dired-do-delete'. When deleting a file, check if its a directory; if so, and the directory is
deleted, ask to kill any buffers that were visiting files that were children of that directory."
  (let* ((file (dired-get-filename))
         (deleting-directory-p (file-directory-p file)))
    (let ((result (funcall fun arg)))
      (when (and deleting-directory-p
                 (not (file-exists-p file))) ; check that file was actually deleted
        (dolist (buf (buffer-list))
          (-when-let (buffer-file (buffer-file-name buf))
            (when (string-prefix-p (expand-file-name file) (expand-file-name buffer-file))
              (kill-buffer-ask buf)))))
      result)))

(tweak-package dired
  :declare (dired-do-delete dired-find-file dired-get-filename dired-hide-details-mode)
  :vars ((dired-recursive-copies  . 'always)
         (dired-recursive-deletes . 'always))
  :require (dired-x)                              ; dired-smart-shell-command, dired-jump (C-x C-j), etc.
  :advice ((#'dired-do-delete :around #'cam/around-dired-do-delete)
           (#'dired-find-file :after  #'cam/after-dired-find-file)
           (#'dired-smart-shell-command :after (lambda (&rest _)
                                                 (revert-buffer))))
  :load ((add-hook 'focus-in-hook #'cam/revert-dired-buffers))
  :minor-modes (dired-hide-details-mode))

(tweak-package dired-x
  :declare (dired-smart-shell-command))


;;; [[<Emacs Lisp]]

(defvar-local cam/byte-compile nil
  "Make this a file-local variable and we'll byte compile it whenever it's saved.")

(defvar-local cam/generate-autoloads nil
  "Generate autoloads for this file whenever it's saved.")

(defun cam/emacs-lisp-macroexpand-last-sexp ()
  (interactive)
  (call-interactively #'pp-macroexpand-last-sexp)
  (with-current-buffer "*Pp Macroexpand Output*"
    (macrostep-mode 1)))

(defun cam/emacs-lisp-eval-switch-to-ielm ()
  (interactive)
  (eval-buffer)
  (-if-let (ielm-window (get-window-with-predicate
                         (lambda (window)
                           (string-equal (buffer-name (window-buffer window)) "*ielm*"))))
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

(tweak-package elisp-mode
  :mode-name emacs-lisp-mode
  :load ((put 'add-hook 'lisp-indent-function 1))
  :minor-modes (aggressive-indent-mode
                auto-complete-mode
                column-enforce-mode
                eldoc-mode
                elisp-slime-nav-mode
                emacs-lisp-color-code-mode
                morlock-mode
                todo-font-lock-mode
                wiki-nav-mode)
  :setup ((cam/lisp-mode-setup)
          (unless (string-equal user-init-file (buffer-file-name))
            (flycheck-mode 1)))
  :local-hooks ((after-save-hook . (lambda ()
                                     (when cam/byte-compile
                                       (byte-compile-file (buffer-file-name) :load))
                                     (when cam/generate-autoloads
                                       (update-file-autoloads (buffer-file-name) :save-after cam/autoloads-file)))))
  :local-vars ((fill-column . 118)
               (emacs-lisp-docstring-fill-column . 118))
  :keys (("<C-M-return>" . #'cam/emacs-lisp-eval-switch-to-ielm)
         ("C-c RET"      . #'cam/emacs-lisp-macroexpand-last-sexp)
         ("C-x C-e"      . #'pp-eval-last-sexp)
         ("<S-tab>" . #'auto-complete)
         ("<backtab>" . #'auto-complete)
         ("<f1>" . #'elisp-slime-nav-describe-elisp-thing-at-point)
         ("<f10>" . #'cam/emacs-lisp-insert-message)))

(tweak-package dash
  :declare (dash-enable-font-lock)
  :load ((dash-enable-font-lock)))

(tweak-package elisp-slime-nav
  :load ((diminish 'elisp-slime-nav-mode))
  :keys (("C-c C-d" . #'elisp-slime-nav-describe-elisp-thing-at-point)))

(tweak-package ielm
  :mode-name inferior-emacs-lisp-mode
  :hook-name ielm-mode-hook
  :minor-modes (aggressive-indent-mode
                auto-complete-mode
                elisp-slime-nav-mode
                morlock-mode)
  :setup ((cam/lisp-mode-setup)
          (ac-emacs-lisp-mode-setup))
  :local-vars ((indent-line-function . #'lisp-indent-line))     ; automatically indent multi-line forms correctly
  :keys (("C-c RET" . #'cam/emacs-lisp-macroexpand-last-sexp)
         ("<S-tab>" . #'auto-complete)
         ("<backtab>" . #'auto-complete)
         ("<f1>" . #'elisp-slime-nav-describe-elisp-thing-at-point)))

(tweak-package nadvice
  :load ((put #'advice-add 'lisp-indent-function 2)))


;;; [[<Eval Expresssion (Minibuffer)]]
(tweak-package simple
  :hook-name eval-expression-minibuffer-setup-hook
  :minor-modes (company-mode
                paredit-mode)
  :local-vars ((company-echo-delay . 10)))


;;; [[<Find Things Fast]]
(tweak-package find-things-fast
  :load ((nconc ftf-filetypes '("*.clj"
                                "*.cljc"
                                "*.cljs"
                                "*.css"
                                "*.edn"
                                "*.el"
                                "*.html"
                                "*.js"
                                "*.jsx"
                                "*.java"
                                "*.lisp"
                                "*.md"
                                "*.mustache"
                                "*.pml"
                                "*.yaml"
                                "*.yml"))))


;;; [[<Git Commit Mode]]
(tweak-package git-commit
  :mode-name git-commit
  :minor-modes (flyspell-mode))


;;; [[<Guide Key]]
(tweak-package guide-key
  :vars ((guide-key/idle-delay . 1.0)
         (guide-key/recursive-key-sequence-flag . t)
         (guide-key/guide-key-sequence . '("<f12>" "<f1>"
                                           "<help>" "A-'"
                                           "A-*"    "A-,"
                                           "A-/"    "A-1"
                                           "A-3"    "A-\""
                                           "A-^"    "A-_"
                                           "A-`"    "A-r"
                                           "A-~"    "C-c"
                                           "C-h"    "C-x"
                                           "M-o"))))


;;; [[<Helm]]
(tweak-package helm
  :vars ((helm-buffers-fuzzy-matching . t) ; enable fuzzy matching for helm
         (helm-recentf-fuzzy-match    . t)
         (helm-M-x-fuzzy-match        . t)))

;;; [[<Java]]
(tweak-package cc-mode
  :mode-name java-mode
  :setup ((cam/c-mode-setup))
  :keys (("C-j" . #'newline)))


;;; [[<loccur]]
(tweak-package loccur
  :declare (loccur))

;;; [[<Magit]]
(defun cam/magit-visit-pull-request-url ()
  "Visit the current git branch's PR on GitHub."
  (interactive)
  (browse-url (concat "http://github.com/"
                      (->> (magit-get "remote" (magit-get-current-remote) "url")
                           (string-remove-suffix ".git")
                           (string-remove-prefix "git@github.com:"))
                      "/pull/"
                      (magit-get-current-branch))))

(defun cam/magit-shell-command ()
  "Replacement for `shell-command' in `magit-status-mode'.
Calls `magit-refresh' after the command finishes."
  (interactive)
  (call-interactively #'shell-command)
  (call-interactively #'magit-refresh))

(defun cam/refresh-magit-buffers ()
  "Refresh all `magit-status-mode' buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'magit-status-mode)
        (magit-refresh)))))

(defun cam/magit-buffer-p (buffer)
  (string-prefix-p "*magit" (buffer-name buffer)))

(defun cam/kill-all-magit-buffers-and-windows ()
  "Kill all magit-related buffers and windows."
  (interactive)
  ;; kill all the visible magit buffers & their windows
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (let ((buffer (window-buffer window)))
        (when (cam/magit-buffer-p buffer)
          (with-current-buffer buffer
            (kill-buffer-and-window))))))
  ;; ok, now kill any magit buffers in the background
  (dolist (buffer (buffer-list))
    (when (cam/magit-buffer-p buffer)
      (kill-buffer buffer))))

(tweak-package magit
  :mode-name magit-status-mode
  :declare (magit-get magit-get-current-branch magit-get-current-remote magit-refresh)
  :vars ((magit-auto-revert-mode-lighter . "")
         (magit-last-seen-setup-instructions . "1.4.0")
         (magit-push-always-verify . nil)
         ;; Don't prompt to save buffers in the current repo before performing Magit actions
         (magit-save-repository-buffers . 'dontask))
  :load ((add-hook 'focus-in-hook #'cam/refresh-magit-buffers))
  :keys (("C-x 4 0" . #'cam/kill-all-magit-buffers-and-windows)
         ("M-!"     . #'cam/magit-shell-command)
         ("V"       . #'cam/magit-visit-pull-request-url)
         ("s-u"     . #'magit-refresh)))


;;; [[<markdown]]

(eval-after-load "markdown-mode"
  '(progn
     (require 'preview-markdown)
     (add-hook 'markdown-mode-hook
       (lambda ()
         (add-hook 'after-save-hook #'preview-markdown-if-automatic-previews-enabled nil t)))))


(tweak-package markdown-mode
  :mode-name markdown-mode
  :minor-modes (flyspell-mode)
  ;; :setup ((add-hook 'after-save-hook #'user/preview-markdown (not :append) :local))
  )

;;; [[<Messages]]

(defun cam/clear-messages-buffer ()
  (interactive)
  (cam/when-let-visible-buffer ((buffer (string-equal "*Messages*" (buffer-name buffer))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (read-only-mode 1))))

(tweak-package simple
  :mode-name messages-buffer-mode
  :keys (("c" . #'cam/clear-messages-buffer)))

;;; [[<Objective-C]]
(tweak-package cc-mode
  :mode-name objc-mode
  :load (;; Automatically open .h files with @interface declarations as obj-c rather than c
         (add-to-list 'magic-mode-alist
                      `(,(lambda ()
                           (and (string-equal (file-name-extension buffer-file-name) "h")
                                (re-search-forward "@\\<interface\\>"
                                                   magic-mode-regexp-match-limit t)))
                        . objc-mode)))
  :setup ((cam/c-mode-setup))
  :keys (("C-j" . #'newline)))

;;; [[<Org]]
(defun cam/org-insert-code-block ()
  "Insert a new Org code block and start editing it."
  (interactive)
  (org-return-indent)
  (insert "#+BEGIN_SRC emacs-lisp")
  (org-return-indent)
  (insert "#+END_SRC")
  (forward-line -1)
  (org-end-of-line)
  (org-return-indent)
  (org-edit-src-code))

(tweak-package org
  :declare (org-bookmark-jump-unhide org-end-of-line org-return-indent)
  :vars ((org-support-shift-select . nil))
  :minor-modes (flyspell-mode)
  :local-vars ((truncate-lines . nil))
  :keys (("C-c c" . #'cam/org-insert-code-block)))

(tweak-package org-src
  :declare (org-edit-src-code))


;;; [[<Paredit]]
(tweak-package paredit
  :declare (paredit-backward-delete
            paredit-close-curly paredit-doublequote paredit-forward-delete paredit-forward-up paredit-in-string-p paredit-newline
            paredit-open-round paredit-open-square paredit-reindent-defun)
  ;; Tell paredit it's ok to delete selection in these contexts. Otherwise delete-selection-mode doesn't work with paredit
  :load ((put #'paredit-forward-delete  'delete-selection 'supersede)
         (put #'paredit-backward-delete 'delete-selection 'supersede)
         (put #'paredit-open-round      'delete-selection t)
         (put #'paredit-open-square     'delete-selection t)
         (put #'paredit-doublequote     'delete-selection t)
         (put #'paredit-newline         'delete-selection t)))


;;; [[<Perl]]
(tweak-package cperl-mode
  :mode-name cperl-mode
  :minor-modes (eldoc-mode
                electric-pair-local-mode
                ;; perl-completion-mode
                )
  :vars ((cperl-electric-keywords . t)
         (cperl-indent-level . 4))
  :keys (("C-c C-d" . #'cperl-perldoc))
  :local-vars ((eldoc-documentation-function . (lambda ()
                                                 (car
                                                  (let (cperl-message-on-help-error)
                                                    (cperl-get-help))))))
  ;; :setup ((add-to-list 'ac-sources 'ac-source-perl-completion))
  :auto-mode-alist ("\.pl$"
                    "\.pm$"))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))


;;; [[<Python]]
(tweak-package python
  :minor-modes (electric-pair-local-mode))

;;; [[<Racket]]
(tweak-package racket-mode
  :mode-name racket-mode
  :minor-modes (company-mode
                eldoc-mode)
  :setup ((cam/lisp-mode-setup)
          (auto-complete-mode -1)
          (eldoc-mode 1))
  :local-vars ((eldoc-documentation-function . #'racket-repl-eldoc-function))
  :keys (("<f1>" . #'racket-repl-describe)
         ("<S-tab>" . #'company-complete)
         ("<backtab>" . #'company-complete)))

(tweak-package racket-repl
  :setup ((cam/racket-mode-setup)
          (eldoc-mode 1))
  :local-vars ((eldoc-documentation-function . #'racket-repl-eldoc-function))
  :keys (("<f1>" . #'racket-repl-describe)
         ("<S-tab>" . #'company-complete)
         ("<backtab>" . #'company-complete)))

;;; [[<Shell]]
(tweak-package sh-script
  :mode-name sh-mode
  :minor-modes (electric-pair-local-mode
                todo-font-lock-mode)
  :keys (("C-j" . #'newline)))


;;; [[<(Common) Lisp Mode]]
(defun cam/save-load-switch-to-sly ()
  (interactive)
  (save-buffer)
  (sly-compile-and-load-file)
  (sly-switch-to-most-recent 'sly-mrepl-mode))

(setq inferior-lisp-program "sbcl")

;; Tweak configuration for the Sly minor mode as opposed to lisp-mode because otherwise tweak-package will redefine
;; cam/lisp-mode-setup

;;; [[<Sly]]
(tweak-package sly
  ;; :require (ac-sly)
  :minor-modes (auto-complete-mode)
  :keys (("<S-tab>" . #'auto-complete)
         ("<backtab>" . #'auto-complete)
         ("<C-M-return>" . #'cam/save-load-switch-to-sly))
  :setup ((cam/lisp-mode-setup)
          ;; don't load ac-sly until after sly is loaded, otherwise there will be circular requires between them
          (eval-after-load 'sly
            '(progn
               (require 'ac-sly)
               (set-up-sly-ac :fuzzy)))))

;;; [[<text-mode]]
(tweak-package text-mode
  :mode-name text-mode
  :minor-modes (flyspell-mode))

;;; [[<Web Mode]]

(defun cam/js-insert-console-dot-log (text)
  (interactive "sconsole.log: ")
  (if current-prefix-arg
      (insert "console.log(\"" text "\"); // NOCOMMIT")
    (insert "console.log(\"" text ":\", " text "); // NOCOMMIT")))

(tweak-package web-mode
  :mode-name web-mode
  :minor-modes (column-enforce-mode
                electric-pair-local-mode
                rainbow-delimiters-mode)
  :keys (("C-j" . #'newline)
         ("<S-tab>" . #'auto-complete)
         ("<backtab>" . #'auto-complete)
         ("<f10>" . #'cam/js-insert-console-dot-log))
  :auto-mode-alist ("\.js$"
                    "\.json$"
                    "\.html$"
                    "\.jsx$"
                    "\.mustache$"))

;;; [[<(n)xml Mode]]

(tweak-package nxml-mode
  :mode-name nxml-mode
  :minor-modes (electric-pair-mode
                company-mode)
  :keys (("<S-tab>" . #'company-complete)
         ("<backtab>" . #'company-complete)
         ("C-j" . #'newline))
  :vars ((nxml-slash-auto-complete-flag . t))
  :auto-mode-alist (".pml$")
  :setup ((when (string-equal (file-name-extension buffer-file-name) "pml")
            (message "<Loading cam/pml-mode>")
            (cam/pml-mode 1))))


;;; [[<YAML Mode]]
(tweak-package yaml-mode
  :mode-name yaml-mode
  :keys (("C-j" . #'newline-and-indent)
         ("C-m" . #'newline-and-indent)))


;;; [[<Global Minor Modes]]

;; Modes to disable
(blink-cursor-mode -1)                            ; disable annoying blinking cursor

;; Modes to enable
(delete-selection-mode 1)                         ; typing will delete selected text
(editorconfig-mode 1)                             ; parse .editorconfig files and apply settings for things like indentation
(global-anzu-mode 1)                              ; show number of matches in mode line while searching
(global-auto-revert-mode 1)                       ; automatically reload files when they change on disk
(global-diff-hl-mode 1)                           ; Show which lines have changed since last git commit in the fringe
(global-eldoc-mode 1)                             ; Automatically enable eldoc-mode in any buffers possible. Display fn arglists / variable dox in minibuffer
(global-undo-tree-mode 1)
(guide-key-mode 1)                                ; Show list of completions for keystrokes after a delay
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(projectile-mode 1)
(rainbow-mode 1)                                  ; Colorize strings like #FCE94F
(recentf-mode 1)                                  ; Track recently visited files
(save-place-mode 1)                               ; automatically save position in files & start at that position next time you open them
(winner-mode 1)

;; for some obnoxious reason there's no global-rainbow-mode so this will have to suffice
(add-hook 'find-file-hook (lambda ()
                            (rainbow-mode 1)))


;;; [[<Diminished Minor Modes]]

(dolist (mode '(anzu-mode
                diff-hl-mode
                editorconfig-mode
                global-auto-revert-mode
                guide-key-mode
                projectile-mode
                rainbow-mode
                undo-tree-mode))
  (diminish mode))


;;; ---------------------------------------- [[<Powerline & Evil Mode]] ----------------------------------------

(require 'evil)
(require 'powerline)
(require 'powerline-evil)

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

(setq-default display-line-numbers nil)
(setq-local display-line-numbers-widen t)

;; (defun cam/evil-mode-setup ()
;;   (interactive)
;;   (if (eq evil-state 'normal)
;;       (setq-local display-line-numbers 'visual)
;;     (progn
;;       (setq cursor-type 'box)
;;       (set-cursor-color (cam/evil-state-color nil))
;;       (kill-local-variable 'display-line-numbers))))

;; (add-hook 'evil-mode-hook #'cam/evil-mode-setup)

;; set colors for evil-STATE-state-cursor for various evil states
(dolist (state '(emacs normal visual insert replace operator motion))
  (set (intern (format "evil-%s-state-cursor" state))
       (list (cam/evil-state-color state) 'box)))

;; define faces for various evil states e.g. cam/active-evil-normal-state
(defun cam/active-evil-state-face-symb (&optional state)
  (let ((state (or state evil-state 'nil)))
    (intern (format "cam/active-evil-%s-state" state))))

(dolist (state '(nil emacs normal visual insert replace operator motion))
  (let ((symb (cam/active-evil-state-face-symb state))
        (face (list (list t :background (cam/evil-state-color state) :foreground "white"))))
    (message (format "(face-spec-set %s %s)" symb face))
    (face-spec-set symb face)))

(defun cam/face-symb (active-or-inactive where)
  (intern (format "cam/%s-%s" active-or-inactive where)))

;; define all of our other faces now
(dolist (group '((active . ((side-outer "gray70" "black")
                            (side-center "gray80" "black")
                            (side-inner "gray90" "black")
                            (center "white" "black")))
                 (inactive . ((side-outer "gray10" "gray60")
                              (side-center "gray20" "gray70")
                              (side-inner "gray30" "gray80")
                              (center "gray40" "gray90")))))
  (let ((active-or-inactive (car group))
        (faces (cdr group)))
    (dolist (face faces)
      (let ((symb (cam/face-symb active-or-inactive (car face)))
            (bg (cadr face))
            (fg (caddr face)))
        (face-spec-set symb (list (list t :background bg :foreground fg)))))))

(setq powerline-evil-tag-style 'verbose)

(defun cam/enable-relative-line-numbers ()
  (interactive)
  (setq-local display-line-numbers 'visual))

(defun cam/disable-relative-line-numbers ()
  (interactive)
  (kill-local-variable 'display-line-numbers))

;; TODO - motion state (?)
(dolist (hook '(evil-normal-state-entry-hook evil-operator-state-entry-hook evil-motion-state-entry-hook))
  (add-hook hook #'cam/enable-relative-line-numbers)
  (add-hook hook #'cam/switch-to-smartparens))

(dolist (hook '(evil-emacs-state-entry-hook evil-insert-state-entry-hook ;; evil-replace-state-entry-hook
                                            ))
  (add-hook hook #'cam/disable-relative-line-numbers)
  (add-hook hook #'cam/switch-to-paredit))

(kill-local-variable 'mode-line-format)

(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active?            (powerline-selected-window-active))
           (active-or-inactive (if active? 'active 'inactive))
           (color-face         (if active?
                                   (cam/active-evil-state-face-symb)
                                 (cam/face-symb active-or-inactive 'side-outer)))
           (side-outer-face    (cam/face-symb active-or-inactive 'side-outer))
           (side-center-face   (cam/face-symb active-or-inactive 'side-center))
           (side-inner-face    (cam/face-symb active-or-inactive 'side-inner))
           (center-face        (cam/face-symb active-or-inactive 'center))

           (lhs-outside
            (list
             (powerline-raw (concat (powerline-evil-tag) " ") color-face 'l)))

           (lhs-center
            (list
             (powerline-raw
              (concat
               ;; %b = buffer name
               " %b "
               (when (buffer-modified-p)
                 "[modified] "))
              side-center-face)
             (powerline-arrow-left side-center-face side-inner-face)))

           (lhs-inside
            (list
             (powerline-major-mode side-inner-face 'l)
             (powerline-process side-inner-face)
             (powerline-raw " " side-inner-face)))

           (center-left
            (list
             (powerline-arrow-left side-inner-face center-face)
             (powerline-minor-modes center-face 'l)
             (powerline-narrow center-face 'l)
             (powerline-raw " " center-face)))

           (center-right
            (list
             (when-let ((process (powerline-process)))
               (powerline-raw process center-face 'r))
             (powerline-arrow-right center-face side-inner-face)))

           (rhs-inside
            (list
             (powerline-raw
              (concat
               " "
               (powerline-encoding)
               (when buffer-read-only
                 " [readonly]"))
              side-inner-face
              'r)
             (powerline-arrow-right side-inner-face side-center-face)))

           (rhs-center
            (list
             (powerline-raw
              ;; %l = line number; %C = column number
              (concat
               " L%l/"
               (int-to-string (line-number-at-pos (point-max)))
               " C%C")
              side-center-face 'r)
             (powerline-arrow-right side-center-face side-outer-face)))

           (rhs-outside
            (list
             (when global-mode-string
               (powerline-raw global-mode-string side-outer-face 'r))
             (powerline-vc side-outer-face 'r)))

           (lhs (append lhs-outside lhs-center lhs-inside center-left))

           (rhs (append center-right rhs-inside rhs-center rhs-outside)))
      (concat
       (powerline-render lhs)
       (powerline-fill center-face (powerline-width rhs))
       (powerline-render rhs))))))

;; (defun cam/window-configuration-change-evil-setup ()
;;   (kill-local-variable 'mode-line-format))

;; (add-hook 'window-configuration-change-hook #'cam/window-configuration-change-evil-setup)

(set-face-bold 'mode-line-inactive nil)
(set-face-bold 'mode-line nil)

(setq evil-default-state 'emacs)

(defalias 'evil-insert-state 'evil-emacs-state) ; always use emacs state instead of insert state.

;; (define-key evil-emacs-state-map (kbd "C-[") #'evil-normal-state)

;; (evil-mode 1)

;;; ---------------------------------------- [[<Final Setup]] ----------------------------------------

(ignore-errors
  (load-file custom-file))

(unless cam/has-loaded-init-p
  (toggle-frame-maximized))

;; delete the *Warnings* buffer after 100 ms, then try again at 500ms and 1 second to make sure it's gone.
(defun cam/-delete-warning-buffer ()
  (message "<DELETE WARNING BUFFER>")
  (cam/when-let-visible-buffer ((buffer (string-equal (buffer-name buffer) "*Warnings*")))
    (unless (eq (get-buffer-window nil) this-window)
      (delete-window this-window))
    (kill-buffer buffer)))

(run-at-time 0.1 (not :repeat) #'cam/-delete-warning-buffer)
(run-at-time 0.5 (not :repeat) #'cam/-delete-warning-buffer)
(run-at-time 1.0 (not :repeat) #'cam/-delete-warning-buffer)

(setq cam/has-loaded-init-p t)

(ignore-errors ; only seems to work on Emacs 25+
  (message "Loaded init.el in %.0f ms." (* (float-time (time-subtract after-init-time before-init-time)) 1000.0)))

(require 'unicode-fonts)
(unicode-fonts-setup)
