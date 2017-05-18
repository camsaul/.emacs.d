;;; -*- lexical-binding: t; coding: utf-8; cam/byte-compile: t; comment-column: 50; -*-

;; (unless (>= emacs-major-version 25)
;;   (error "This setup requires Emacs version 25 or newer."))

;;; TOC:
;;; [[Initial Setup]]
;;; [[Package Setup]]
;;; [[Global Setup]]
;;;    [[Theme]]
;;;    [[Global Requires]]
;;;    [[Autoloads]]
;;;    [[Diminished Minor Modes]]
;;;    [[Global Settings]]
;;;    [[Global Macros]]
;;;    [[Global Functions]]
;;;    [[Global Hooks]]
;;;    [[Emacs 24 Workarounds]]
;;;    [[Global Keybindings]]
;;; [[Mode/Package Specific Setup]]
;;;    [[etc]]
;;;    [[Lisp Modes]]
;;;    [[auto-complete]]
;;;    [[Clojure]]
;;;    [[dired]]
;;;    [[company]]
;;;    [[Emacs Lisp]]
;;;    [[Eval Expresssion (Minibuffer)]]
;;;    [[Find Things Fast]]
;;;    [[Git Commit Mode]]
;;;    [[Guide Key]]
;;;    [[Helm]]
;;;    [[js2-mode]]
;;;    [[loccur]]
;;;    [[Magit]]
;;;    [[markdown]]
;;;    [[Objective-C]]
;;;    [[Org]]
;;;    [[Paredit]]
;;;    [[Perl]]
;;;    [[Shell]]
;;;    [[Sly]]
;;;    [[Web Mode]]
;;;    [[YAML Mode]]
;;; [[Global Minor Modes]]
;;; [[Final Setup]]
;;; [[Experimental]]

;;; ---------------------------------------- [[<Initial Setup]] ----------------------------------------
;;; (Things that need to happen as soon as this file starts loading)

(setq gc-cons-threshold (* 128 1024 1024)         ; By default GC starts around ~780kB. Since this isn't the 90s GC when we hit 128MB
      load-prefer-newer t)                        ; load .el files if they're newer than .elc ones

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

;; In an effort to be really annoying you can only suppress the startup echo area message if you set it through customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-echo-area-message (user-login-name)))

(add-to-list 'safe-local-variable-values '(cam/byte-compile . t))
(add-to-list 'safe-local-variable-values '(cam/generate-autoloads . t))

(defvar cam/has-loaded-init-p nil
  "Have we done a complete load of the init file yet? (Use this to keep track of things we only want to run once, but not again if we call eval-buffer).")



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

;; ignore the warnings about having ~/.emacs.d in the load path
(eval-after-load 'warnings
  '(advice-add #'display-warning :around
     (lambda (function type message &optional level buffer-name)
       (unless (and (eq type 'initialization)
                    (string-prefix-p "Your `load-path' seems to contain" message))
         (funcall function type message level buffer-name)))))
(add-to-list 'load-path (expand-file-name user-emacs-directory) :append)

(defconst cam/autoloads-file (concat user-emacs-directory "autoloads.el"))

;; byte recompile the other files in this dir if needed
(defconst cam/auxilary-init-files
  (eval-when-compile (let (files)
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


(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org"       . "http://orgmode.org/elpa/")))

(defconst cam/packages
  '(ac-cider                                      ; auto-complete <-> cider
    ac-sly                                        ; auto-complete <-> sly
    ace-jump-mode
    ace-jump-zap                                  ; ace-jump-mode version of zap-to-char / zap-up-to-char
    aggressive-indent                             ; Minor mode to aggressively keep code always indented
    anzu                                          ; Show number of matches in mode-line while searching
    anything                                      ; prereq for perl-completion
    auto-complete                                 ; auto-completion
    cider                                         ; Clojure Interactive Development Environment that Rocks
    clj-refactor                                  ; Clojure refactoring minor mode
    clojure-mode-extra-font-locking
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
    everything                                    ; Required by perl-completion-mode
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
    js2-mode                                      ; Javascript
    loccur
    macrostep                                     ; Interactive macrostepper for Emacs Lisp
    magit
    markdown-mode                                 ; Major mode for editing markdown files
    morlock                                       ; Extra font-locking for Emacs Lisp
    multiple-cursors
    moe-theme
    nyan-mode                                     ; Nyan Cat shows position in mode-line
    org                                           ; Get latest version of org from Org package archive
    paredit
    perl-completion                               ; Auto-complete for Perl
    ;; pos-tip                                       ; Native tooltips
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
    yaml-mode))

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
    (set-frame-font "Source Code Pro-12" (not :keep-size) t))) ; t = apply font to all frames going forward & save setting to custom.el (supposedly)

(defun cam/setup-frame ()
  (set-fringe-style '(6 . 0))                     ; ¾ width fringe on the left and none on the right

  (moe-theme-random-color)
  (set-face-foreground 'mode-line "#111111")
  (set-cursor-color (face-background 'mode-line))
  (set-face-background 'mode-line-buffer-id nil)) ;  Don't show a blue background behind buffer name on modeline for deselected frames
(advice-add #'make-frame-command :after #'cam/setup-frame)

(unless cam/has-loaded-init-p
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
                mode-line-end-spaces
                " "
                mode-line-position))


;;; [[<Global Requires]]

(require 'editorconfig)

(eval-when-compile
  (require 'cl-lib)
  (require 'dash)
  (require 'subr-x)) ; when-let, thread-last, string-remove-prefix, etc.


;;; [[<Autoloads]]

(autoload #'describe-minor-mode "help")
(autoload #'perl-completion-mode "perl-completion")


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

      custom-file (expand-file-name
                   (concat user-emacs-directory
                           "custom.el"))

      echo-keystrokes 0.1                         ; show keystrokes in progress in minibuffer after 0.1 seconds instead of 1 second
      frame-resize-pixelwise t                    ; maximize as much as possible rather than rounding to closest whole line
      garbage-collection-messages t               ; Show messages when garbage collection occurs so we don't set the GC threshold too high and make Emacs laggy
      global-auto-revert-non-file-buffers t       ; also auto-revert buffers like dired
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
      shift-select-mode nil                       ; real Emacs users don't use shift-selection
      vc-make-backup-files t                      ; Make backups of files even if they're under VC
      visible-bell t
      w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-apps-modifier 'alt
      w32-lwindow-modifier 'super
      w32-rwindow-modifier 'hyper)

(setq-default indent-tabs-mode nil                ; disable insertion of tabs
              save-place t                        ; Automatically save place in each file
              truncate-lines t)                   ; don't display "continuation lines" (don't wrap long lines)


;;; [[<Global Hooks]]

(add-hook 'before-save-hook
  (lambda ()
    (delete-trailing-whitespace)
    (set-buffer-file-coding-system 'utf-8-auto-unix)))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p) ; if we're saving a script, give it execute permissions


;;; [[<Emacs 24 Workarounds]]
(unless (fboundp #'electric-pair-local-mode)
  (fset #'electric-pair-local-mode #'electric-pair-mode))
(unless (fboundp #'comint-clear-buffer)
  (fset #'comint-clear-buffer (lambda ()))) ; no-op


;;; [[<Global Keybindings]]

(cam/global-set-keys
  ("<A-escape>"    . #'helm-mark-ring)
  ("<A-return>"    . #'wiki-nav-ido)
  ("<C-M-s-down>"  . #'windmove-down)
  ("<C-M-s-left>"  . #'cam/windmove-left-or-other-frame)
  ("<C-M-s-right>" . #'cam/windmove-right-or-other-frame)           ; Use <f11> <key> for toggling various minor modes
  ("<C-M-s-up>"    . #'windmove-up)
  ("<H-SPC>"       . #'mc/mark-all-like-this)
  ("<H-escape>"    . #'ace-jump-line-mode)
  ("<H-return>"    . #'mc/mark-next-lines)
  ("<S-backspace>" . #'cam/hungry-delete-backward)
  ("<S-delete>"    . #'cam/hungry-delete-forward)
  ("<escape>"      . #'ace-jump-mode)
  ("<f5>"          . #'ftf-find-file)                               ; alternate bindings since super modifier doesn't work well on Windows
  ("<f6>"          . #'ftf-grepsource)
  ("<f11>"         . nil)
  ("<f11> a"       . #'aggressive-indent-mode)
  ("<f11> p"       . #'paredit-mode)
  ("<f11> r"       . #'read-only-mode)
  ("<f11> w"       . #'whitespace-mode)
  ("<f12> b"       . #'cam/bing-search)
  ("<f12> i"       . #'cam/instant-clojure-cheatsheet-search)
  ("<f12> j"       . #'cam/javadocs-search)
  ("<f12> k"       . #'cam/browse-korma-dox)
  ("<insert>"      . nil)
  ("A-;"           . #'cam/loccur)
  ("A-r l"         . #'rotate-layout)
  ("A-r w"         . #'rotate-window)
  ("C-="           . #'magit-status)
  ("C-M-y"         . #'helm-show-kill-ring)
  ("C-M-S-k"       . #'backward-kill-sexp)
  ("C-S-k"         . #'cam/backward-kill-line)
  ("C-c C-g"       . #'keyboard-quit)
  ("C-s-;"         . #'cam/align-map)
  ("C-h M"         . #'describe-minor-mode)
  ("C-x C-b"       . #'helm-buffers-list)
  ("C-x C-f"       . #'helm-find-files)
  ("C-x C-g"       . #'keyboard-quit)
  ("C-x C-r"       . #'helm-recentf)
  ("C-x C-z"       . nil)                                           ; instead of suspend-frame
  ("C-x b"         . #'helm-buffers-list)
  ("C-x C-d"       . #'dired)                                       ; instead of ido-list-directory
  ("C-x C-q"       . nil)                                           ; remove keybinding for read-only-mode since I almost never press it on purpose
  ("C-x f"         . #'helm-find-files)
  ("C-x k"         . #'kill-this-buffer)
  ("C-x r r"       . #'register-list)                               ; replaces copy-rectangle-to-register
  ("C-z"           . #'undo)
  ("ESC <up>"      . #'windmove-up)
  ("H-M-a"         . #'mc/skip-to-previous-like-this)
  ("H-M-e"         . #'mc/skip-to-next-like-this)
  ("H-;"           . #'cam/realign-eol-comments)
  ("H-a"           . #'mc/mark-previous-like-this)
  ("H-e"           . #'mc/mark-next-like-this)
  ("M-:"           . #'pp-eval-expression)                          ; Instead of regular eval-expression
  ("M-g"           . #'goto-line)                                   ; Instead of 'M-g g' for goto-line, since I don't really use anything else with the M-g prefix
  ("M-j"           . #'cam/join-next-line)
  ("M-x"           . #'helm-M-x)
  ("M-z"           . #'ace-jump-zap-up-to-char)
  ("M-/"           . #'hippie-expand)                               ; Instead of dabbrev-expand
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
(defun cam/lisp-mode-setup ()
  (highlight-parentheses-mode 1)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)

  (add-hook 'before-save-hook
    #'cam/untabify-current-buffer
    (not :append)
    :local))


;;; [[<auto-complete]]
(tweak-package auto-complete
  :declare (ac-complete-functions ac-complete-symbols ac-complete-variables)
  :vars ((ac-delay . 0.05)
         (ac-auto-show-menu . 0.1)
         (ac-candidate-menu-height . 30)
         (ac-menu-height . 30)         ; show 20 results instead of 10
         (ac-quick-help-delay . 0.2)
         (ac-quick-help-height . 50)   ; increase max height of quick help from 20 lines to 50
         (ac-use-menu-map . t))
  :load ((cam/suppress-messages
           ;; (require 'pos-tip)

           (ac-config-default)

           (add-to-list 'ac-modes 'cider-repl-mode)
           (add-to-list 'ac-modes 'ielm-mode)))
  :keymap ac-menu-map
  :keys (("A-f" . #'ac-complete-functions)
         ("A-s" . #'ac-complete-symbols)
         ("A-v" . #'ac-complete-variables)))

(tweak-package auto-complete-config
  :declare (ac-emacs-lisp-mode-setup))


;;; [[<Clojure]]

(cl-defun cam/visible-buffer-matching (pred &optional return-multiple-values?)
  "Return the first buffer visible in any window on any frame that satisfies PRED."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (let ((buffer (window-buffer window)))
        (when (funcall pred buffer)
          (cl-return-from cam/visible-buffer-matching (if return-multiple-values?
                                                          (list buffer window frame)
                                                        buffer)))))))

;; TODO - move these to appropriate places !
(cl-defmacro cam/when-let-buffer (((binding &body pred-body) &rest more) &body body)
  (declare (indent 1))
  `(cl-multiple-value-bind (,binding this-window this-frame) (cam/visible-buffer-matching (lambda (,binding)
                                                                                            ,@pred-body) :return-multiple-values)
     (when ,binding
       ,(if more `(cam/when-let-buffer ,more ,@body)
          `(progn ,@body)))))

(defun cam/cider-clear-output-buffer-when-visible ()
  "If the `cider-mode' output buffer is visible, clear its contents."
  (cam/when-let-buffer ((buffer (string-prefix-p "*nrepl-server" (buffer-name buffer))))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))))

(declare-function cider-load-buffer-and-switch-to-repl-buffer "cider-mode")

(defun cam/clojure-save-load-switch-to-cider ()
  (interactive)
  (save-buffer)
  (if (not (cider-connected-p)) (cider-jack-in)
    (ignore-errors
      (cider-load-buffer-and-switch-to-repl-buffer :set-namespace)
      (cider-repl-clear-buffer))
    ;; sometimes this doesn't work the first time around (not sure why) so try it twice just to be sure
    (ignore-errors
      (cider-load-buffer-and-switch-to-repl-buffer :set-namespace)
      (cider-repl-clear-buffer))
    (cam/cider-clear-output-buffer-when-visible)))

(tweak-package clojure-mode
  :mode-name clojure-mode
  :require (clojure-mode-extra-font-locking)
  :minor-modes (auto-complete-mode
                cider-mode
                clj-refactor-mode
                eldoc-mode
                todo-font-lock-mode)
  :setup ((cam/lisp-mode-setup)
          (ac-cider-setup)
          (cljr-add-keybindings-with-modifier "A-H-"))
  :local-vars nil
  :local-hooks nil
  :keys (("<C-M-s-return>" . #'cam/clojure-save-load-switch-to-cider)))

(tweak-package clj-refactor
  :load ((diminish 'clj-refactor-mode))
  :vars ((cljr-auto-sort-ns . nil)
         (cljr-expectations-test-declaration . "[expectations :refer :all]")))

(defun cam/ansi-colorize-nrepl-output-buffer-if-needed (f process output)
  (let ((old-max (with-current-buffer (process-buffer process)
                   (point-max))))
    (funcall f process (replace-regexp-in-string "^.+ :: " "" output)) ; strip the logging prefix while we're at it
    (with-current-buffer (process-buffer process)
      (ansi-color-apply-on-region old-max (point-max)))))

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
           (#'nrepl-server-filter :around #'cam/ansi-colorize-nrepl-output-buffer-if-needed))
  :minor-modes (auto-complete-mode
                aggressive-indent-mode
                eldoc-mode)
  :setup ((cam/lisp-mode-setup)
          (ac-cider-setup))
  :keys (("M-RET" . #'cider-switch-to-last-clojure-buffer)
         ("{" . #'paredit-open-curly)))

(tweak-package cider-macroexpansion
  :setup ((read-only-mode -1)))

(tweak-package cider-interaction
  :declare (cider-connected-p cider-current-ns cider-load-buffer cider-switch-to-last-clojure-buffer cider-switch-to-relevant-repl-buffer))

(tweak-package cider-repl
  :declare (cider-repl-clear-buffer cider-repl-return cider-repl-set-ns))


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
  "Around-advice for `dired-do-delete'. When deleting a file, check
if its a directory; if so, and the directory is deleted, ask to kill
any buffers that were visiting files that were children of that directory."
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

(defun cam/emacs-lisp-save-switch-to-ielm-if-visible ()
  (interactive)
  (save-buffer)
  (-when-let ((ielm-window (get-window-with-predicate (lambda (window)
                                                        (string= (buffer-name (window-buffer window)) "*ielm*")))))
    (select-window ielm-window)
    (comint-clear-buffer)
    (comint-kill-input)))

;; TODO - Emacs 25 only
(tweak-package elisp-mode
  :mode-name emacs-lisp-mode
  :load ((put 'add-hook 'lisp-indent-function 1))
  :minor-modes (aggressive-indent-mode
                auto-complete-mode
                eldoc-mode
                elisp-slime-nav-mode
                emacs-lisp-color-code-mode
                morlock-mode
                todo-font-lock-mode
                wiki-nav-mode)
  :setup ((cam/lisp-mode-setup)
          (unless (string= user-init-file (buffer-file-name))
            (flycheck-mode 1)))
  :local-hooks ((after-save-hook . (lambda ()
                                     (when cam/byte-compile
                                       (byte-compile-file (buffer-file-name) :load))
                                     (when cam/generate-autoloads
                                       (update-file-autoloads (buffer-file-name) :save-after cam/autoloads-file)))))
  :keys (("<C-M-s-return>" . #'cam/emacs-lisp-save-switch-to-ielm-if-visible)
         ("C-c RET"        . #'cam/emacs-lisp-macroexpand-last-sexp)
         ("C-x C-e"        . #'pp-eval-last-sexp)))

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
  :keys (("C-c RET" . #'cam/emacs-lisp-macroexpand-last-sexp)))

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
                                "*.css"
                                "*.edn"
                                "*.el"
                                "*.html"
                                "*.js"
                                "*.jsx"
                                "*.java"
                                "*.md"
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


;;; [[<js2-mode]]
(tweak-package js2-mode
  :mode-name js2-mode
  :minor-modes (electric-pair-local-mode
                rainbow-delimiters-mode)
  :keys (("C-j" . #'newline)
         ("M-j" . nil))
  :auto-mode-alist ("\.js$"
                    "\.json$"))


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
         (magit-save-repository-buffers . 'dontask)) ; Don't prompt to save buffers in the current repo before performing Magit actions
  :load ((add-hook 'focus-in-hook #'cam/refresh-magit-buffers))
  :keys (("C-x 4 0" . #'cam/kill-all-magit-buffers-and-windows)
         ("M-!"     . #'cam/magit-shell-command)
         ("V"       . #'cam/magit-visit-pull-request-url)
         ("s-u"     . #'magit-refresh)))


;;; [[<markdown]]
(tweak-package markdown-mode
  :mode-name markdown-mode
  :minor-modes (flyspell-mode))

;;; [[<Objective-C]]
(tweak-package cc-mode
  :mode-name objc-mode
  :load ( ;; Automatically open .h files with @interface declarations as obj-c rather than c
         (add-to-list 'magic-mode-alist
                      `(,(lambda ()
                           (and (string= (file-name-extension buffer-file-name) "h")
                                (re-search-forward "@\\<interface\\>"
                                                   magic-mode-regexp-match-limit t)))
                        . objc-mode)))
  :minor-modes (auto-complete-mode
                electric-pair-mode)
  :local-vars ((tab-width . 4)
               (c-basic-indent . 4)
               (c-basic-offset . 4))
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
                perl-completion-mode)
  :vars ((cperl-electric-keywords . t)
         (cperl-indent-level . 4))
  :keys (("C-c C-d" . #'cperl-perldoc))
  :local-vars ((eldoc-documentation-function . (lambda ()
                                                 (car
                                                  (let (cperl-message-on-help-error)
                                                    (cperl-get-help))))))
  :setup ((add-to-list 'ac-sources 'ac-source-perl-completion))
  :auto-mode-alist ("\.pl$"
                    "\.pm$"))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))


;;; [[<Shell]]
(tweak-package sh-script
  :mode-name sh-mode
  :minor-modes (electric-pair-local-mode
                todo-font-lock-mode))


;;; [[[<Sly]]
(tweak-package sly
  :vars ((inferior-lisp-program . "/usr/local/bin/sbcl"))
  :require (ac-sly)
  :minor-modes (auto-complete-mode)
  :setup ((cam/lisp-mode-setup)
          (set-up-sly-ac :fuzzy)))


;;; [[<Web Mode]]
(tweak-package web-mode
  :mode-name web-mode
  :minor-modes (electric-pair-local-mode
                rainbow-delimiters-mode)
  :keys (("C-j" . #'newline))
  :auto-mode-alist ("\.html$"
                    "\.jsx$"))


;;; [[<YAML Mode]]
(tweak-package yaml-mode
  :mode-name yaml-mode
  :keys (("C-j" . #'newline)))


;;; [[<Global Minor Modes]]

;; Modes to disable
(blink-cursor-mode -1)                            ; disable annoying blinking cursor

;; Modes to enable
(delete-selection-mode 1)                         ; typing will delete selected text
(global-anzu-mode 1)                              ; show number of matches in mode line while searching
(global-auto-revert-mode 1)                       ; automatically reload files when they change on disk
(global-diff-hl-mode 1)                           ; Show which lines have changed since last git commit in the fringe
(global-eldoc-mode 1)                             ; Automatically enable eldoc-mode in any buffers possible. Display fn arglists / variable dox in minibuffer
(global-undo-tree-mode 1)
(guide-key-mode 1)                                ; Show list of completions for keystrokes after a delay
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(nyan-mode 1)                                     ; Nyan Cat in mode line
(projectile-global-mode 1)
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
                global-auto-revert-mode
                guide-key-mode
                projectile-mode
                rainbow-mode
                undo-tree-mode))
  (diminish mode))


;;; ---------------------------------------- [[<Final Setup]] ----------------------------------------

(ignore-errors
  (load-file custom-file))

(unless cam/has-loaded-init-p
  (toggle-frame-maximized))

(setq cam/has-loaded-init-p t)

(ignore-errors ; only seems to work on Emacs 25+
  (message "Loaded init.el in %.0f ms." (* (float-time (time-subtract after-init-time before-init-time)) 1000.0)))


;;; ---------------------------------------- [[<Experimental]] ----------------------------------------

;;; ---------------------------------------- [[<Messages Auto-Scrolling]] ----------------------------------------

(cl-defun cam/buffer-window (buffer)
  "Return the first window on any frame showing BUFFER, if any."
  (dolist (frame (frame-list))
    (dolist (w (window-list frame))
      (when (eq (window-buffer w) buffer)
        (cl-return-from cam/buffer-window w)))))

(defun cam/scroll-messages-to-end ()
  "Scroll to the end of the *Messages* buffer if needed if it is currently visible."
  (when-let (messages-buffer (get-buffer "*Messages*"))
    (when-let (w (cam/buffer-window messages-buffer))
      (when (< (window-end w) (point-max))
        (with-selected-window w
          (set-window-start w (save-excursion
                                (goto-char (point-max))
                                (forward-line (- (- (window-height) 3)))
                                (point))))))))

(defvar cam/scroll-messages-async-delay 2
  "Number of seconds to wait before asynchronously scrolling the *Messages* buffer.")

(defvar cam/scroll-messages-timer nil)

(defun cam/scroll-messages-and-reset-timer ()
  (unwind-protect
      (unless (eq (current-buffer) (get-buffer "*Messages*")) ; don't scroll if *Messages* is the current buffer
        (cam/scroll-messages-to-end))
    (setq cam/scroll-messages-timer nil)))

(defun cam/scroll-messages-async (&rest _)
  (unless cam/scroll-messages-timer
    (setq cam/scroll-messages-timer (run-with-timer cam/scroll-messages-async-delay nil #'cam/scroll-messages-and-reset-timer))))

(advice-add #'message :after #'cam/scroll-messages-async)


;;; ---------------------------------------- [[cam/clojure-docstr-extra-font-lock-mode]] ----------------------------------------

(defconst cam/clojure-docstr-font-lock-keywords
  '(("\\<\\([[:upper:]-]+[[:punct:]]?\\)\\>" 1 (when (paredit-in-string-p)
                                                 'font-lock-variable-name-face)
     prepend)
    ("`\\(\\*?:?[[:alnum:]_<>*-/:]+[\\?!]?\\*?\\)`" 1 (when (paredit-in-string-p)
                                                        'font-lock-constant-face)
     prepend)))

(defconst cam/clojure-docstr-font-lock-mode-lighter
  " cam/clj-doc-fl")

(defvar-local cam/clojure-docstr-font-lock-mode nil)

(defun cam/clojure-docstr-font-lock-mode (&optional arg)
  (interactive)
  (if (null arg) (progn (cam/clojure-docstr-font-lock-mode (if cam/clojure-docstr-font-lock-mode -1 1))
                        (when (called-interactively-p 'interactive)
                          (message "Docstr font-locking %s in current buffer." (if cam/clojure-docstr-font-lock-mode "enabled" "disabled"))))
    (let ((enable (> arg 0)))
      (when enable
        (add-to-list 'minor-mode-alist (list 'cam/clojure-docstr-font-lock-mode cam/clojure-docstr-font-lock-mode-lighter)))
      (funcall (if enable #'font-lock-add-keywords #'font-lock-remove-keywords) nil cam/clojure-docstr-font-lock-keywords)
      (setq-local cam/clojure-docstr-font-lock-mode enable))
    (font-lock-flush)
    (font-lock-ensure)))

(add-hook 'clojure-mode-hook #'cam/clojure-docstr-font-lock-mode)
(moe-dark)


;;; ------------------------------------------------------------ Insert logging statements ------------------------------------------------------------

(defun cam/insert-console-dot-log (text)
  (interactive "sconsole.log: ")
  (if current-prefix-arg
      (insert "console.log(\"" text "\"); // NOCOMMIT")
    (insert "console.log(\"" text ":\", " text "); // NOCOMMIT")))

(eval-after-load 'web-mode
  '(define-key web-mode-map (kbd "<f10>") #'cam/insert-console-dot-log))

(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "<f10>") #'cam/insert-console-dot-log))

(defun cam/insert-clojure-println (text)
  (interactive "sprintln: ")
  (insert "(println \"" text ":\" " text ") ; NOCOMMIT"))

(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "<f10>") #'cam/insert-clojure-println))
