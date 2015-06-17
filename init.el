;;; -*- lexical-binding: t; coding: utf-8; byte-compile-dynamic: nil; comment-column: 50; -*-

;; (unless (>= emacs-major-version 25)
;;   (error "This setup requires Emacs version 25 or newer."))

;;; TOC:
;;; [[Initial Setup]]
;;; [[Package Setup]]
;;;    [[Function Declarations]]
;;; [[Global Setup]]
;;;    [[Theme]]
;;;    [[Global Requires]]
;;;    [[Autoloads]]
;;;    [[Global Minor Modes]]
;;;    [[Diminished Minor Modes]]
;;;    [[Global Settings]]
;;;    [[auto-mode-alist]]
;;;    [[Global Functions]]
;;;    [[Global Hooks]]
;;;    [[Emacs 24 Workarounds]]
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
;;;    [[Git Commit Mode]]
;;;    [[Guide Key]]
;;;    [[Helm]]
;;;    [[js2-mode]]
;;;    [[Magit]]
;;;    [[Org]]
;;;    [[Paredit]]
;;;    [[Sly]]
;;;    [[Web Mode]]
;;;    [[YASnippet]]
;;; [[Final Setup]]
;;; [[Experimental]]

;;; ---------------------------------------- [[<Initial Setup]] ----------------------------------------
;;; (Things that need to happen as soon as this file starts loading)

(setq gc-cons-threshold (* 128 1024 1024)         ; By default GC starts around ~780kB. Since this isn't the 90s GC when we hit 128MB
      load-prefer-newer t)                        ; load .el files if they're newer than .elc ones

;; EXPERIMENTAL !!!
;; Set garbage collection threshold to something crazy,
;; Then run garbage collection only when Emacs is idle
(setq gc-cons-threshold (* 1024 1024 1024 8))     ; 8 GB?
(run-with-idle-timer (* 60 2) :repeat #'garbage-collect)

(defvar cam/has-loaded-init-p nil
  "Have we done a complete load of the init file yet? (Use this to keep track of things we only want to run once, but not again if we call eval-buffer).")

;;; Don't show toolbar, scrollbar, splash screen, startup screen

(dolist (mode '(scroll-bar-mode
                tool-bar-mode))
  (when (boundp mode)
    (funcall mode -1)))

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

(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org"       . "http://orgmode.org/elpa/")))

(defconst cam/packages
  '(ac-cider                                      ; auto-complete <-> cider
    ac-js2                                        ; auto-complete <-> skewer <-> js2-mode
    ac-sly                                        ; auto-complete <-> sly
    ace-jump-mode
    ace-jump-zap                                  ; ace-jump-mode version of zap-to-char / zap-up-to-char
    aggressive-indent                             ; Minor mode to aggressively keep code always indented
    anzu                                          ; Show number of matches in mode-line while searching
    auto-complete                                 ; auto-completion
    auto-yasnippet                                ; quickly create disposable yasnippets
    cider                                         ; Clojure Interactive Development Environment that Rocks
    clj-refactor                                  ; Clojure refactoring minor mode
    clojure-mode-extra-font-locking
    clojure-snippets                              ; Clojure snippets!
    company                                       ; auto-completion
    dash
    diff-hl                                       ; mark uncommited changes in the fringe
    diminish                                      ; Replace or hide minor modes in mode-line
    dockerfile-mode                               ; Major mode for editing Dockerfiles
    editorconfig                                  ; Read EditorConfig files
    elisp-slime-nav                               ; Make M-. and M-, work in elisp like the do in slime
    ert                                           ; Emacs Lisp Regression Testing
    esup                                          ; Emacs Start-Up Profiler <3
    find-things-fast
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
    pos-tip                                       ; Native tooltips
    projectile
    rainbow-delimiters
    rainbow-mode
    register-list                                 ; dired-like editing of Emacs registers
    saveplace                                     ; save position of point when killing a buffer
    sly                                           ; Common Lisp IDE
    skewer-mode                                   ; live JS web dev for emacs
    rotate                                        ; rotate-window, rotate-layout, etc.
    undo-tree
    web-mode                                      ; major-mode for editing web templates
    wiki-nav                                      ; Navigate a file using [[WikiStrings]]
    yaml-mode
    yasnippet))                                   ; OK, we will use snippets

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

(custom-set-variables
 `(package-selected-packages ',cam/packages))

(eval-when-compile
  (require 'moe-theme)
  ;; (mapc #'require cam/packages)
  )

;; Declare some functions so byte compiler stops bitching about them possibly not being defined at runtime
(defmacro declare-functions (file &rest funs)
  (declare (indent 1))
  `(progn ,@(mapcar (lambda (fun)
                      `(declare-function ,fun ,file))
                    funs)))


;;; [[<Function Declarations]]

(declare-functions "auto-yasnippet"       aya-create aya-expand)
(declare-functions "auto-complete"        ac-complete-functions ac-complete-symbols ac-complete-variables)
(declare-functions "auto-complete-config" ac-emacs-lisp-mode-setup)
(declare-functions "cider"                cider-jack-in)
(declare-functions "cider-interaction"    cider-connected-p cider-current-ns cider-load-buffer cider-switch-to-last-clojure-buffer cider-switch-to-relevant-repl-buffer)
(declare-functions "cider-repl"           cider-repl-clear-buffer cider-repl-return cider-repl-set-ns)
(declare-functions "dired"                dired-do-delete dired-find-file dired-get-filename dired-hide-details-mode)
(declare-functions "dired-x"              dired-smart-shell-command)
(declare-functions "loccur"               loccur)
(declare-functions "magit"                magit-get magit-get-current-branch magit-get-current-remote magit-refresh)
(declare-functions "org"                  org-bookmark-jump-unhide org-end-of-line org-return-indent)
(declare-functions "org-src"              org-edit-src-code)
(declare-functions "paredit"              paredit-backward-delete paredit-close-curly paredit-doublequote paredit-forward-delete paredit-forward-up paredit-in-string-p
                                          paredit-newline paredit-open-round paredit-open-square)
(declare-functions "skewer-mode"          skewer-ping)


;;; ---------------------------------------- [[<Global Setup]] ----------------------------------------

;;; [[<Theme]]

(require 'moe-theme)

;; Load the theme just once, otherwise the screen will flicker all cray if we try to eval this buffer again
(unless cam/has-loaded-init-p
  (moe-dark))

;; (defconst cam/mode-line-color "#FCE94F")

(defun cam/setup-frame ()
  (set-frame-font "Source Code Pro-12")

  (set-fringe-style '(6 . 0))                     ; ¾ width fringe on the left and none on the right

  (moe-theme-random-color)
  (set-face-foreground 'mode-line "#111111")
  ;; (set-face-background 'mode-line cam/mode-line-color)
  (set-cursor-color (face-background 'mode-line))
  (set-face-background 'mode-line-buffer-id nil)) ; Don't show a blue background behind buffer name on modeline for deselected frames
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
  (require 'subr-x))                              ; when-let, thread-last, string-remove-prefix, etc.


;;; [[<Autoloads]]

(autoload #'describe-minor-mode "help")


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
(projectile-global-mode 1)
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(nyan-mode 1)                                     ; Nyan Cat in mode line
(rainbow-mode 1)                                  ; Colorize strings like #FCE94F
(save-place-mode 1)                               ; automatically save position in files & start at that position next time you open them
(winner-mode 1)
(time
 (yas-global-mode 1))                               ; Snippets. ~400 ms !

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
(dolist (hook.mode '((button-lock-mode-hook           . button-lock-mode)
                     (highlight-parentheses-mode-hook . highlight-parentheses-mode)
                     (wiki-nav-mode-hook              . wiki-nav-mode)))
  (add-hook (car hook.mode) (lambda ()
                              (diminish (cdr hook.mode)))))


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
      garbage-collection-messages t               ; Show messages when garbage collection occurs so we don't set the GC threshold too high and make Emacs laggy
      global-auto-revert-non-file-buffers t       ; also auto-revert buffers like dired
      next-line-add-newlines t                    ; C-n (#'next-line) will add a newline at the end of the buffer instead of giving you an error
      ns-right-command-modifier 'hyper
      ns-right-control-modifier 'hyper
      ns-right-option-modifier 'alt
      recentf-max-menu-items 50                   ; show more recent files in [Helm]recentf
      recentf-max-saved-items 50
      require-final-newline t                     ; add final newline on save
      revert-without-query '(".*")                ; tell revert-buffer to revert all buffers without confirmation
      save-interprogram-paste-before-kill t       ; Save clipboard strings (from other applications) into kill-ring before replacing them
      savehist-mode t                             ; Periodically save minibuffer history
      select-enable-clipboard t                   ; Cutting and pasting uses the clipboard
      shift-select-mode nil                       ; real Emacs users don't use shift-selection
      vc-make-backup-files t                      ; Make backups of files even if they're under VC
      visible-bell t)

(setq-default indent-tabs-mode nil                ; disable insertion of tabs
              save-place t                        ; Automatically save place in each file
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
  (declare (indent 0))
  `(cl-letf (((symbol-function 'message) (lambda (&rest _))))
     ,@body))

(defun cam/windmove-left-or-other-frame ()
  (interactive)
  (condition-case _
      (call-interactively #'windmove-left)
    (error (call-interactively #'other-frame))))

(defun cam/windmove-right-or-other-frame ()
  (interactive)
  (condition-case _
      (call-interactively #'windmove-right)
    (error (call-interactively #'other-frame))))

(defvar cam/insert-spaces-goal-col nil)
(defun cam/insert-spaces-to-goal-column (arg)
  "Called without a prefix arg, insert spaces until we reach `cam/insert-spaces-goal-col'.
Called with a prefix arg, set the value of `cam/insert-spaces-goal-col' to point."
  (interactive "P")
  (if arg (progn (setq-local cam/insert-spaces-goal-col (current-column))
                 (message "Insert spaces to column %d." (current-column)))
    (progn
      (unless cam/insert-spaces-goal-col
        (error "Don't know where to insert spaces to! Call this function with a prefix arg to set it."))
      (let ((num-spaces (- cam/insert-spaces-goal-col (current-column))))
        (if (< num-spaces 0) (delete-char num-spaces)
          (insert-char ?  num-spaces))))))

(defun cam/string-remove-text-properties (string)
  "Return a copy of STRING with all of its text properties removed."
  (let ((s (copy-seq string)))
    (set-text-properties 0 (length s) nil s)
    s))

(defun cam/instant-clojure-cheatsheet-search (search-term)
  "Open a browser window and search Instant Clojure Cheatsheet for SEARCH-TERM."
  (interactive (list (read-string "Search Instant Clojure Cheatsheet for: " (when (symbol-at-point)
                                                                              (-> (symbol-at-point)
                                                                                  symbol-name
                                                                                  cam/string-remove-text-properties)))))
  (browse-url (format "http://localhost:13370/#?q=%s" search-term)))

(defun cam/bing-search (search-term)
  "Open a browser window and search BING for SEARCH-TERM."
  (interactive (list (read-string "Search Bing for: " (when (symbol-at-point)
                                                        (-> (symbol-at-point)
                                                            symbol-name
                                                            cam/string-remove-text-properties)))))
  (browse-url (format "http://bing.com/search?q=%s" search-term)))

(defun cam/browse-korma-dox ()
  "Open a browser window with the SQL Korma documentation."
  (interactive)
  (browse-url "http://www.sqlkorma.com/docs"))

(defun cam/javadocs-search (search-term)
  "Open a browser window and search javadocs.org for SEARCH-TERM."
  (interactive (list (read-string "Search javadocs.org for: " (when (symbol-at-point)
                                                                (->> (symbol-at-point)
                                                                     symbol-name
                                                                     cam/string-remove-text-properties
                                                                     (string-remove-suffix "."))))))
  (browse-url (format "http://javadocs.org/%s" search-term)))


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

(dolist (key-command-pair '(("<A-escape>"    . #'helm-mark-ring)
                            ("<A-return>"    . #'wiki-nav-ido)
                            ("<C-M-s-down>"  . #'windmove-down)
                            ("<C-M-s-left>"  . #'cam/windmove-left-or-other-frame)
                            ("<C-M-s-right>" . #'cam/windmove-right-or-other-frame)  ; Use <f11> <key> for toggling various minor modes
                            ("<C-M-s-up>"    . #'windmove-up)
                            ("<H-SPC>"       . #'mc/mark-all-like-this)
                            ("<H-escape>"    . #'ace-jump-line-mode)
                            ("<H-return>"    . #'mc/mark-next-lines)
                            ("<escape>"      . #'ace-jump-mode)
                            ("<f11>"         . nil)
                            ("<f11> a"       . #'aggressive-indent-mode)
                            ("<f11> p"       . #'paredit-mode)
                            ("<f11> w"       . #'whitespace-mode)
                            ("<f12> b"       . #'cam/bing-search)
                            ("<f12> i"       . #'cam/instant-clojure-cheatsheet-search)
                            ("<f12> j"       . #'cam/javadocs-search)
                            ("<f12> k"       . #'cam/browse-korma-dox)
                            ("A-;"           . #'cam/loccur)
                            ("A-r l"         . #'rotate-layout)
                            ("A-r w"         . #'rotate-window)
                            ("C-="           . #'magit-status)
                            ("C-M-y"         . #'helm-show-kill-ring)
                            ("C-M-S-k"       . #'backward-kill-sexp)
                            ("C-S-k"         . #'cam/backward-kill-line)
                            ("C-c C-g"       . #'keyboard-quit)
                            ("C-h M"         . #'describe-minor-mode)
                            ("C-x C-b"       . #'helm-buffers-list)
                            ("C-x C-f"       . #'helm-find-files)
                            ("C-x C-g"       . #'keyboard-quit)
                            ("C-x C-r"       . #'helm-recentf)
                            ("C-x C-z"       . nil)                                  ; instead of suspend-frame
                            ("C-x b"         . #'helm-buffers-list)
                            ("C-x C-d"       . #'dired)                              ; instead of ido-list-directory
                            ("C-x C-q"       . nil)                                  ; remove keybinding for read-only-mode since I almost never press it on purpose
                            ("C-x f"         . #'helm-find-files)
                            ("C-x k"         . #'kill-this-buffer)
                            ("C-x r r"       . #'register-list)                      ; replaces copy-rectangle-to-register
                            ("C-z"           . #'undo)
                            ("ESC <up>"      . #'windmove-up)
                            ("H-M-a"         . #'mc/skip-to-previous-like-this)
                            ("H-M-e"         . #'mc/skip-to-next-like-this)
                            ("H-a"           . #'mc/mark-previous-like-this)
                            ("H-e"           . #'mc/mark-next-like-this)
                            ("M-:"           . #'pp-eval-expression)                 ; Instead of regular eval-expression
                            ("M-g"           . #'goto-line)                          ; Instead of 'M-g g' for goto-line, since I don't really use anything else with the M-g prefix
                            ("M-j"           . #'cam/join-next-line)
                            ("M-x"           . #'helm-M-x)
                            ("M-z"           . #'ace-jump-zap-up-to-char)
                            ("M-/"           . #'hippie-expand)                      ; Instead of dabbrev-expand
                            ("s-;"           . #'cam/insert-spaces-to-goal-column)
                            ("s-Z"           . #'undo-tree-redo)
                            ("s-f"           . #'ftf-grepsource)
                            ("s-o"           . #'ftf-find-file)))
  (global-set-key (kbd (car key-command-pair)) (eval (cdr key-command-pair))))


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

  (if (not (cider-connected-p)) (cider-jack-in)
    (progn
      (cider-load-buffer)
      (cider-repl-set-ns (cider-current-ns))
      (cider-switch-to-relevant-repl-buffer)
      (cider-repl-clear-buffer))))

(defun cam/clojure-mode-setup ()
  (cam/lisp-mode-setup)
  (auto-complete-mode 1)
  (ac-cider-setup)
  (clj-refactor-mode 1)
  (require 'clojure-mode-extra-font-locking)

  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-hook 'clojure-mode-hook #'cam/clojure-mode-setup)

(eval-after-load 'clojure-mode
  '(progn
     (message "EVAL AFTER LOAD CLOJURE MODE!!!")
     (clojure-snippets-initialize)

     (define-key clojure-mode-map (kbd "<C-M-s-return>") #'cam/clojure-save-load-switch-to-cider)

     (cljr-add-keybindings-with-modifier "A-H-")))

(defun cam/cider-repl-mode-setup ()
  (cam/lisp-mode-setup)
  (auto-complete-mode 1)
  (ac-cider-setup)
  (aggressive-indent-mode 1))
(add-hook 'cider-repl-mode-hook #'cam/cider-repl-mode-setup)

;; Delete trailing whitespace that may have been introduced by auto-complete
(eval-after-load 'cider
  '(progn (advice-add #'cider-repl-return :before (lambda ()
                                                    (interactive)
                                                    (call-interactively #'delete-trailing-whitespace)))

          (define-key cider-repl-mode-map (kbd "M-RET") #'cider-switch-to-last-clojure-buffer)))

(setq cider-auto-select-error-buffer nil
      cider-repl-use-pretty-printing t)

(defun cam/cider-repl-messages-buffer ()
  (let ((messages-buffer nil))
    (dolist (buf (buffer-list))
      (unless messages-buffer
        (when (string-match-p "^\*nrepl-server .*\*$" (buffer-name buf))
          (setq messages-buffer buf))))
    messages-buffer))


;;; [[<company]]
(setq company-idle-delay 0.01
      company-minimum-prefix-length 1)


;;; [[<dired]]
(defun cam/after-dired-find-file ()
  "After-advice for `dired-find-file'. Kill all `dired' buffers
unless they are the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (with-current-buffer buf
        (when (eq major-mode 'dired-mode)
          (kill-buffer buf))))))

(defun cam/dired-mode-setup ()
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook #'cam/dired-mode-setup)

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

(eval-after-load 'dired
  '(progn
     (require 'dired-x)                           ; dired-smart-shell-command, dired-jump (C-x C-j), etc.

     (add-hook 'focus-in-hook #'cam/revert-dired-buffers)

     (advice-add #'dired-do-delete :around #'cam/around-dired-do-delete)
     (advice-add #'dired-find-file :after  #'cam/after-dired-find-file)

     (advice-add #'dired-smart-shell-command      ; after running a shell command in dired revert the buffer right away
         :after (lambda (&rest _)
                  (revert-buffer)))))

(setq dired-recursive-copies  'always
      dired-recursive-deletes 'always)

;;; [[<Emacs Lisp]]
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

(eval-after-load 'elisp-slime-nav
  '(define-key elisp-slime-nav-mode-map (kbd "C-c C-d") #'elisp-slime-nav-describe-elisp-thing-at-point)) ; instead of C-c C-d d

(eval-after-load (if (>= emacs-major-version 25) 'elisp-mode ; Emacs Lisp stuff was moved to elisp-mode in Emacs 25
                   'lisp-mode)
  '(progn
     (define-key emacs-lisp-mode-map (kbd "<C-M-s-return>") #'cam/emacs-lisp-save-switch-to-ielm-if-visible)
     (define-key emacs-lisp-mode-map (kbd "C-c RET")        #'cam/emacs-lisp-macroexpand-last-sexp)
     (define-key emacs-lisp-mode-map (kbd "C-x C-e")        #'pp-eval-last-sexp)))

(eval-after-load 'dash
  '(dash-enable-font-lock))

(defun cam/emacs-lisp-mode-setup ()
  (require 'subr-x) ; when-let, etc.
  (cam/lisp-mode-setup)
  (aggressive-indent-mode 1)
  (cam/suppress-messages
    (auto-complete-mode 1))
  (elisp-slime-nav-mode 1)
  (morlock-mode 1)
  (wiki-nav-mode 1)

  (when (string= (buffer-file-name) user-init-file)
    (add-hook 'after-save-hook
              (lambda ()
                (byte-compile-file (buffer-file-name) :load))
              nil
              :local)))
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

(eval-when-compile
  (require 'ielm))

(eval-after-load 'ielm
  '(define-key inferior-emacs-lisp-mode-map (kbd "C-c RET") #'cam/emacs-lisp-macroexpand-last-sexp))

;; Indentation <3
(eval-after-load 'nadvice
  '(put #'advice-add 'lisp-indent-function 2))


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


;;; [[<Git Commit Mode]]
(defun cam/git-commit-mode-setup ()
  (flyspell-mode 1))
(add-hook 'git-commit-mode-hook #'cam/git-commit-mode-setup)


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
(setq helm-buffers-fuzzy-matching t               ; enable fuzzy matching for helm
      helm-recentf-fuzzy-match t
      helm-M-x-fuzzy-match t)


;;; [[<js2-mode]]
(defun cam/js2-mode-setup ()
  (electric-pair-local-mode 1)
  (rainbow-delimiters-mode 1)
  (skewer-mode 1)
  (ac-js2-mode 1)

  (unless (skewer-ping)
    (run-skewer)))
(add-hook 'js2-mode-hook #'cam/js2-mode-setup)

(eval-after-load 'js2-mode
  '(progn (define-key js2-mode-map (kbd "C-j") #'newline) ; instead of electrict-newline-maybe-indent, which doesn't indent
          (define-key js2-mode-map (kbd "M-j") nil)))     ; keep #'cam/join-next-line instead of whatever weird js2-mode fn


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

(eval-after-load 'magit
  '(progn
     (define-key magit-status-mode-map (kbd "M-!") #'cam/magit-shell-command)
     (define-key magit-status-mode-map (kbd "V")   #'cam/magit-visit-pull-request-url)
     (define-key magit-status-mode-map (kbd "s-u") #'magit-refresh)

     (add-hook 'focus-in-hook #'cam/refresh-magit-buffers)))

(setq magit-auto-revert-mode-lighter     ""
      magit-last-seen-setup-instructions "1.4.0")


;;; [[<Org]]
(defun cam/org-mode-setup ()
  (flyspell-mode 1)
  (setq-local truncate-lines nil)

  (define-key org-mode-map (kbd "C-c c") #'cam/org-insert-code-block))
(add-hook 'org-mode-hook #'cam/org-mode-setup)

(setq org-support-shift-select nil)

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


;;; [[<Paredit]]
(eval-after-load 'paredit
  ;; Tell paredit it's ok to delete selection in these contexts. Otherwise delete-selection-mode doesn't work with paredit
  '(progn (put #'paredit-forward-delete  'delete-selection 'supersede)
          (put #'paredit-backward-delete 'delete-selection 'supersede)
          (put #'paredit-open-round      'delete-selection t)
          (put #'paredit-open-square     'delete-selection t)
          (put #'paredit-doublequote     'delete-selection t)
          (put #'paredit-newline         'delete-selection t)))


;;; [[[<Sly]]
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(defun cam/sly-setup ()
  (cam/lisp-mode-setup)
  (auto-complete-mode 1)

  (require 'ac-sly)
  (set-up-sly-ac :fuzzy))

(add-hook 'sly-mode-hook #'cam/sly-setup)


;;; [[<Web Mode]]
(defun cam/web-mode-setup ()
  (aggressive-indent-mode 1)
  (electric-pair-local-mode 1)
  (rainbow-delimiters-mode 1)

  (define-key web-mode-map
    (kbd "C-j") #'newline))                       ; instead of electric-newline-and-maybe-indent which doesn't indent :/
(add-hook 'web-mode-hook #'cam/web-mode-setup)


;;; [[<YASnippet]]
(eval-after-load 'yasnippet
  '(progn
     (define-key yas-minor-mode-map (kbd "H-w") #'aya-create)
     (define-key yas-minor-mode-map (kbd "H-y") #'aya-expand)))

(setq yas-verbosity 0)                            ; Don't need to see a message every time a snippet file is loaded


;;; ---------------------------------------- [[<Final Setup]] ----------------------------------------

;; Byte-compile init.el if needed for next time around
(let* ((init-file (expand-file-name (concat user-emacs-directory "init.el"))) ; don't use var user-init-file because it will be set to the .elc file while loading
       (compiled-init-file (concat init-file "c")))
  (when (or (not compiled-init-file)
            (file-newer-than-file-p init-file compiled-init-file))
    (byte-compile-file init-file)))

(ignore-errors
  (load-file custom-file))

(unless cam/has-loaded-init-p
  (toggle-frame-maximized))

(setq cam/has-loaded-init-p t)

(ignore-errors ; only seems to work on Emacs 25+
  (message "Loaded init.el in %.0f ms." (* (float-time (time-subtract after-init-time before-init-time)) 1000.0)))


;; ---------------------------------------- [[<Experimental]] ----------------------------------------

;; ---------------------------------------- [[<cam/align-map]] ----------------------------------------
(defun cam/get-max-col (&optional max)
  (save-excursion
    (condition-case _
        (progn
          (backward-sexp 2)                          ; Move from end of val to beginning of key
          (forward-sexp)                             ; Move to end of key
          (let ((col (+ (current-column) 1)))        ; val should start one space after key
            (backward-sexp)                          ; Move back to start of key
            (cam/get-max-col (max (or max 0) col)))) ; recurse until error is thrown when we reach the first key
      (error (message "Max column is %d" max)
             max))))

(defun cam/align-map-args-to-column ()
  (save-excursion
    (ignore-errors
      (backward-sexp)                                ; move to start of val
      (cam/insert-spaces-to-goal-column nil)         ; insert spaces
      (backward-sexp)                                ; move to start of key
      (cam/align-map-args-to-column))))              ; recurse until error is thrown when we reach the first sexp

(defun cam/align-map ()
  (interactive)
  (save-excursion
    (when (paredit-in-string-p)                      ; If we're in a string jump out so we don't insert a } when calling (paredit-close-curly)
      (paredit-forward-up))
    (paredit-close-curly)                            ; jump to char after closing }
    (backward-char)                                  ; move back onto } -- end of last sexp
    (setq-local cam/insert-spaces-goal-col (cam/get-max-col))
    (cam/align-map-args-to-column))
  (paredit-reindent-defun))

(global-set-key (kbd "C-s-;") #'cam/align-map)


;; ---------------------------------------- [[<cam/auto-update-packages]] ----------------------------------------

(defun cam/auto-update-packages ()
  (message "Emacs has been inactive for 60 minutes! Auto-updating packages...")
  (package-refresh-contents)
  (cl-letf (((symbol-function 'save-some-buffers) (lambda (&rest _)))
            ((symbol-function 'y-or-n-p)          (lambda (_) t)))
    (save-window-excursion
      (package-list-packages-no-fetch)
      (package-menu-mark-upgrades)
      (ignore-errors
        (package-menu-execute :noquery))
      (package-menu-mark-obsolete-for-deletion)
      (ignore-errors
        (package-menu-execute :noquery))
      (package-autoremove)
      ;; Kill all the package buffers in case there's more than one
      (ignore-errors
        (while (kill-buffer "*Packages*"))))))
(run-with-idle-timer (* 60 60) :repeat #'cam/auto-update-packages)


;; ---------------------------------------- [[<cam/cleanup-extra-buffers]] ----------------------------------------

(cl-defun cam/buffer-window (buffer)
  "Return the first window on any frame showing BUFFER, if any."
  (dolist (frame (frame-list))
    (dolist (w (window-list frame))
      (when (eq (window-buffer w) buffer)
        (cl-return-from cam/buffer-window w)))))

(defconst cam/buffer-auto-delete-exclusion-patterns
  '("^\\*Messages\\*$"
    "^\\*cider-repl"
    "^\\*nrepl-server")
  "Patterns of buffer names that should never be deleted by `cam/cleanup-extra-buffers'.")

(defun cam/should-delete-buffer (buf)
  (and (or (not (buffer-file-name buf))
           (not (buffer-modified-p buf))
           (get-buffer-process buf))
       (not (cam/buffer-window buf))
       (cl-notany (lambda (pattern)
                    (string-match pattern (buffer-name buf)))
                  cam/buffer-auto-delete-exclusion-patterns)))

(defun cam/cleanup-extra-buffers ()
  "Remove unused buffers whenever Emacs has been idle for 2 minutes."
  (message "Cleaning extra buffers...")
  (let ((case-fold-search :ignore-case))
    (dolist (buf (buffer-list))
      (when (cam/should-delete-buffer buf)
        (kill-buffer buf)))))
(run-with-idle-timer (* 30) :repeat #'cam/cleanup-extra-buffers)


;; ---------------------------------------- [[<Messages Auto-Scrolling]] ----------------------------------------

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


;; ---------------------------------------- [[<cam/time]] ----------------------------------------

(defmacro time (&rest body)
  "Evaluate BODY and echo the amount of time it took, and return its result.
Like Clojure's `time'."
  (let ((start-time (cl-gensym "start-time-"))
        (result     (cl-gensym "result-"))
        (end-time   (cl-gensym "end-time-")))
    `(let* ((,start-time (current-time))
            (,result     (progn
                           ,@body))
            (,end-time   (current-time)))
       (let ((elapsed-microseconds (+ (* (- (cl-second ,end-time)
                                            (cl-second ,start-time)) 1000000)
                                      (- (cl-third ,end-time)
                                         (cl-third ,start-time)))))
         (apply #'message "Elapsed time: %.1f %s." (cond
                                                    ((< elapsed-microseconds 1000)    `(,elapsed-microseconds "µs"))
                                                    ((< elapsed-microseconds 1000000) `(,(/ elapsed-microseconds 1000.0) "ms"))
                                                    (:else                            `(,(/ elapsed-microseconds 1000000.0) "seconds")))))
       ,result)))
