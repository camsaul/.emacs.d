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
;;;    [[Org]]
;;;    [[Paredit]]
;;;    [[Sly]]
;;;    [[Web Mode]]
;;;    [[YASnippet]]
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
 '(inhibit-startup-echo-area-message (user-login-name)))

(add-to-list 'safe-local-variable-values '(cam/byte-compile . t))

(defvar cam/has-loaded-init-p nil
  "Have we done a complete load of the init file yet? (Use this to keep track of things we only want to run once, but not again if we call eval-buffer).")


;;; ---------------------------------------- [[<Package Setup]] ----------------------------------------

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


;;; ---------------------------------------- [[<Global Setup]] ----------------------------------------

;;; [[<Theme]]

(eval-when-compile
  (require 'moe-theme))
(require 'moe-theme)

;; Load the theme just once, otherwise the screen will flicker all cray if we try to eval this buffer again
(unless cam/has-loaded-init-p
  (moe-dark)
  (set-frame-font "Source Code Pro-12" (not :keep-size) t)) ; t = apply font to all frames going forward & save setting to custom.el (supposedly)

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
      visible-bell t)

(setq-default indent-tabs-mode nil                ; disable insertion of tabs
              save-place t                        ; Automatically save place in each file
              truncate-lines t)                   ; don't display "continuation lines" (don't wrap long lines)


;;; [[<Global Macros]]

(defmacro cam/suppress-messages (&rest body)
  "Evaluate BODY with the `message' function temporarily bound to `ignore'."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'message) #'ignore))
     ,@body))

(defmacro cam/time (&rest body)
  "Evaluate BODY and echo the amount of time it took, and return its result.
Like Clojure's `time'."
  (declare (indent 0))
  (let ((start-time (make-symbol "start-time"))
        (result     (make-symbol "result"))
        (elapsed    (make-symbol "elapsed-time"))
        (body       (if (cdr body) `(progn ,@body)
                      (car body))))
    `(let* ((,start-time (cam/current-microseconds))
            (,result     ,body)
            (,elapsed   (- (cam/current-microseconds)
                           ,start-time)))
       (apply #'message ,(let* ((form-str (prin1-to-string body))
                                (form-str (if (> (length form-str) 50) (concat (substring form-str 0 50) "...")
                                            form-str)))
                           (concat form-str " elapsed time: %.1f %s."))
              (cond ((< ,elapsed 1000)    `(,,elapsed "µs"))
                    ((< ,elapsed 1000000) `(,(/ ,elapsed 1000.0) "ms"))
                    (:else                `(,(/ ,elapsed 1000000.0) "seconds"))))
       ,result)))

(defmacro cam/time* (&rest forms)
  "Evalute each form in FORMS and echo a message about how long each took to execute."
  (declare (indent 0))
  `(progn ,@(cl-loop for form in forms
                     collect `(cam/time ,form))))

(cl-defmacro cam/use-package (package &key
                                      (mode-name (intern (format "%s-mode" (symbol-name package))))
                                      (hook-name (intern (format "%s-hook" (symbol-name mode-name))))
                                      declare
                                      vars
                                      require
                                      advice
                                      load
                                      minor-modes
                                      setup
                                      local-vars
                                      local-hooks
                                      (keymap (intern (format "%s-map" (symbol-name mode-name))))
                                      keys
                                      auto-mode-alist)
  (declare (indent 1))
  `(progn
     (eval-when-compile
       ,@(cl-loop for p in (cons package require)
                  collect `(require ',p)))
     ,@(cl-loop for f in declare
                collect `(declare-function ,f ,(symbol-name package)))
     ,@(cl-loop for (var . value) in vars
                collect `(setq ,var ,value))
     ,(when (or require advice load keys)
        `(eval-after-load ',package
           '(progn
              ,@(cl-loop for other-package in require
                         collect `(require ',other-package))
              ,@(cl-loop for item in advice
                         collect `(advice-add ,@item))
              ,@load
              ,@(cl-loop for (binding . command) in keys
                         collect `(define-key ,keymap (kbd ,binding) ,command)))))
     ,@(when (or minor-modes setup local-vars local-hooks)
         (let ((setup-fn-name (intern (format "cam/%s-setup" (symbol-name mode-name)))))
           `((defun ,setup-fn-name ()
               ,@(cl-loop for minor-mode in minor-modes
                          collect `(,minor-mode 1))
               ,@setup
               ,@(cl-loop for (var . value) in local-vars
                          collect `(setq-local ,var ,value))
               ,@(cl-loop for (hook . fun) in local-hooks
                          collect `(add-hook ',hook ,fun ,(not :append) :local)))
             (add-hook ',hook-name (function ,setup-fn-name)))))
     ,@(cl-loop for pattern in auto-mode-alist
                collect `(add-to-list 'auto-mode-alist '(,pattern . ,mode-name)))))


;;; [[<Global Functions]]

(defun cam/current-microseconds ()
  "Return the current time in microseconds."
  (cl-destructuring-bind (_ seconds microseconds _) (current-time)
    (+ (* seconds 1000000) microseconds)))

(defun cam/untabify-current-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun cam/join-next-line ()
  (interactive)
  (join-line -1))

;; TODO - why not just make this an autoload???
(defun cam/loccur ()
  (interactive)
  (require 'loccur)
  (call-interactively #'loccur))

(defun cam/backward-kill-line ()
  "Kill line from current cursor position to beginning of line."
  (interactive)
  (kill-line 0))

(defun cam/hungry-delete ()
  "Delete all but one spaces after `point'."
  (interactive)
  (while (= (char-after (1+ (point))) 32)
    (delete-char 1)))

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
  (let ((s (copy-sequence string)))
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

(cl-defun cam/realign-eol-comment-current-line ()
  "Realign the end-of-line comment for the current line to `comment-column'."
  (interactive)
  (unless comment-column
    (error "comment-column is not set!"))
  (save-excursion
    (beginning-of-line)
    (let ((current-line (line-number-at-pos (point))))
      (search-forward ";")
      (backward-char)
      ;; Check to make sure we're still on the same line
      (unless (= (line-number-at-pos (point)) current-line)
        (cl-return-from cam/realign-eol-comment-current-line)))
    ;; Make sure this is just a single ";"
    (when (= (char-after (1+ (point))) ?\;)
      (cl-return-from cam/realign-eol-comment-current-line))
    ;; calculate the number of spaces to add / remove and do so
    (let ((cols-difference (- comment-column
                              (current-column))))
      ;; TODO - should we make sure we don't delete any chars that aren't spaces ?
      (cond
       ((> cols-difference 0) (insert-char ?  cols-difference))
       ((< cols-difference 0) (delete-char cols-difference))))))

(defun cam/realign-eol-comments ()
  "Re-align end-of-line comments to `comment-column' in the current region
if it is active; otherwise re-align comments on the current line."
  (interactive)
  (if (not (region-active-p)) (cam/realign-eol-comment-current-line)
    (let ((last-line (line-number-at-pos (region-end)))
          (line (line-number-at-pos (region-beginning))))
      (save-excursion
        (goto-char (region-beginning))
        (while (<= (prog1 line (cl-incf line)) last-line)
          (cam/realign-eol-comment-current-line)
          (forward-line))))))

(defun cam/-align-map-get-max-col (&optional max)
  (save-excursion
    (condition-case _
        (progn
          (backward-sexp 2)                          ; Move from end of val to beginning of key
          (forward-sexp)                             ; Move to end of key
          (let ((col (+ (current-column) 1)))        ; val should start one space after key
            (backward-sexp)                          ; Move back to start of key
            (cam/-align-map-get-max-col (max (or max 0) col)))) ; recurse until error is thrown when we reach the first key
      (error (message "Max column is %d" max)
             max))))

(defun cam/-align-map-args-to-column ()
  (save-excursion
    (ignore-errors
      (backward-sexp)                                ; move to start of val
      (cam/insert-spaces-to-goal-column nil)         ; insert spaces
      (backward-sexp)                                ; move to start of key
      (cam/-align-map-args-to-column))))              ; recurse until error is thrown when we reach the first sexp

(defun cam/align-map ()
  "Align the values in a Clojure map."
  (interactive)
  (save-excursion
    (when (paredit-in-string-p)                      ; If we're in a string jump out so we don't insert a } when calling (paredit-close-curly)
      (paredit-forward-up))
    (paredit-close-curly)                            ; jump to char after closing }
    (backward-char)                                  ; move back onto } -- end of last sexp
    (setq-local cam/insert-spaces-goal-col (cam/-align-map-get-max-col))
    (cam/-align-map-args-to-column))
  (paredit-reindent-defun))


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

(cl-loop for (key . command) in '(("<A-escape>"    . #'helm-mark-ring)
                                  ("<A-return>"    . #'wiki-nav-ido)
                                  ("<C-M-s-down>"  . #'windmove-down)
                                  ("<C-M-s-left>"  . #'cam/windmove-left-or-other-frame)
                                  ("<C-M-s-right>" . #'cam/windmove-right-or-other-frame)           ; Use <f11> <key> for toggling various minor modes
                                  ("<C-M-s-up>"    . #'windmove-up)
                                  ("<H-SPC>"       . #'mc/mark-all-like-this)
                                  ("<H-escape>"    . #'ace-jump-line-mode)
                                  ("<H-return>"    . #'mc/mark-next-lines)
                                  ("<S-delete>"    . #'cam/hungry-delete)
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
         do (global-set-key (kbd key) (eval command)))


;;; ---------------------------------------- [[<Mode/Package Specific Setup]] ----------------------------------------

;;; [[<etc]]

(cam/use-package button-lock
  :load ((diminish 'button-lock-mode)))

(cam/use-package wiki-nav
  :load ((diminish 'wiki-nav-mode)))

(cam/use-package highlight-parentheses
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
(cam/use-package auto-complete
  :declare (ac-complete-functions ac-complete-symbols ac-complete-variables)
  :vars ((ac-delay . 0.05)
         (ac-auto-show-menu . 0.1)
         (ac-candidate-menu-height . 30)
         (ac-menu-height . 30)         ; show 20 results instead of 10
         (ac-quick-help-prefer-pos-tip . t) ; use native tooltips provided by pos-tip
         (ac-quick-help-delay . 0.2)
         (ac-use-menu-map . t))
  :load ((cam/suppress-messages
           (require 'pos-tip)

           (ac-config-default)

           (add-to-list 'ac-modes 'cider-repl-mode)
           (add-to-list 'ac-modes 'ielm-mode)))
  :keymap ac-menu-map
  :keys (("A-f" . #'ac-complete-functions)
         ("A-s" . #'ac-complete-symbols)
         ("A-v" . #'ac-complete-variables)))

(cam/use-package auto-complete-config
  :declare (ac-emacs-lisp-mode-setup))


;;; [[<Clojure]]
(defun cam/clojure-save-load-switch-to-cider ()
  (interactive)
  (save-buffer)

  (if (not (cider-connected-p)) (cider-jack-in)
    (cider-load-buffer)
    (cider-repl-set-ns (cider-current-ns))
    (cider-switch-to-relevant-repl-buffer)
    (cider-repl-clear-buffer)))

(cam/use-package clojure-mode
  :mode-name clojure-mode
  :require (clojure-mode-extra-font-locking)
  :load ((clojure-snippets-initialize))
  :minor-modes (auto-complete-mode
                clj-refactor-mode)
  :setup ((cam/lisp-mode-setup)
          (ac-cider-setup)
          (eval-after-load 'yasnippet
            '(add-to-list 'ac-sources 'ac-source-yasnippet))
          (cljr-add-keybindings-with-modifier "A-H-"))
  :local-vars nil
  :local-hooks nil
  :keys (("<C-M-s-return>" . #'cam/clojure-save-load-switch-to-cider)))

(cam/use-package clj-refactor
  :load ((diminish 'clj-refactor-mode)))

(defun cam/cider-repl-messages-buffer ()
  (let ((messages-buffer nil))
    (dolist (buf (buffer-list))
      (unless messages-buffer
        (when (string-match-p "^\*nrepl-server .*\*$" (buffer-name buf))
          (setq messages-buffer buf))))
    messages-buffer))

(cam/use-package cider
  :mode-name cider-repl-mode
  :declare (cider-jack-in)
  :vars ((cider-auto-select-error-buffer . nil)
         (cider-repl-use-pretty-printing . t))
  :advice ((#'cider-repl-return :before (lambda ()
                                          "Delete trailing whitespace that may have been introduced by `auto-complete'."
                                          (interactive)
                                          (call-interactively #'delete-trailing-whitespace))))
  :minor-modes (auto-complete-mode
                aggressive-indent-mode)
  :setup ((cam/lisp-mode-setup)
          (ac-cider-setup))
  :keys (("M-RET" . #'cider-switch-to-last-clojure-buffer)))

(cam/use-package cider-interaction
  :declare (cider-connected-p cider-current-ns cider-load-buffer cider-switch-to-last-clojure-buffer cider-switch-to-relevant-repl-buffer))

(cam/use-package cider-repl
  :declare (cider-repl-clear-buffer cider-repl-return cider-repl-set-ns))


;;; [[<company]]
(cam/use-package company
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

(cam/use-package dired
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

(cam/use-package dired-x
  :declare (dired-smart-shell-command))


;;; [[<Emacs Lisp]]
(defvar-local cam/byte-compile nil
  "Make this a file-local variable and we'll byte compile it whenever it's saved.")

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
(cam/use-package elisp-mode
  :mode-name emacs-lisp-mode
  :load ((put 'add-hook 'lisp-indent-function 1))
  :minor-modes (aggressive-indent-mode
                auto-complete-mode
                eldoc-mode
                elisp-slime-nav-mode
                morlock-mode
                wiki-nav-mode)
  :setup ((cam/lisp-mode-setup))
  :local-hooks ((after-save-hook . (lambda ()
                                     (when cam/byte-compile
                                       (byte-compile-file (buffer-file-name) :load)))))
  :keys (("<C-M-s-return>" . #'cam/emacs-lisp-save-switch-to-ielm-if-visible)
         ("C-c RET"        . #'cam/emacs-lisp-macroexpand-last-sexp)
         ("C-x C-e"        . #'pp-eval-last-sexp)))

(cam/use-package dash
  :declare (dash-enable-font-lock)
  :load ((dash-enable-font-lock)))

(cam/use-package elisp-slime-nav
  :load ((diminish 'elisp-slime-nav-mode))
  :keys (("C-c C-d" . #'elisp-slime-nav-describe-elisp-thing-at-point)))

(cam/use-package ielm
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

(cam/use-package nadvice
  :load ((put #'advice-add 'lisp-indent-function 2)))


;;; [[<Eval Expresssion (Minibuffer)]]
(cam/use-package simple
  :hook-name eval-expression-minibuffer-setup-hook
  :minor-modes (company-mode
                paredit-mode)
  :local-vars ((company-echo-delay . 10)))


;;; [[<Find Things Fast]]
(cam/use-package find-things-fast
  :load ((nconc ftf-filetypes '("*.clj"
                                "*.css"
                                "*.el"
                                "*.html"
                                "*.js"
                                "*.java"
                                "*.md"
                                "*.yml"))))


;;; [[<Git Commit Mode]]
(cam/use-package git-commit-mode
  :mode-name git-commit-mode
  :minor-modes (flyspell-mode))


;;; [[<Guide Key]]
(cam/use-package guide-key
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
(cam/use-package helm
  :vars ((helm-buffers-fuzzy-matching . t) ; enable fuzzy matching for helm
         (helm-recentf-fuzzy-match    . t)
         (helm-M-x-fuzzy-match        . t)))


;;; [[<js2-mode]]
(cam/use-package js2-mode
  :mode-name js2-mode
  :minor-modes (ac-js2-mode
                electric-pair-local-mode
                rainbow-delimiters-mode
                skewer-mode)
  :setup ((unless (skewer-ping)
            (run-skewer)))
  :keys (("C-j" . #'newline)
         ("M-j" . nil))
  :auto-mode-alist ("\.js$"))

(cam/use-package skewer-mode
  :declare (skewer-ping))


;;; [[<loccur]]
(cam/use-package loccur
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

(cam/use-package magit
  :mode-name magit-status-mode
  :declare (magit-get magit-get-current-branch magit-get-current-remote magit-refresh)
  :vars ((magit-auto-revert-mode-lighter     . "")
         (magit-last-seen-setup-instructions . "1.4.0"))
  :load ((add-hook 'focus-in-hook #'cam/refresh-magit-buffers))
  :keys (("M-!" . #'cam/magit-shell-command)
         ("V"   . #'cam/magit-visit-pull-request-url)
         ("s-u" . #'magit-refresh)))

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

(cam/use-package org
  :declare (org-bookmark-jump-unhide org-end-of-line org-return-indent)
  :vars ((org-support-shift-select . nil))
  :minor-modes (flyspell-mode)
  :local-vars ((truncate-lines . nil))
  :keys (("C-c c" . #'cam/org-insert-code-block)))

(cam/use-package org-src
  :declare (org-edit-src-code))


;;; [[<Paredit]]
(cam/use-package paredit
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


;;; [[[<Sly]]
(cam/use-package sly
  :vars ((inferior-lisp-program . "/usr/local/bin/sbcl"))
  :require (ac-sly)
  :minor-modes (auto-complete-mode)
  :setup ((cam/lisp-mode-setup)
          (set-up-sly-ac :fuzzy)))


;;; [[<Web Mode]]
(cam/use-package web-mode
  :mode-name web-mode
  :minor-modes (aggressive-indent-mode
                electric-pair-local-mode
                rainbow-delimiters-mode)
  :keys (("C-j" . #'newline))
  :auto-mode-alist ("\.html$"))


;;; [[<YASnippet]]
(cam/use-package yasnippet
  :mode-name yas-minor-mode
  :vars ((yas-verbosity . 0))
  :keys (("H-w" . #'aya-create)
         ("H-y" . #'aya-expand)))

(cam/use-package auto-yasnippet
  :declare (aya-create aya-expand))


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
(rxt-global-mode 1)                               ; Commands for converting and font-locking regular expressions
(save-place-mode 1)                               ; automatically save position in files & start at that position next time you open them
(winner-mode 1)
(yas-global-mode 1)

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
                undo-tree-mode
                yas-minor-mode))
  (diminish mode))


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
        (while (kill-buffer "*Packages*")))
      (message "Auto updated packages."))))
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
    "^\\*sly-mrepl for sbcl\\*$")
  "Patterns of buffer names that should never be deleted by `cam/cleanup-extra-buffers'.")

(defun cam/should-delete-buffer (buf)
  (and (or (not (buffer-file-name buf))
           (not (buffer-modified-p buf)))
       (not (get-buffer-process buf))
       (not (cam/buffer-window buf))
       (cl-notany (lambda (pattern)
                    (string-match pattern (buffer-name buf)))
                  cam/buffer-auto-delete-exclusion-patterns)))

(defun cam/cleanup-extra-buffers ()
  "Remove unused buffers whenever Emacs has been idle for 2 minutes."
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


;;; ---------------------------------------- [[Extra Font Locking]] ----------------------------------------

;; special form - purple
;; macro - blue
;; function - green
;; var - orange
(defconst cam/rainbow-elisp-mode-keywords
  '(("\\<\\(nil\\|t\\)\\>" 1 (unless (or (paredit-in-string-p)
                                         (paredit-in-comment-p))
                               'font-lock-builtin-face)
     keep)
    ("[^:]\\<\\([[:lower:]-]+[[:lower:]]\\)\\>"
     0 (-when-let (symb (intern-soft (match-string 1)))
         (unless (or (paredit-in-string-p)
                     (paredit-in-comment-p))
           (cond
            ((special-form-p symb) 'font-lock-builtin-face)
            ((macrop (symbol-function symb)) 'font-lock-constant-face)
            ((fboundp symb) 'font-lock-keyword-face)
            ((and (boundp symb)
                  (not (keywordp symb))) 'font-lock-variable-name-face)
            ((featurep symb) 'italic))))
     prepend)
    ("\\(\\(?:#?'\\)?\\<cam/[[:lower:]-]+[[:lower:]]\\)\\>"
     1 (unless (or (paredit-in-string-p)
                   (paredit-in-comment-p))
         'font-lock-type-face)
     prepend)))

(eval-after-load 'elisp-mode
  '(font-lock-add-keywords 'emacs-lisp-mode cam/rainbow-elisp-mode-keywords))


;;; ---------------------------------------- [[cam/todo-font-lock-mode]] ----------------------------------------

(defconst cam/todo-font-lock-mode-keywords
  '(("\\<\\(TODO\\|HACK\\)\\>" 1 (when (paredit-in-comment-p)
                                   'font-lock-warning-face)
     prepend)))

(defconst cam/todo-font-lock-mode-lighter
  " cam/todo-fl")

(defvar-local cam/todo-font-lock-mode nil)

(defun cam/todo-font-lock-mode (&optional arg)
  (interactive)
  (if (null arg) (progn (cam/todo-font-lock-mode (if cam/todo-font-lock-mode -1 1))
                        (when (called-interactively-p 'interactive)
                          (message "TODO Font-Lock %s in current buffer." (if cam/todo-font-lock-mode "enabled" "disabled"))))
    (let ((enable (> arg 0)))
      (when enable
        (add-to-list 'minor-mode-alist (list 'cam/todo-font-lock-mode cam/todo-font-lock-mode-lighter)))
      (funcall (if enable #'font-lock-add-keywords #'font-lock-remove-keywords) nil cam/todo-font-lock-mode-keywords)
      (setq-local cam/todo-font-lock-mode enable))
    (font-lock-flush)
    (font-lock-ensure)))

(add-hook 'clojure-mode-hook #'cam/todo-font-lock-mode)
(add-hook 'emacs-lisp-mode-hook #'cam/todo-font-lock-mode)


;;; ---------------------------------------- [[cam/clojure-docstr-extra-font-lock-mode]] ----------------------------------------

(defconst cam/clojure-docstr-font-lock-keywords
  '(("\\<\\([[:upper:]-]+[[:punct:]]?\\)\\>" 1 (when (paredit-in-string-p)
                                                 'font-lock-variable-name-face)
     prepend)
    ("`\\([[:alnum:]_<>*-/:]+\\)`" 1 (when (paredit-in-string-p)
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
