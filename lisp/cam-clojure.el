;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'cam-macros))

(require 'cam-lisp)

(require 'ansi-color)
(require 'cam-todo-font-lock)
(require 'cider)
(require 'cider-eldoc)
(require 'clj-refactor)
(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'column-enforce-mode)
(require 'company)
(require 'diminish)
(require 'eldoc)
(require 'flycheck)
(require 'flycheck-clj-kondo)
(require 'flyspell)
(require 'lsp-mode)

(declare-function helm-imenu "helm-imenu")

(diminish 'clj-refactor-mode)

(setq cider-auto-select-error-buffer nil
      cider-repl-use-pretty-printing t
      cljr-favor-prefix-notation     nil
      cider-enable-flex-completion   t)

(define-clojure-indent
  (matcha '(1 (:defn)))
  (matche '(1 (:defn)))
  (matchu '(1 (:defn))))

;;;###autoload
(defun cam/clj-switch-to-test-namespace ()
  "Switch to the test namespace for the current buffer; or if this is a test namespace, switch back to the code
namespace."
  (interactive)
  (let* ((directory (file-name-directory buffer-file-name))
         (source-file-p (string-match-p "/src/" directory)))
    (find-file (concat (if source-file-p
                           (replace-regexp-in-string "/src/" "/test/" directory)
                         (replace-regexp-in-string "/test/" "/src/" directory))
                       (if source-file-p
                           (concat (file-name-base buffer-file-name) "_test")
                         (replace-regexp-in-string "_test$" "" (file-name-base buffer-file-name)))
                       "."
                       (file-name-extension buffer-file-name)))))

(defun cam/clj-switch-between-model-and-api-namespaces ()
  "(For Metabase development) if the current buffer is a /model/ namespace, switch to the corresponding /api/ namespace,
and vice versa."
  (interactive)
  (find-file
   (if (string-match-p "/models/" buffer-file-name)
       (replace-regexp-in-string "/models/" "/api/" buffer-file-name)
     (replace-regexp-in-string "/api/" "/models/" buffer-file-name))))

;;;###autoload
(defun cam/clj-save-load-switch-to-cider ()
  (interactive)
  (save-buffer)
  (let ((buffer-ns (cider-current-ns)))
    (save-excursion
      (cider-switch-to-repl-buffer)
      (cider-repl-set-ns buffer-ns)
      (cider-repl-clear-buffer)))
  (let ((callback (cider-load-file-handler
                   (current-buffer)
                   (lambda (_source-buffer)
                     (cider-switch-to-repl-buffer)
                     (set-window-start nil (point-min))))))
    ;;(cider-load-buffer (current-buffer) callback :undef-all)
    (cider-load-buffer (current-buffer) callback)))

(defvar cam/clojure--load-buffer-clean-namespace--namespace-cleaned-p nil)

;;;###autoload
;; (cl-defun cam/clj-load-buffer-clean-namespace (&optional (buffer (current-buffer)))
;;   "When CIDER is active attempt to load BUFFER (by default, the current buffer) and clean its namespace declaration
;; form."
;;   (interactive "bBuffer: ")
;;   (when (not cam/clojure--load-buffer-clean-namespace--namespace-cleaned-p)
;;     (with-demoted-errors "Error cleaning namespace declaration: %S"
;;       (with-current-buffer (get-buffer buffer)
;;         (let ((filename (file-name-nondirectory (buffer-file-name))))
;;           ;; don't run for `project.clj` or EDN files
;;           (when (and (not (string-equal filename "project.clj"))
;;                      (not (string-match-p "\.edn$" filename))
;;                      (cider-current-repl))
;;             ;; unfortunately it doesn't look like you can use `cider-load-buffer` programatically without saving the file
;;             ;; first, because when binding `cider-save-file-on-load` to `nil` it prompts asking whether you want to save
;;             (let ((cider-save-file-on-load t))
;;               (cider-load-buffer))
;;             (cljr-clean-ns)
;;             (when (buffer-modified-p)
;;               ;; prevent recursive calls !
;;               (let ((cam/clojure--load-buffer-clean-namespace--namespace-cleaned-p t))
;;                 (save-buffer)))))))))

;;;###autoload
(defun cam/clj-insert-println (text)
  (interactive "sprintln: ")
  (if current-prefix-arg
      (insert "(println \"" text "\") ; NOCOMMIT")
    (insert "(println \"" text ":\" " text ") ; NOCOMMIT")))

(defun cam/-insert-smallclojure--header (text)
  "Insert a small header like: ;;; --- TEXT ---"
  (let* ((total-width 112)
         (padding (/ (- total-width (length text)) 2))
         (dashes (make-string padding ?-)))
    (insert
     (concat
      ";;; "
      dashes
      " "
      text
      " "
      dashes
      ;; if total-width and text aren't BOTH odd or BOTH even we'll have one less dash than needed so add an extra so
      ;; things line up
      (unless (eq (cl-oddp (length text))
                  (cl-oddp total-width))
        "-")))))

(defun cam/-insert-largeclojure--header (text)
  "Insert a large box-style header."
  (let* ((total-width 112)
         (padding (/ (- total-width (length text)) 2))
         (horizonal-border (concat ";;; +"
                                   (make-string total-width ?-)
                                   "+\n"))
         (text-padding (make-string padding ? )))
    (insert
     (concat
      ;; top row
      horizonal-border
      ;; middle row
      ";;; |"
      text-padding
      text
      text-padding
      ;; if total-width and text aren't BOTH odd or BOTH even we'll have one less space than needed so add an extra so
      ;; things line up
      (unless (eq (cl-oddp (length text))
                  (cl-oddp total-width))
        " ")
      "|\n"
      ;; bottom row
      horizonal-border))))

;;;###autoload
(defun cam/clj-insert-header (text)
  (interactive "sheader text: ")
  (if current-prefix-arg
      (cam/-insert-smallclojure--header text)
    (cam/-insert-largeclojure--header text)))

(defun cam/clojure-find-definition ()
  "Try finding something using LSP. If that fails, try finding it using CIDER."
  (interactive)
  (let ((result (call-interactively #'lsp-find-definition)))
    (when (and (stringp result)
               (string-prefix-p "LSP :: Not found" result))
      (call-interactively #'cider-find-var))))

(cam/tweak-package clojure-mode
  :mode-name clojure-mode
  :minor-modes (cider-mode
                clj-refactor-mode
                company-mode
                column-enforce-mode
                eldoc-mode
                flycheck-mode
                lsp
                cam/todo-font-lock-mode)
  :setup ((cam/lisp-mode-setup)
          (flyspell-prog-mode)
          (cljr-add-keybindings-with-modifier "A-H-")
          (when (fboundp 'auto-complete-mode)
            (auto-complete-mode -1)))
  :local-vars ((cider-redirect-server-output-to-repl . t)
               (clojure-align-forms-automatically . t)        ; vertically aligns some forms automatically (supposedly)
               (clojure-docstring-fill-column . 118)          ; docstring column width of 117
               (company-idle-delay . 0.2)
               (company-minimum-prefix-length . 2)
               (eldoc-documentation-function . #'cider-eldoc)
               (fill-column . 118))                           ; non-docstring column width of 117, which fits nicely on GH
  :keys (("<C-M-return>" . #'cam/clj-save-load-switch-to-cider)
         ("C-j"          . #'newline)
         ("<f1>"         . #'cider-doc)
         ("<f7>"         . #'cam/clj-switch-to-test-namespace)
         ("<f8>"         . #'cam/clj-switch-between-model-and-api-namespaces)
         ("<f9>"         . #'cam/clj-insert-header)
         ("<f10>"        . #'cam/clj-insert-println)
         ("<insert>"     . #'helm-imenu)
         ("<f12> i"      . #'cam/open-metabase-issue-or-pr)
         ("<f12> j"      . #'cider-javadoc)
         ("M-."          . #'cam/clojure-find-definition)))

(advice-add #'cider-repl-return :before
  (lambda ()
    "Delete trailing whitespace that may have been introduced by `auto-complete'."
    (interactive)
    (call-interactively #'delete-trailing-whitespace)))

;; (advice-add #'nrepl-server-filter :around #'cam/clj-ansi-colorize-nrepl-output-buffer-if-needed)

(cam/tweak-package cider
  :mode-name cider-repl-mode
  :minor-modes (company-mode
                eldoc-mode)
  :local-vars ((cider-redirect-server-output-to-repl . t)
               (company-idle-delay . 0.2)
               (company-minimum-prefix-length . 2))
  :setup ((cam/lisp-mode-setup)
          (when (fboundp 'auto-complete-mode)
            (auto-complete-mode -1)))
  :keys (("M-RET"   . #'cider-switch-to-last-clojure-buffer)
         ("RET"     . #'cider-repl-return)
         ("{"       . #'paredit-open-curly)
         ("<f1>"    . #'cider-doc)
         ("<f12> j" . #'cider-javadoc)))

(cam/tweak-package cider-macroexpansion
  :setup ((read-only-mode -1))
  :keys  (("C-c RET" . #'cider-macroexpand-1)))

(provide 'cam-clojure)
