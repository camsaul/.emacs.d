;;; metabase.el -- Minor mode for editing Metabase Clojure files -*- lexical-binding: t; coding: utf-8; -*-

;; Copyright © 2015 Metabase, Inc.
;;
;; Authors: Cam Saül <cam@metabase.com>
;;
;; Package-Version: 0.1
;; Keywords: clojure metabase
;; Package-Requires: ((emacs "24.5"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock and indentation for Clojure files that are part of the Metabase project.
;; Font-locking requires `paredit'.

;; `metabase-mode' can automatically enable itself when visiting the appropriate Clojure buffers.
;; add the following to your `init.el':
;;
;; (add-hook 'clojure-mode-hook #'metabase-mode-maybe-enable)

;;; License:

;; TODO license blurb

;;; Code:

(require 'clojure-mode)
(require 'paredit)

(defconst metabase--font-lock-keywords
  '(("\\<\\([[:upper:]-]+[[:punct:]]?\\)\\>" 1 (when (paredit-in-string-p)
                                                 'font-lock-variable-name-face)
     prepend)
    ("`\\([[:alnum:]_<>*-/:]+\\)`" 1 (when (paredit-in-string-p)
                                       'font-lock-constant-face)
     prepend))
  "Font-lock keywords to add when `metabase-mode' is active.")

(defconst metabase--mode-lighter
  " metabase")

(defvar-local metabase-mode nil
  "Whether `metabase-mode' is enabled in the current buffer.")

;;;###autoload
(defun metabase-mode (&optional arg)
  "Enable/disable/toggle `metabase-mode' in the current buffer.
ARG greter than 0 enables the mode, less than 0 disables it, and no argument
toggles it."
  (interactive)
  (if (null arg) (progn (metabase-mode (if metabase-mode -1 1))
                        (when (called-interactively-p 'interactive)
                          (message "Docstr font-locking %s in current buffer."
                                   (if metabase-mode "enabled" "disabled"))))
    (let ((enable (> arg 0)))
      (when enable
        (add-to-list 'minor-mode-alist (list 'metabase-mode
                                             metabase--mode-lighter)))
      (funcall (if enable #'font-lock-add-keywords #'font-lock-remove-keywords)
               nil metabase--font-lock-keywords)
      (setq-local metabase-mode enable))
    (font-lock-flush)
    (font-lock-ensure)))

;;;###autoload
(defun metabase-mode-maybe-enable ()
  "Enable `metabase-mode' in the current buffer if it's a Metabase file."
  (when (and (buffer-file-name)
             (string-match "src/metabase/.*\\.clj$" (expand-file-name (buffer-file-name))))
    (metabase-mode 1)))

(define-clojure-indent
  (api-let 2)
  (assoc* 1)
  (auto-parse 1)
  (catch-api-exceptions 0)
  (check 1)
  (context 2)
  (create-database-definition 1)
  (execute-query 1)
  (expect 1)
  (expect-eval-actual-first 1)
  (expect-expansion 0)
  (expect-let 1)
  (expect-when-testing-against-dataset 1)
  (expect-when-testing-mongo 1)
  (expect-with-all-drivers 1)
  (expect-with-dataset 1)
  (expect-with-datasets 1)
  (ins 1)
  (let-400 1)
  (let-404 1)
  (let-500 1)
  (match 1)
  (match-$ 1)
  (macrolet 1)
  (org-perms-case 1)
  (pdoseq 1)
  (qp-expect-with-datasets 1)
  (resolve-private-fns 1)
  (symbol-macrolet 1)
  (sync-in-context 2)
  (upd 2)
  (when-testing-dataset 1)
  (with-credentials 1))

(put 'defannotation 'clojure-doc-string-elt 2)
(put 'defendpoint 'clojure-doc-string-elt 3)
(put 'defhook 'clojure-doc-string-elt 2)
(put 'defsetting 'clojure-doc-string-elt 2)

(provide 'metabase)
;;; metabase.el ends here
