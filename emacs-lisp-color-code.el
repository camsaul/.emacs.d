;;; emacs-lisp-color-code -- Elisp minor mode for color-coding symbols based on their type -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:

;; special form - purple
;; macro - blue
;; function - green
;; var - orange

;;; Code:

(defconst emacs-lisp-color-code--keywords
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

;;;###autoload
(define-minor-mode emacs-lisp-color-code-mode
  "Font-lock Emacs Lisp symbols based on their type.
Macros, functions, variables, and special-forms are all highlighed differently."
  :lighter " cam/ClrCd"
  (if emacs-lisp-color-code-mode (font-lock-add-keywords nil emacs-lisp-color-code--keywords)
    (font-lock-remove-keywords nil emacs-lisp-color-code--keywords))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'emacs-lisp-color-code)
;;; emacs-lisp-color-code.el ends here
