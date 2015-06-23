;;; todo-font-lock -- font-like error words like TODO and DEPRECATED -*- cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:
;;; Code:

(defconst todo-font-lock-mode-keywords
  '(("\\<\\(TODO\\|HACK\\)\\>" 1 (when (paredit-in-comment-p)
                                   'font-lock-warning-face)
     prepend)))

;;;###autoload
(define-minor-mode todo-font-lock-mode
  "Add extra font-locking for words like TODO and DEPRECATED."
  :lighter " cam/TodoFL"
  (if todo-font-lock-mode (font-lock-add-keywords nil todo-font-lock-mode-keywords)
    (font-lock-remove-keywords nil todo-font-lock-mode-keywords))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'todo-font-lock)
;;; todo-font-lock.el ends here
