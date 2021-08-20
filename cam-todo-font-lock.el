;;; cam-todo-font-lock -- font-like error words like TODO and DEPRECATED -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defconst cam/todo-font-lock--keywords
  '(("\\<\\(TODO\\|HACK\\|DEPRECATED\\|NOCOMMIT\\)\\>" 1 (when (paredit-in-comment-p)
                                                           'font-lock-warning-face)
     prepend)))

;;;###autoload
(define-minor-mode cam/todo-font-lock-mode
  "Add extra font-locking for words like TODO and DEPRECATED."
  :lighter ""
  (if cam/todo-font-lock-mode
      (font-lock-add-keywords nil cam/todo-font-lock--keywords)
    (font-lock-remove-keywords nil cam/todo-font-lock--keywords))
  (font-lock-flush)
  (font-lock-ensure))

(provide 'cam-todo-font-lock)
;;; cam-todo-font-lock.el ends here
