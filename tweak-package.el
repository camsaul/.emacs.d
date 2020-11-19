;;; tweak-package -- macro for organizing personal customizations for a package -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(cl-defmacro tweak-package (package &key
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
       '(progn ,@(cl-loop for p in (cons package require)
                          collect `(require ',p))))
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

(provide 'tweak-package)
;;; tweak-package.el ends here
