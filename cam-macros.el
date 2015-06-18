;;; -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;; [[etc]]
;; [[cam/use-package]]
;; [[time]]
;; [[Lazy Loading]]

(eval-when-compile
  (require 'cl-lib))

;; ---------------------------------------- [[<etc]] ----------------------------------------

;;;###autoload
(defmacro cam/suppress-messages (&rest body)
  (declare (indent 0))
  `(cl-letf (((symbol-function 'message) (lambda (&rest _))))
     ,@body))


;;; ---------------------------------------- [[<cam/use-package]] ----------------------------------------

;; TODO - function declarations and autoloads ?
;;;###autoload
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
       ,(when (or vars advice load setup local-vars local-hooks keys)
          `(require ',package))
       ,@(when require
           (mapcar (lambda (other-package)
                     `(require ',other-package))
                   require)))
     ,@(when declare
         (mapcar (lambda (f)
                   `(declare-function ,f ,(symbol-name package)))
                 declare))
     ,@(when vars
         (mapcar (lambda (var.value)
                   `(setq ,(car var.value) ,(cdr var.value)))
                 vars))
     ,(when (or require advice load keys)
        `(eval-after-load ',package
           '(progn
              ,@(when require
                  (mapcar (lambda (other-package)
                            `(require ',other-package))
                          require))
              ,@(when advice
                  (mapcar (lambda (item)
                            `(advice-add ,@item))
                          advice))
              ,@load
              ,@(when keys
                  (mapcar (lambda (binding.command)
                            `(define-key ,keymap (kbd ,(car binding.command)) ,(cdr binding.command)))
                          keys)))))
     ,@(when (or minor-modes setup local-vars local-hooks)
         (let ((setup-fn-name (intern (format "cam/%s-setup" (symbol-name mode-name)))))
           `((defun ,setup-fn-name ()
               ,@(when minor-modes
                   (mapcar (lambda (minor-mode)
                             (if (atom minor-mode) `(,minor-mode 1)
                               (apply (cl-function (lambda (mode &key diminish)
                                                     `(progn
                                                        (,mode 1)
                                                        ,(when diminish
                                                           (if (eq diminish t) `(diminish ',mode)
                                                             `(diminish ',diminish))))
                                                     )) minor-mode)))
                           minor-modes))
               ,@setup
               ,@(when local-vars
                   (mapcar (lambda (var.value)
                             `(setq-local ,(car var.value) ,(cdr var.value)))
                           local-vars))
               ,@(when local-hooks
                   (mapcar (lambda (hook.fun)
                             `(add-hook ',(car hook.fun) ,(cdr hook.fun) ,(not :append) :local))
                           local-hooks)))
             (add-hook ',hook-name (function ,setup-fn-name)))))
     ,@(when auto-mode-alist
         (mapcar (lambda (pattern)
                   `(add-to-list 'auto-mode-alist '(,pattern . ,mode-name)))
                 auto-mode-alist))))


;; ---------------------------------------- [[<time]] ----------------------------------------

(defsubst cam/current-microseconds ()
  (let ((time (current-time)))
    (+ (* (nth 1 time) 1000000)
       (nth 2 time))))

;;;###autoload
(defmacro time (&rest body)
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
       (apply #'message ,(let* ((form-str (format "%s" body))
                                (form-str (if (> (length form-str) 50) (concat (substring form-str 0 50) "...")
                                            form-str)))
                           (concat "\n" form-str " elapsed time: %.1f %s."))
              (cond ((< ,elapsed 1000)    `(,,elapsed "µs"))
                    ((< ,elapsed 1000000) `(,(/ ,elapsed 1000.0) "ms"))
                    (:else                `(,(/ ,elapsed 1000000.0) "seconds"))))
       ,result)))

;;;###autoload
(defmacro time* (&rest forms)
  (declare (indent 0))
  `(progn ,@(mapcar (lambda (form)
                      `(time ,form))
                    forms)))

(provide 'cam-macros)
