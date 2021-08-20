;;; cam-macros -- General Macros -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; [[cam/time]] ------------------------------------------------------------

(defsubst cam/current-microseconds ()
  "Return the current time in microseconds."
  (cl-destructuring-bind (_ seconds microseconds _) (current-time)
    (+ (* seconds 1000000) microseconds)))

;;;###autoload
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

;;;###autoload
(defmacro cam/time* (&rest forms)
  "Evalute each form in FORMS and echo a message about how long each took to execute."
  (declare (indent 0))
  `(progn ,@(cl-loop for form in forms
                     collect `(cam/time ,form))))

;;;###autoload
(defmacro cam/suppress-messages (&rest body)
  "Evaluate BODY with the `message' function temporarily bound to `ignore'."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'message) #'ignore))
     ,@body))

;;;###autoload
(defmacro cam/global-set-keys (&rest keys)
  (declare (indent 0))
  `(progn ,@(cl-loop for (key . command) in keys
                     collect `(global-set-key ,(kbd key) ,command))))

;;;###autoload
(cl-defun cam/visible-buffer-matching (pred &optional return-multiple-values?)
  "Return the first buffer visible in any window on any frame that satisfies PRED."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (let ((buffer (window-buffer window)))
        (when (funcall pred buffer)
          (cl-return-from cam/visible-buffer-matching (if return-multiple-values?
                                                          (list buffer window frame)
                                                        buffer)))))))

;;;###autoload
(cl-defmacro cam/when-let-visible-buffer (((binding &body pred-body) &rest more) &body body)
  "Iterate through all *visibile* buffers; for each buffer, bind it to BINDING, and bind its window and frame to
anaphors `this-window' and `this-frame', respectively; then evaluate PRED-BODY. If the result is truthy, evaluate
BODY. Example:

\(cam/when-let-visible-buffer ((buffer (string-equal (buffer-name buffer) \"*Warnings*\")))
  \(do-something buffer))"
  (declare (indent 1))
  `(cl-multiple-value-bind (,binding this-window this-frame) (cam/visible-buffer-matching (lambda (,binding)
                                                                                            ,@pred-body) :return-multiple-values)
     (when ,binding
       ,(if more
            `(cam/when-let-visible-buffer ,more ,@body)
          `(progn ,@body)))))

(provide 'cam-macros)
;;; cam-macros.el ends here
