;;; cam-macros -- General Macros -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(message "LOADED CAM-MACROS!")

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


;;; [[cam/suppress-messages]] ----------------------------------------------------------------------

;;;###autoload
(defmacro cam/suppress-messages (&rest body)
  "Evaluate BODY with the `message' function temporarily bound to `ignore'."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'message) #'ignore))
     ,@body))


;;; [[cam/global-set-keys]] ----------------------------------------------------------------------

(defmacro cam/global-set-keys (&rest keys)
  (declare (indent 0))
  `(progn ,@(cl-loop for (key . command) in keys
                     collect `(global-set-key ,(kbd key) ,command))))

(provide 'cam-macros)
;;; cam-macros.el ends here
