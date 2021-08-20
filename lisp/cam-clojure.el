;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'cam-macros)
  (require 'clojure-mode)
  (require 'cider)
  (require 'cider-repl))

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

;;;###autoload
(defun cam/clj-switch-between-model-and-api-namespaces ()
  "(For Metabase development) if the current buffer is a /model/ namespace, switch to the corresponding /api/ namespace,
and vice versa."
  (interactive)
  (find-file
   (if (string-match-p "/models/" buffer-file-name)
       (replace-regexp-in-string "/models/" "/api/" buffer-file-name)
     (replace-regexp-in-string "/api/" "/models/" buffer-file-name))))

;;;###autoload
(defun cam/-clj-clear-nrepl-server-buffer ()
  "Clear contents of the `*nrepl-server*` buffer."
  (cam/when-let-visible-buffer ((buffer (string-prefix-p "*nrepl-server" (buffer-name buffer))))
    (with-current-buffer buffer
      (comint-clear-buffer))))

;;;###autoload
(defun cam/clj-save-load-switch-to-cider ()
  (interactive)
  (save-buffer)
  (cam/-clj-clear-nrepl-server-buffer)
  (cider-load-buffer-and-switch-to-repl-buffer :set-namespace)
  (cider-repl-clear-buffer))

(defvar cam/-clojure-load-buffer-clean-namespace--namespace-cleaned-p nil)

;;;###autoload
(cl-defun cam/clj-load-buffer-clean-namespace (&optional (buffer (current-buffer)))
  "When CIDER is active attempt to load BUFFER (by default, the current buffer) and clean its namespace declaration
form."
  (interactive "bBuffer: ")
  (when (not cam/-clojure-load-buffer-clean-namespace--namespace-cleaned-p)
    (with-demoted-errors "Error cleaning namespace declaration: %S"
      (with-current-buffer (get-buffer buffer)
        (let ((filename (file-name-nondirectory (buffer-file-name))))
          ;; don't run for `project.clj` or EDN files
          (when (and (not (string-equal filename "project.clj"))
                     (not (string-match-p "\.edn$" filename))
                     (cider-current-repl))
            ;; unfortunately it doesn't look like you can use `cider-load-buffer` programatically without saving the file
            ;; first, because when binding `cider-save-file-on-load` to `nil` it prompts asking whether you want to save
            (let ((cider-save-file-on-load t))
              (cider-load-buffer))
            (cljr-clean-ns)
            (when (buffer-modified-p)
              ;; prevent recursive calls !
              (let ((cam/-clojure-load-buffer-clean-namespace--namespace-cleaned-p t))
                (save-buffer)))))))))

;;;###autoload
(defun cam/clj-insert-println (text)
  (interactive "sprintln: ")
  (if current-prefix-arg
      (insert "(println \"" text "\") ; NOCOMMIT")
    (insert "(println \"" text ":\" " text ") ; NOCOMMIT")))

(defun cam/-insert-small-clojure-header (text)
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

(defun cam/-insert-large-clojure-header (text)
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
      (cam/-insert-small-clojure-header text)
    (cam/-insert-large-clojure-header text)))

;;;###autoload
(defun cam/clj-ansi-colorize-nrepl-output-buffer-if-needed (f process output)
  (let ((old-max (with-current-buffer (process-buffer process)
                   (point-max))))
    (funcall f process (replace-regexp-in-string "^.+ :: " "" output)) ; strip the logging prefix while we're at it
    (with-current-buffer (process-buffer process)
      (ansi-color-apply-on-region old-max (point-max)))))

(provide 'cam-clojure)
