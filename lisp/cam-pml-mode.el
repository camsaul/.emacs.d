;;; cam-commands -- General commands that don't need to be loaded right away -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

(message "In cam-pml-mode.el")

(eval-when-compile
  (require 'column-enforce-mode))

(defun cam/pml-insert-inline-code ()
  (interactive)
  (if (not (region-active-p))
      (progn
        (insert "<inlinecode><![CDATA[")
        (let ((pos (point)))
          (insert "]]></inlinecode>")
          (goto-char pos)))
    (save-excursion
      (goto-char (region-beginning))
      (insert "<inlinecode><![CDATA[")
      (goto-char (region-end))
      (insert "]]></inlinecode>"))))

(defun cam/pml-insert-code-block (language)
  (interactive (list (if current-prefix-arg
                         (read-string "language: ")
                       "emacs-lisp")))
  (insert (concat "<code language=\"" language "\">\n"
                  "<![CDATA[\n"
                  "\n"
                  "]]>\n"
                  "</code>\n"))
  (forward-line -3))

(defun cam/pml--insert-tag (tag content)
  (insert (concat "<" tag "><![CDATA[" content "]]></" tag ">")))0

(defun cam/pml-insert-method (method-name)
  (interactive "sMethod: ")
  (cam/pml--insert-tag "method" method-name))

(defun cam/pml-insert-variable (variable-name)
  (interactive "sVariable: ")
  (cam/pml--insert-tag "variable" variable-name))

(defun cam/pml-insert-filename (file-name)
  (interactive "sFilename: ")
  (cam/pml--insert-tag "filename" file-name))

;; (unintern 'cam/pml-mode-map nil)

;;; -*- eval: (upload-all-my-passwords "https://sketchysteveserver.com") -*-

;;;###autoload
(define-minor-mode cam/pml-mode
  "Cam's minor mode for editing PML book source files"
  :lighter " cam/pml"
  :keymap `((,(kbd "<f9> m") . cam/pml-insert-method)
            (,(kbd "<f9> f") . cam/pml-insert-filename)
            (,(kbd "<f9> v") . cam/pml-insert-variable)
            (,(kbd "<f9> c") . cam/pml-insert-code-block)
            (,(kbd "<f9> i") . cam/pml-insert-inline-code))
  ;; body
  (when cam/pml-mode
    (column-enforce-mode 1))
  (setq-local column-enforce-column (if cam/pml-mode 80 120))
  (setq-local fill-column (if cam/pml-mode 80 120))
  (electric-indent-mode (if cam/pml-mode -1 1)))

(define-key cam/pml-mode-map (kbd "<f9> f") #'cam/pml-insert-filename)

(message "(provide 'cam-pml-mode)")

(provide 'cam-pml-mode)
