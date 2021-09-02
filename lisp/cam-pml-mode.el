;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'column-enforce-mode)
(require 'nxml-mode)

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

(defun cam/pml--cdata (content)
  (format "<![CDATA[%s]]>" content))

(defun cam/pml--insert-tag (tag content)
  (insert (format "<%s>%s</%s>" tag content tag)))

(defun cam/pml--insert-multiline-tag (tag)
  (cam/pml--insert-tag tag "\n\n")
  (forward-line -1))

(defun cam/pml--insert-tag-cdata (tag content)
  (cam/pml--insert-tag tag (cam/pml--cdata content)))

(defun cam/pml-insert-method (method-name)
  (interactive "sMethod: ")
  (cam/pml--insert-tag-cdata "method" method-name))

(defun cam/pml-insert-variable (variable-name)
  (interactive "sVariable: ")
  (cam/pml--insert-tag-cdata "variable" variable-name))

(defun cam/pml-insert-filename (file-name)
  (interactive "sFilename: ")
  (cam/pml--insert-tag-cdata "filename" file-name))

(defun cam/pml-insert-section (section-number title)
  (interactive (list (string-to-number (read-answer "section type # " '(("1" ?1 "insert sect1")
                                                                        ("2" ?2 "insert sect2")
                                                                        ("3" ?3 "insert sect3")
                                                                        ("4" ?4 "insert sect4"))))
                     (let ((title))
                       (while (or (null title)
                                  (string-empty-p title))
                         (setq title (read-string "Title: ")))
                       title)))
  (let ((tag (format "sect%d" section-number)))
    (message "Insert <%s> %s" tag title)
    (insert (format "<%s>\n" tag))
    (insert "<title>\n")
    (insert title "\n")
    (insert "</title>\n")
    (insert "\n")
    (insert "\n")
    (insert (format  "</%s>\n" tag))
    (insert (format  "<!-- End of %s %s -->\n" tag title))
    (forward-line -3)))

(defun cam/pml-insert-paragraph ()
  (interactive)
  (cam/pml--insert-multiline-tag "p"))

;; (unintern 'cam/pml-mode-map nil)

(defconst cam/pml--tags-to-wrap-in-cdata
  '("keystroke"))

(defconst cam/pml--multiline-tags
  '("li"
    "ol"
    "p"
    "ul"))

(defconst cam/pml--child-tags-alist
  '(("li" . "p")
    ("ol" . "li")
    ("ul" . "li")))

(defun cam/pml--insert-tag-maybe-cdata (tag content)
  (let* ((multiline-p (member tag cam/pml--multiline-tags))
         (child-tag (assoc tag cam/pml--child-tags-alist))
         (content* (unless child-tag
                     content))
         (content* (if multiline-p
                       (concat "\n" content* "\n")
                     content*)))
    (if (member tag cam/pml--tags-to-wrap-in-cdata)
        (cam/pml--insert-tag-cdata tag content*)
      (cam/pml--insert-tag tag content*))
    (when multiline-p
      (forward-line -1))
    (when child-tag
      (cam/pml--insert-tag-maybe-cdata (cdr child-tag) content))))

(defun cam/pml--read-tag-content (tag)
  (or (read-string (format "<%s> contents: " tag))
      (cam/pml--read-tag-content tag)))

(defun cam/pml-insert-anything (tag content)
  (interactive (let ((tag (read-answer "insert "
                                       '(("author"    ?a "insert <author>")
                                         ("emph"      ?e "insert <emph>")
                                         ("firstuse"  ?F "insert <firstuse>")
                                         ("footnote"  ?f "insert <footnote>")
                                         ("initials"  ?i "insert <initials>")
                                         ("li"        ?l "insert <li> (list item)")
                                         ("keystroke" ?k "insert <keystroke>")
                                         ("missing"   ?m "insert <missing>")
                                         ("ol"        ?o "insert <ol> (ordered list)")
                                         ("ul"        ?u "insert <ul> (unordered list)")))))
                 (list tag (cam/pml--read-tag-content tag))))
  (cam/pml--insert-tag-maybe-cdata tag content))

;;;###autoload
(define-minor-mode cam/pml-mode
  "Cam's minor mode for editing PML book source files"
  :lighter " cam/pml"
  :keymap `((,(kbd "<f9> <f9>") . cam/pml-insert-anything)
            (,(kbd "<f9> c")    . cam/pml-insert-code-block)
            (,(kbd "<f9> f")    . cam/pml-insert-filename)
            (,(kbd "<f9> i")    . cam/pml-insert-inline-code)
            (,(kbd "<f9> m")    . cam/pml-insert-method)
            (,(kbd "<f9> p")    . cam/pml-insert-paragraph)
            (,(kbd "<f9> s")    . cam/pml-insert-section)
            (,(kbd "<f9> v")    . cam/pml-insert-variable))
  ;; body
  (when cam/pml-mode
    (column-enforce-mode 1))
  (setq-local column-enforce-column (if cam/pml-mode 80 120))
  (setq-local fill-column (if cam/pml-mode 80 120))
  (electric-indent-mode (if cam/pml-mode -1 1)))

;; (message "(provide 'cam-pml-mode)")

(provide 'cam-pml-mode)
