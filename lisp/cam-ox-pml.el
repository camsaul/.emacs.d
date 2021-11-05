;;; -*- lexical-binding: t; -*-

(require 'ox)
(require 'ox-publish)
(require 'xml)

(defconst cam/ox-pml-known-snippet-tags
  '("filename"
    "firstuse"
    "initials"
    "keystroke"
    "method"
    "missing"
    "variable"
    "dt")
  "Allowed tags for #+tag: lines and @@tag:contents@@ forms.")

(defmacro cam/org-element-bind (bindings element &rest body)
  "This is basically just CL with-slots but for an `org-mode' element. See http://clhs.lisp.se/Body/m_w_slts.htm"
  (declare (indent 2))
  (let ((element-symb (make-symbol "element")))
    `(let* ((,element-symb ,element)
            ,@(mapcar (lambda (binding)
                        (cl-destructuring-bind (var key) (if (listp binding)
                                                             binding
                                                           (list binding binding))
                          `(,var (org-element-property ,(intern (concat ":" (symbol-name key)))
                                                       ,element-symb))))
                      bindings))
       ,@body)))

(defun cam/ox-pml--ensure-newline (contents)
  (if (or (null contents)
          (string-blank-p contents))
      ""
    (concat (string-trim-right contents) "\n")))

(defun cam/ox-pml--unhandled (type &rest args)
  (error "unhandled org type: %s" type)
  ;; (concat
  ;;  "\n"
  ;;  (prin1-to-string (cons type
  ;;                         (car args) ;; (butlast args)
  ;;                         )))
  )

(defun cam/ox-pml-bold (_bold contents _info)
  ;; (format "<emph>%s</emph>" contents)
  (#'cam/ox-pml--unhandled 'bold _bold contents _info))

(defun cam/ox-pml-center-block (&rest args)
  (apply #'cam/ox-pml--unhandled 'center-block args))

(defun cam/ox-pml-clock (&rest args)
  (apply #'cam/ox-pml--unhandled 'clock args))

;; inline code e.g. ~code~
(defun cam/ox-pml-code (code _contents _info)
  (cam/org-element-bind (value) code
    (format "<inlinecode><![CDATA[%s]]></inlinecode>" value)))

(defun cam/ox-pml-drawer (&rest args)
  (apply #'cam/ox-pml--unhandled 'drawer args))

;; TODO -- this is ridiculous
(defun cam/ox-pml--parse-arguments (arguments-string)
  (let ((start 0)
        (end (length arguments-string))
        output)
    (while (< start end)
      (cl-destructuring-bind (key . start*) (read-from-string arguments-string start end)
        (cl-destructuring-bind (val . start*) (read-from-string arguments-string start* end)
          (push (cons key val) output)
          (setq start start*))))
    output))

;; e.g.
;;
;; #+BEGIN: image fileref "images/describe_face.png" width "75%"
;; /M-x describe-face/
;; #+END:
;;
;; becomes
;;
;; <figure>
;; <imagedata width="75%" fileref="images/describe_face.png" />
;; <p>
;; <emph>M-x describe-face</emph>
;; </p>
;; </figure>
(defun cam/ox-pml-dynamic-block (dynamic-block content info)
  (cam/org-element-bind (begin block-name arguments) dynamic-block
    (cond
     ((string-equal block-name "image")
      (let* ((arguments (cam/ox-pml--parse-arguments arguments))
             (_ (unless (assoc 'fileref arguments)
                  (error "image on line %d missing fileref attribute" (line-number-at-pos begin t))))
             (properties (mapconcat (lambda (pair)
                                      (cl-destructuring-bind (key . value) pair
                                        (format "%s=\"%s\""
                                                (symbol-name key)
                                                value)))
                                    arguments
                                    " ")))
        (concat "<figure>\n"
                (format "<imagedata %s />\n" properties)
                (when (and (stringp content)
                           (not (string-blank-p content)))
                  (cam/ox-pml--ensure-newline content))
                "</figure>\n")))

     (t
      (cam/ox-pml--unhandled dynamic-block content info)))))

(defun cam/ox-pml-entity (&rest args)
  (apply #'cam/ox-pml--unhandled 'entity args))

(defun cam/ox-pml-example-block (&rest args)
  (apply #'cam/ox-pml--unhandled 'example-block args))

;; to insert XML tags literally:
;;
;; #+BEGIN_EXPORT cam-pml
;; ...
;; #+END_EXPORT
(defun cam/ox-pml-export-block (export-block contents info)
  (cam/org-element-bind (type value) export-block
    (cond
     ((string-equal type "CAM-PML")
      value)

     (t
      (cam/ox-pml--unhandled 'export-block export-block contents info)))))

(defun cam/ox-pml-export-snippet (export-snippet _content _info)
  (cam/org-element-bind (back-end value) export-snippet
    (unless (member back-end cam/ox-pml-known-snippet-tags)
      (error "Unknown tag '%s' in @@ expression" back-end))
    (format "<%s><![CDATA[%s]]></%s>" back-end value back-end)))

(defun cam/ox-pml-fixed-width (&rest args)
  (apply #'cam/ox-pml--unhandled 'fixed-width args))

(defun cam/ox-pml-footnote-reference (footnote-reference _contents info)
  (message "(org-element-property footnote-reference :type): %s" (org-element-property :type footnote-reference))
  (when (equal (org-element-property :type footnote-reference) 'standard)
    (let ((footnote (org-export-get-footnote-definition footnote-reference info)))
      (concat "<footnote><p>"
              (org-export-data footnote info)
              "</p></footnote>"))))

(defun cam/ox-pml-headline (headline contents info)
  (cam/org-element-bind (title level begin) headline
    (let ((title (org-export-data title info)))
      ;; make sure we're not doing dumb stuff like putting <sect4> in a <sect2>
      (unless (= level 1)
        (let ((parent (org-export-get-parent-element headline)))

          (cam/org-element-bind ((parent-level level) (parent-title title)) parent
            (unless (= level (1+ parent-level))
              (error "Invalid nesting: sect%d '%s' on line %d cannot be a child of sect%d '%s'\n"
                     level title (line-number-at-pos begin t)
                     parent-level (org-export-data parent-title info))))))
      (concat
       (format "<sect%d>\n" level)
       (format "<title>%s</title>\n" title)
       (cam/ox-pml--ensure-newline contents)
       (format "</sect%d>\n" level)
       (format "<!-- End of sect%d %s -->\n" level title)))))

(defun cam/ox-pml-horizontal-rule (&rest args)
  (apply #'cam/ox-pml--unhandled 'horizontal-rule args))

(defun cam/ox-pml-inline-src-block (&rest args)
  (apply #'cam/ox-pml--unhandled 'inline-src-block args))

(defun cam/ox-pml-inlinetask (&rest args)
  (apply #'cam/ox-pml--unhandled 'inlinetask args))

(defun cam/ox-pml-inner-template (contents _info)
  contents)

(defun cam/ox-pml-italic (_italic contents _info)
  (format "<emph>%s</emph>" contents))

;; list item
(defun cam/ox-pml-item (_list-item contents _info)
  (concat
   "<li>\n"
   (cam/ox-pml--ensure-newline contents)
   "</li>\n"))

(defconst cam/ox-pml-tag-keywords
  '("TITLE" "AUTHOR" "MISSING" "ED")
  "Org keywords to insert directly into the PML output as XML elements")

(defun cam/ox-pml-keyword (keyword _contents info)
  (cam/org-element-bind (key value) keyword
    (cond
     ((member key cam/ox-pml-tag-keywords)
      (let ((tag (downcase key)))
        (format "<%s>%s</%s>" tag (cam/ox-pml-plain-text value info) tag)))

     ((string-equal key "DT")
      (format "<dt newline=\"yes\">%s</dt>\n" (cam/ox-pml-plain-text value info)))

     (t
      (cam/ox-pml--unhandled 'keyword keyword _contents info)))))

(defun cam/ox-pml-latex-environment (&rest args)
  (apply #'cam/ox-pml--unhandled 'latex-environment args))

(defun cam/ox-pml-latex-fragment (&rest args)
  (apply #'cam/ox-pml--unhandled 'latex-fragment args))

(defun cam/ox-pml-line-break (&rest args)
  (apply #'cam/ox-pml--unhandled 'line-break args))

(defun cam/ox-pml-link (link content _info)
  (cam/org-element-bind (raw-link type path) link
    ;; HTTPS URL
    (format "<url>%s</url>" raw-link)))

(defun cam/ox-pml-node-property (&rest args)
  (apply #'cam/ox-pml--unhandled 'node-property args))

(defconst cam/ox-pml--do-not-wrap-in-paragraph-elements
  '("<figure>")
  "Elements to never directly wrap in <p> tags.")

(defun cam/ox-pml-paragraph (_paragraph contents _info)
  (let ((do-not-wrap-p (and (stringp contents)
                            (cl-member (string-trim-left contents)
                                       cam/ox-pml--do-not-wrap-in-paragraph-elements
                                       :test (lambda (contents prefix)
                                               (string-prefix-p prefix contents))))))
    (unless (string-blank-p contents)
      (if do-not-wrap-p
          contents
        (format "<p>\n%s</p>\n" (cam/ox-pml--ensure-newline contents))))))

(defun cam/ox-pml-plain-list (plain-list contents _info)
  (cam/org-element-bind (type) plain-list
    (cond
     ((eq type 'unordered)
      (format "<ul>\n%s</ul>\n" (cam/ox-pml--ensure-newline contents)))
     ((eq type 'ordered)
      (format "<ol>\n%s</ol>\n" (cam/ox-pml--ensure-newline contents)))
     (t
      (cam/ox-pml--unhandled 'plain-list plain-list contents _info)))))

(defun cam/ox-pml-plain-text (text _info)
  (string-replace " -- " " --- " (xml-escape-string text)))

(defun cam/ox-pml-planning (&rest args)
  (apply #'cam/ox-pml--unhandled 'planning args))

(defun cam/ox-pml-property-drawer (&rest args)
  (apply #'cam/ox-pml--unhandled 'property-drawer args))

(defun cam/ox-pml-quote-block (_quote-block contents _info)
  (concat "<blockquote>\n"
          (cam/ox-pml--ensure-newline contents)
          "</blockquote>\n"))

(defun cam/ox-pml-radio-target (&rest args)
  (apply #'cam/ox-pml--unhandled 'radio-target args))

(defun cam/ox-pml-section (_section contents _info)
  contents)

(defun cam/ox-pml-special-block (special-block content info)
  (let ((tag (org-element-property :type special-block)))
    (concat (format "<%s>\n" tag)
            (cam/ox-pml--ensure-newline content)
            (format "</%s>\n" tag))))

(defun cam/ox-pml-src-block (src-block _contents _info)
  (let* ((lang (org-element-property :language src-block))
         (code-info (org-export-unravel-code src-block))
         (code (car code-info))
         ;; (refs (cdr code-info))
         )
    (concat
     (if lang
         (format "<code language=\"%s\">\n" lang)
       "<code>\n")
     "<![CDATA[\n"
     (cam/ox-pml--ensure-newline code)
     "]]>\n"
     "</code>\n")))

(defun cam/ox-pml-statistics-cookie (&rest args)
  (apply #'cam/ox-pml--unhandled 'statistics-cookie args))

(defun cam/ox-pml-strike-through (&rest args)
  (apply #'cam/ox-pml--unhandled 'strike-through args))

(defun cam/ox-pml-subscript (&rest args)
  (apply #'cam/ox-pml--unhandled 'subscript args))

(defun cam/ox-pml-superscript (&rest args)
  (apply #'cam/ox-pml--unhandled 'superscript args))

(defun cam/ox-pml-table (&rest args)
  (apply #'cam/ox-pml--unhandled 'table args))

(defun cam/ox-pml-table-cell (&rest args)
  (apply #'cam/ox-pml--unhandled 'table-cell args))

(defun cam/ox-pml-table-row (&rest args)
  (apply #'cam/ox-pml--unhandled 'table-row args))

(defun cam/ox-pml-target (&rest args)
  (apply #'cam/ox-pml--unhandled 'target args))

(defun cam/ox-pml-template (contents _info)
  (concat
   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
   "<!DOCTYPE chapter SYSTEM \"local/xml/markup.dtd\">\n"
   "<chapter>\n"
   contents
   "</chapter>\n"))

(defun cam/ox-pml-timestamp (&rest args)
  (apply #'cam/ox-pml--unhandled 'timestamp args))

(defun cam/ox-pml-underline (&rest args)
  (apply #'cam/ox-pml--unhandled 'underline args))

(defun cam/ox-pml-verbatim (verbatim _contents _info)
  (cam/org-element-bind (value) verbatim
    value))

(defun cam/ox-pml-verse-block (&rest args)
  (apply #'cam/ox-pml--unhandled 'verse-block args))

(defconst cam/ox-pml-translate-alist
  '((bold . cam/ox-pml-bold)
    (center-block . cam/ox-pml-center-block)
    (clock . cam/ox-pml-clock)
    (code . cam/ox-pml-code)
    (drawer . cam/ox-pml-drawer)
    (dynamic-block . cam/ox-pml-dynamic-block)
    (entity . cam/ox-pml-entity)
    (example-block . cam/ox-pml-example-block)
    (export-block . cam/ox-pml-export-block)
    (export-snippet . cam/ox-pml-export-snippet)
    (fixed-width . cam/ox-pml-fixed-width)
    (footnote-reference . cam/ox-pml-footnote-reference)
    (headline . cam/ox-pml-headline)
    (horizontal-rule . cam/ox-pml-horizontal-rule)
    (inline-src-block . cam/ox-pml-inline-src-block)
    (inlinetask . cam/ox-pml-inlinetask)
    (inner-template . cam/ox-pml-inner-template)
    (italic . cam/ox-pml-italic)
    (item . cam/ox-pml-item)
    (keyword . cam/ox-pml-keyword)
    (latex-environment . cam/ox-pml-latex-environment)
    (latex-fragment . cam/ox-pml-latex-fragment)
    (line-break . cam/ox-pml-line-break)
    (link . cam/ox-pml-link)
    (node-property . cam/ox-pml-node-property)
    (paragraph . cam/ox-pml-paragraph)
    (plain-list . cam/ox-pml-plain-list)
    (plain-text . cam/ox-pml-plain-text)
    (planning . cam/ox-pml-planning)
    (property-drawer . cam/ox-pml-property-drawer)
    (quote-block . cam/ox-pml-quote-block)
    (radio-target . cam/ox-pml-radio-target)
    (section . cam/ox-pml-section)
    (special-block . cam/ox-pml-special-block)
    (src-block . cam/ox-pml-src-block)
    (statistics-cookie . cam/ox-pml-statistics-cookie)
    (strike-through . cam/ox-pml-strike-through)
    (subscript . cam/ox-pml-subscript)
    (superscript . cam/ox-pml-superscript)
    (table . cam/ox-pml-table)
    (table-cell . cam/ox-pml-table-cell)
    (table-row . cam/ox-pml-table-row)
    (target . cam/ox-pml-target)
    (template . cam/ox-pml-template)
    (timestamp . cam/ox-pml-timestamp)
    (underline . cam/ox-pml-underline)
    (verbatim . cam/ox-pml-verbatim)
    (verse-block . cam/ox-pml-verse-block)))

(org-export-define-backend 'cam-pml
  cam/ox-pml-translate-alist)

(defun cam/-compile-pml-file (pml-file-name)
  (let ((pdf-file-name (replace-regexp-in-string "pml$" "pdf" pml-file-name))
        ;; run shell commands in context of buffer directory
        (default-directory (file-name-directory (buffer-file-name))))
    (delete-file pdf-file-name)
    (message "Compiling %s -> %s" pml-file-name pdf-file-name)
    ;; (let ((default-directory "/home/cam/book/Book/"))
    ;;   (shell-command "rake Ch_SudokuMode.2.pdf")
    ;;   (shell-command "wslview Ch_SudokuMode.2.pdf"))
    ;; this currently only works with Java 8.
    (shell-command (format "rake %s" pdf-file-name) "*Rake Output*" "*Rake Output*")
    (if (file-exists-p pdf-file-name)
        (progn
          (message "Opening %s..." pdf-file-name)
          (shell-command (format "wslview %s" pdf-file-name)))
      (display-buffer "*Rake Output*"))))

(defun cam/org-publish-to-pml ()
  (interactive)
  (let ((pml-file-name (org-export-output-file-name ".pml")))
    (message "Compiling org file to %s..." pml-file-name)
    (delete-file pml-file-name)
    (org-export-to-file 'cam-pml pml-file-name)
    ;; with prefix arg do not compile to PDF
    (when (file-exists-p pml-file-name)
      (if current-prefix-arg
          (let ((pml-buffer (find-file-noselect pml-file-name)))
            (display-buffer pml-buffer))
        (cam/-compile-pml-file pml-file-name)))))

(add-to-list 'org-export-backends 'cam-pml)

(font-lock-add-keywords
 'org-mode
 '(("@@initials:\\([^@]+\\)@@" (0 'font-lock-comment-face) (1 'bold t))
   ("@@firstuse:\\([^@]+\\)@@" (0 'font-lock-comment-face) (1 'italic t))
   ("@@keystroke:\\([^@]+\\)@@" (0 'font-lock-comment-face) (1 'font-lock-string-face t))
   ("@@variable:\\([^@]+\\)@@" (0 'font-lock-comment-face) (1 'font-lock-variable-name-face t))
   ("@@method:\\([^@]+\\)@@" (0 'font-lock-comment-face) (1 'font-lock-function-name-face t))
   ("\\(@@\\)\\([^@:]+\\):\\([^@:]+\\)\\(@@\\)"
    (1 'font-lock-comment-face)
    (2 'org-code)
    (3 'font-lock-string-face)
    (4 'font-lock-comment-face))
   ("#\\+missing:" 0 'font-lock-warning-face t)
   ;; ("#\\+missing:.+$" 0 'font-lock-warning-face prepend)
   ;; ("#\\+missing:" 0 'font-lock-warning-face prepend)
   ;; ("\\(#\\+missing:\\)\\([^\n]+\\)" (0 'font-lock-warning-face t) (2 'italic t))
   ;; ("[[:blank:]]/[^/]+/[[:blank:]]" . 'italic)
   ))

;; (font-lock-remove-keywords nil '("#\\+missing:" . 'font-lock-error-face))

;; font-lock-fontify-buffer
;; font-lock-update
;; font-lock-debug-fontify

;; (font-lock-remove-keywords nil '(nil (0 nil)))
;; (font-lock-remove-keywords nil '(nil))

;; (font-lock-remove-keywords nil '(("@@" . 'org-code)))

(provide 'cam-ox-pml)
