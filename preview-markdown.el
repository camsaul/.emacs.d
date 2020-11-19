;;; -*- lexical-binding: t; coding: utf-8; -*-

(defun preview-markdown--scroll-percentage ()
  (/ (float (line-number-at-pos (window-start)))
     (float (line-number-at-pos (point-max)))))

(defun preview-markdown--set-window-start-to-percentage (scroll-percentage)
  (goto-char (point-min))
  (let ((target-line-number (truncate (* (line-number-at-pos (point-max))
                                         scroll-percentage))))
    (forward-line (1- target-line-number)))
  (set-window-start nil (point)))

(defun preview-markdown--render-preview-current-buffer ()
  (message "Rendering Markdown preview of %s" buffer-file-name)
  (let ((url (concat "file://" buffer-file-name)))
    (shell-command-on-region (point-min) (point-max)
                             "pandoc -f gfm" "*Preview Markdown Output*")
    (switch-to-buffer-other-window "*Preview Markdown Output*")
    (let ((document (libxml-parse-html-region (point) (point-max))))
      (erase-buffer)
      (shr-insert-document `(base ((href . ,url)) ,document))
      (setq buffer-read-only t))))

(defun preview-markdown--preview (filename)
  (save-selected-window
    (find-file filename)
    (let ((scroll-percentage (preview-markdown--scroll-percentage)))
      (preview-markdown--render-preview-current-buffer)
      (preview-markdown--set-window-start-to-percentage scroll-percentage))))

;;;###autoload
(defun preview-markdown (&optional filename)
  "Render a markdown preview of FILENAME (by default, the current file) to HTML
and display it with `shr-insert-document'."
  (interactive "fFile: ")
  (if filename
      (progn
        (preview-markdown--preview filename)
        (switch-to-buffer (current-buffer)))
    (preview-markdown--preview buffer-file-name)))

(defvar-local preview-markdown-enable-automatic-previews t
  "Whether to enable automatic previews for Markdown files.")

;;;###autoload
(defun preview-markdown-if-automatic-previews-enabled ()
  "Render a Markdown preview for the current buffer, but only if
`preview-markdown-automatic-preview' is truthy."
  (when preview-markdown-enable-automatic-previews
    (preview-markdown)))

(provide 'preview-markdown)
