;;; -*- lexical-binding: t; coding: utf-8; -*-

(require 'markdown-mode)

(defvar-local preview-markdown-mode
  nil
  "Non-nil if Preview Markdown mode is enabled.")

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

(defun preview-markdown (&optional filename)
  "Render a markdown preview of FILENAME (by default, the current file) to HTML
and display it with `shr-insert-document'."
  (interactive "fFile: ")
  (if filename
      (progn
        (preview-markdown--preview filename)
        (switch-to-buffer (current-buffer)))
    (preview-markdown--preview buffer-file-name)))

(define-minor-mode preview-markdown-mode
  "A minor mode that automatically renders previews of Markdown files upon save."
  :lighter " Prev-MD"
  (if preview-markdown-mode
      (add-hook 'after-save-hook #'preview-markdown nil t)
    (remove-hook 'after-save-hook #'preview-markdown t)))

(provide 'preview-markdown)

(define-minor-mode preview-markdown-mode
  "A minor mode that automatically renders previews of Markdown files upon save."
  :lighter " Prev-MD"
  ...)
