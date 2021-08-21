(progn
  :autoload-end
  (defvar preview-markdown-mode nil "Non-nil if Preview-Markdown mode is enabled ...")
  (make-variable-buffer-local 'preview-markdown-mode)
  (defun preview-markdown-mode (&optional arg)
    "A minor mode that automatically renders previews of Markdown files upon save."
    (interactive (list (or current-prefix-arg 'toggle)))
    (let ((#:last-message (current-message)))
      (setq preview-markdown-mode
            (if (eq arg 'toggle)
                (not preview-markdown-mode)
              (> (prefix-numeric-value arg) 0)))
      (if preview-markdown-mode
          (add-hook 'after-save-hook #'preview-markdown nil t)
        (remove-hook 'after-save-hook #'preview-markdown t))
      (run-hooks 'preview-markdown-mode-hook
                 (if preview-markdown-mode 'preview-markdown-mode-on-hook 'preview-markdown-mode-off-hook))
      (when (called-interactively-p 'any)
        (unless (and (current-message)
                     (not (equal #:last-message (current-message))))
          (let ((local " in current buffer"))
            (message "Preview-Markdown mode %sabled%s"
                     (if preview-markdown-mode "en" "dis")
                     local)))))
    (force-mode-line-update)
    preview-markdown-mode)
  (defvar preview-markdown-mode-hook nil)
  (with-no-warnings
    (add-minor-mode 'preview-markdown-mode '" Prev-MD"
                    (if (boundp 'preview-markdown-mode-map)
                        preview-markdown-mode-map)
                    nil nil)))
