;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-macros)
  (require 'cam-tweak-package))

(require 'simple)

(defun cam/clear-messages-buffer ()
  (interactive)
  (cam/when-let-visible-buffer ((buffer (string-equal "*Messages*" (buffer-name buffer))))
    (with-current-buffer buffer
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (read-only-mode 1))))

(cam/tweak-package simple
  :mode-name messages-buffer-mode
  :keys (("c" . #'cam/clear-messages-buffer)))

(provide 'cam-messages-mode)
