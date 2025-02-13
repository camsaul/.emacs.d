;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(cam/tweak-package python
  :minor-modes (electric-pair-local-mode)
  :keys (("C-j" . #'newline)))

(provide 'cam-python)
