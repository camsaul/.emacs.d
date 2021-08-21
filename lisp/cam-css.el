;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'css-mode)
(require 'elec-pair)

(cam/tweak-package css-mode
  :mode-name css-mode
  :minor-modes (electric-pair-local-mode))

(provide 'cam-css)
