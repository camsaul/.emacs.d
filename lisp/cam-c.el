;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'cc-mode)

(require 'auto-complete)
(require 'elec-pair)

(cam/tweak-package cc-mode
  :mode-name c-mode
  :minor-modes (auto-complete-mode
                electric-pair-local-mode)
  :local-vars ((tab-width . 4)
               (c-basic-indent . "k&r")
               (c-basic-offset . 4)
               (c-default-style . "linux"))
  :keys (("C-j" . #'newline)))

(provide 'cam-c)
