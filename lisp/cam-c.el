;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'cc-mode)

(require 'company)
(require 'elec-pair)
(require 'rainbow-delimiters)

(cam/tweak-package cc-mode
  :mode-name c-mode
  :minor-modes (company-mode
                electric-pair-local-mode
                rainbow-delimiters-mode)
  :local-vars ((tab-width . 4)
               (c-basic-indent . "k&r")
               (c-basic-offset . 4)
               (c-default-style . "linux"))
  :keys (("C-j" . #'newline)))

(provide 'cam-c)
