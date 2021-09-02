;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'nxml-mode)

(require 'company)
(require 'elec-pair)

(declare-function cam/pml-mode "cam-pml-mode")

(cam/tweak-package nxml-mode
  :mode-name nxml-mode
  :minor-modes (electric-pair-mode
                company-mode)
  :keys (("C-j" . #'newline))
  :vars ((nxml-slash-auto-complete-flag . t))
  :setup ((when (and buffer-file-name
                     (string-equal (file-name-extension buffer-file-name) "pml"))
            (message "<Loading cam/pml-mode>")
            (require 'cam-pml-mode)
            (cam/pml-mode 1))))

(provide 'cam-nxml)
