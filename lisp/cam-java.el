;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'cc-mode)

(require 'cam-c)

(cam/tweak-package cc-mode
  :mode-name java-mode
  :setup ((cam/c-mode-setup))
  :keys (("C-j" . #'newline)))

(provide 'cam-java)
