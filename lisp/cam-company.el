;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'company)

(setq company-idle-delay            0.01
      company-minimum-prefix-length 1)

(cam/tweak-package company
  :keys (("<S-tab>"   . #'company-complete)
         ("<backtab>" . #'company-complete)))

(provide 'cam-company)
