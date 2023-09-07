;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'company)
;; disabled for now because this is busted on macOS
;; (require 'company-posframe)
(require 'company-quickhelp)

(setq company-idle-delay            0.2
      company-minimum-prefix-length 2)

(cam/tweak-package company
  :minor-modes (;; company-posframe-mode
                company-quickhelp-mode)
  :local-vars ((company-quickhelp-delay . 0.1))
  :keys (("<S-tab>"   . #'company-complete)
         ("<backtab>" . #'company-complete)))

(provide 'cam-company)
