;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-macros)
  (require 'cam-tweak-package))

(require 'auto-complete)

(setq ac-auto-show-menu        0.1
      ac-candidate-menu-height 20
      ac-delay                 0.05
      ac-menu-height           20
      ac-quick-help-height     50
      ac-use-menu-map          t)

(cam/suppress-messages
  (ac-config-default)
  ;; disable automatically entering auto-complete-mode. We'll enter it manually if we want it.
  (setq ac-modes nil))

(cam/tweak-package auto-complete
  :keymap ac-menu-map
  :keys (("A-f"       . #'ac-complete-functions)
         ("A-s"       . #'ac-complete-symbols)
         ("A-v"       . #'ac-complete-variables)
         ("<S-tab>"   . #'auto-complete)
         ("<backtab>" . #'auto-complete)))

(provide 'cam-auto-complete)
