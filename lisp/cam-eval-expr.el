;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'simple)

(require 'company)
(require 'paredit)

(cam/tweak-package simple
  :hook-name eval-expression-minibuffer-setup-hook
  :minor-modes (company-mode
                paredit-mode)
  :local-vars ((company-echo-delay . 10)))

(provide 'cam-eval-expr)
