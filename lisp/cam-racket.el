;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'racket-mode)
(require 'cam-lisp)

(require 'company)
(require 'eldoc)

(cam/tweak-package racket-mode
  :mode-name racket-mode
  :minor-modes (company-mode
                eldoc-mode)
  :setup ((cam/lisp-mode-setup)
          (when (fboundp 'auto-complete-mode)
            (auto-complete-mode -1))
          (eldoc-mode 1))
  :local-vars ((eldoc-documentation-function . #'racket-repl-eldoc-function))
  :keys (("<f1>" . #'racket-repl-describe)))

(cam/tweak-package racket-repl
  :setup ((cam/racket-mode-setup)
          (eldoc-mode 1))
  :local-vars ((eldoc-documentation-function . #'racket-repl-eldoc-function))
  :keys (("<f1>" . #'racket-repl-describe)))

(provide 'cam-racket)
