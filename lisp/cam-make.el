(eval-when-compile
  (require 'cam-tweak-package))

(require 'make-mode)

(cam/tweak-package make-mode
  :mode-name makefile-gmake-mode
  :local-vars ((indent-tabs-mode . t)))

(provide 'cam-make)
