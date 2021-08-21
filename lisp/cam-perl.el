;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'cperl-mode)

(require 'eldoc)
(require 'elec-pair)

(setq cperl-electric-keywords t
      cperl-indent-level      4)

(cam/tweak-package cperl-mode
  :mode-name cperl-mode
  :minor-modes (eldoc-mode
                electric-pair-local-mode
                ;; perl-completion-mode
                )
  :keys (("C-c C-d" . #'cperl-perldoc))
  :local-vars ((eldoc-documentation-function . (lambda ()
                                                 (car
                                                  (let (cperl-message-on-help-error)
                                                    (cperl-get-help))))))
  ;; :setup ((add-to-list 'ac-sources 'ac-source-perl-completion))
  :auto-mode-alist ("\.pl$"
                    "\.pm$"))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(provide 'cam-perl)
