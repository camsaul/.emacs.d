;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'sly)

(require 'cam-lisp)

(require 'company)

(defun cam/save-load-switch-to-sly ()
  (interactive)
  (save-buffer)
  (sly-compile-and-load-file)
  (sly-switch-to-most-recent 'sly-mrepl-mode))

(setq inferior-lisp-program "sbcl")

;; Tweak configuration for the Sly minor mode as opposed to lisp-mode because otherwise cam/tweak-package will
;; redefine cam/lisp-mode-setup

;;; [[<Sly]]
(cam/tweak-package sly
  ;; :require (ac-sly)
  :minor-modes (company-mode)
  :keys (("<C-M-return>" . #'cam/save-load-switch-to-sly))
  :setup ((cam/lisp-mode-setup)
          ;; don't load ac-sly until after sly is loaded, otherwise there will be circular requires between them
          ;; (eval-after-load 'sly
          ;;   '(progn
          ;;      (require 'ac-sly)
          ;;      (set-up-sly-ac :fuzzy)))
          ))

(provide 'cam-common-lisp)
