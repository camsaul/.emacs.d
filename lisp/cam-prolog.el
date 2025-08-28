;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'sweeprolog)
(require 'company)

(defun cam/prolog-insert-message (text)
  (interactive "sprint: ")
  (if current-prefix-arg
      (insert "format('" text "~n'),")
    (insert "format('" text " = ~w~n', [" text "]),")))

(cam/tweak-package sweeprolog
  :minor-modes (company-mode
                electric-pair-local-mode
                sweeprolog-electric-layout-mode)
  :keys (("C-j" . #'newline)
         ("<f1>" . #'display-local-help)
         ("<f10>" . #'cam/prolog-insert-message)
         ("S-<return>" . #'sweeprolog-load-buffer)))

(cam/tweak-package sweeprolog
  :mode-name sweeprolog-top-level-mode
  :minor-modes (company-mode
                electric-pair-local-mode))

(provide 'cam-prolog)
