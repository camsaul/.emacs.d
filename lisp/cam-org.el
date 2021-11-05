;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'cam-ox-pml)
(require 'column-enforce-mode)
(require 'company)
(require 'company-yasnippet)
(require 'flyspell)
(require 'org)
(require 'yasnippet)

(declare-function org-edit-src-code "org-src")

(setq org-support-shift-select nil)

(defun cam/org-insert-code-block (language)
  "Insert a new Org code block and start editing it."
  (interactive (list (if current-prefix-arg
                         (read-string "Language: ")
                       "emacs-lisp")))
  (org-return-indent)
  (insert "#+BEGIN_SRC " language)
  (org-return-indent)
  (insert "#+END_SRC")
  (forward-line -1)
  (org-end-of-line)
  (org-return-indent)
  (org-edit-src-code))

(cam/tweak-package org
  :minor-modes (column-enforce-mode
                company-mode
                flyspell-mode
                yas-minor-mode)
  :local-vars ((truncate-lines . nil))
  :keys (("C-c c" . #'cam/org-insert-code-block)
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<up>" . nil)
         ("S-<down>" . nil)
         ("<f9>" . #'cam/org-publish-to-pml)
         ;; ("<insert>" . #'company-yasnippet)
         ("<insert>" . #'yas-expand))
  ;; TODO -- doesn't really work
  ;; :setup ((setq-local company-backends (cl-concatenate 'list company-backends '(company-yasnippet))))
  :setup ((yas-reload-all))
  )

(cam/tweak-package org-src
  :declare (org-edit-src-code))

(provide 'cam-org)
