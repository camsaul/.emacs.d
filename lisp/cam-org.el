;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'org)

(require 'flyspell)

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
  :minor-modes (flyspell-mode)
  :local-vars ((truncate-lines . nil))
  :keys (("C-c c" . #'cam/org-insert-code-block)))

(cam/tweak-package org-src
  :declare (org-edit-src-code))

(provide 'cam-org)
