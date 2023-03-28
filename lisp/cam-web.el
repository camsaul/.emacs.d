;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'web-mode)
(require 'tide)

(require 'column-enforce-mode)
(require 'company)
;; (require 'company-lsp)
(require 'eldoc)
(require 'elec-pair)
(require 'flycheck)
(require 'flyspell)
(require 'rainbow-delimiters)

(defun cam/js-insert-console-dot-log (text)
  (interactive "sconsole.log: ")
  (if current-prefix-arg
      (insert "console.log(\"" text "\"); // NOCOMMIT")
    (insert "console.log(\"" text ":\", " text "); // NOCOMMIT")))

(cam/tweak-package web-mode
  :mode-name web-mode
  :minor-modes (column-enforce-mode
                company-mode
                electric-pair-local-mode
                rainbow-delimiters-mode)
  :keys (("C-j" . #'newline)
         ("<f10>" . #'cam/js-insert-console-dot-log))
  :setup
  ;; make sure autocomplete isn't enabled (we're using company instead)
  ((when (fboundp 'auto-complete-mode)
     (auto-complete-mode -1))
   ;; enable tide-mode for JS files
   (when (let ((ext (file-name-extension buffer-file-name)))
           (or (string= ext "js")
               (string= ext "jsx")))
     (require 'tide)
     (tide-setup))))

(cam/tweak-package tide
  :minor-modes (flycheck-mode
                eldoc-mode
                tide-hl-identifier-mode
                column-enforce-mode
                electric-pair-local-mode
                rainbow-delimiters-mode)
  :keys (("<f1>" . #'tide-documentation-at-point))
  :vars ((tide-always-show-documentation . t))
  :setup
  ((flyspell-prog-mode)
   (company-mode 1)))

(provide 'cam-web)
