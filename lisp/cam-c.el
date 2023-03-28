;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'cc-mode)

(require 'cam-todo-font-lock)
(require 'column-enforce-mode)
(require 'company)
;; (require 'company-lsp)
(require 'company-rtags)
(require 'eglot)
(require 'eldoc-box)
(require 'elec-pair)
(require 'flycheck)
(require 'flyspell)
(require 'lsp-clangd)
(require 'lsp-mode)
;; (require 'lsp-ui)
(require 'rainbow-delimiters)
(require 'rtags)

(cam/tweak-package cc-mode
  :mode-name c-mode
  :minor-modes (cam/todo-font-lock-mode
                column-enforce-mode
                company-mode
                eldoc-box-hover-at-point-mode
                eldoc-box-hover-mode
                electric-pair-local-mode
                flycheck-mode
                rainbow-delimiters-mode)
  :vars (;; (company-lsp-enable-snippet . t) ; nil
         (rtags-completions-enabled . t)
         ;; (rtags-display-result-backend . 'helm)
         )
  :local-vars ((tab-width . 4)
               (c-basic-indent . "k&r")
               (c-basic-offset . 4)
               (c-default-style . "linux")
               ;; FIXME -- doesn't work in `c-mode'
               ;; (comment-start . "// ")
               ;; (comment-end . "")
               )
  :keys (("C-j" . #'newline))
  :setup ((rtags-start-process-unless-running)
          (add-to-list 'eglot-server-programs '((c-mode) "clangd"))
          (eglot-ensure)
          (flyspell-prog-mode)
          ;; for some reason these backends are getting stomped on, so just add them again when we try to autocomplete
          ;; something if they're not already there.
          (cl-labels ((add-company-backends ()
                        (dolist (backend '(company-rtags ;; company-lsp
                                                         ))
                          (add-to-list 'company-backends backend))))
            (add-company-backends)
            (advice-add #'company-complete :before
              (lambda (&rest _args)
                (when (eq major-mode 'c-mode)
                  (add-company-backends))))))
  :keys (("<f1>" . #'eldoc-doc-buffer)
         ("M-." . #'rtags-find-symbol-at-point)))

;; (add-hook 'c-mode-common-hook #'cam/c-mode-setup)

(provide 'cam-c)
