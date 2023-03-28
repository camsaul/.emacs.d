;;; -*- lexical-binding: t; -*-

(require 'paredit)

(put #'paredit-forward-delete  'delete-selection 'supersede)
(put #'paredit-backward-delete 'delete-selection 'supersede)
(put #'paredit-open-round      'delete-selection t)
(put #'paredit-open-square     'delete-selection t)
(put #'paredit-doublequote     'delete-selection t)
(put #'paredit-newline         'delete-selection t)

;; disable `kill-sentence' and `backward-kill-sentence' in paredit mode because they're easy to accidentally type and
;; don't preserve sexp structure
(advice-add #'kill-sentence :before
  (lambda (&optional _arg)
    "Disable `kill-sentence' in `paredit-mode' because it doesn't preserve sexp structure."
    (when paredit-mode
      (error "kill-sentence is disabled in paredit-mode"))))

(advice-add #'backward-kill-sentence :before
  (lambda (&optional _arg)
    "Disable backward-`kill-sentence' in `paredit-mode' because it doesn't preserve sexp structure."
    (when paredit-mode
      (error "backward-kill-sentence is disabled in paredit-mode"))))

(cam/tweak-package paredit
  ;; remove the wacky RET binding recently added to paredit.
  :keys (("RET")
         ("C-j")))

(provide 'cam-paredit)
