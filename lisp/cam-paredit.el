;;; -*- lexical-binding: t; -*-

(require 'paredit)

(put #'paredit-forward-delete  'delete-selection 'supersede)
(put #'paredit-backward-delete 'delete-selection 'supersede)
(put #'paredit-open-round      'delete-selection t)
(put #'paredit-open-square     'delete-selection t)
(put #'paredit-doublequote     'delete-selection t)
(put #'paredit-newline         'delete-selection t)

(provide 'cam-paredit)
