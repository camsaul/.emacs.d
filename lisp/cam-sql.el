;;; -*- lexical-binding: t; -*-

(require 'sql)

(defun cam/sql-mode-setup ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'sql-mode-hook #'cam/sql-mode-setup)

(provide 'cam-sql)
