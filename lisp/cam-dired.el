;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package)
  (require 'subr-x))

(require 'dired)
(require 'dired-x)

(defun cam/after-dired-find-file ()
  "After-advice for `dired-find-file'. Kill all `dired' buffers
unless they are the current buffer."
  (dolist (buf (buffer-list))
    (unless (eq buf (current-buffer))
      (with-current-buffer buf
        (when (eq major-mode 'dired-mode)
          (kill-buffer buf))))))

(defun cam/revert-dired-buffers ()
  "Revert all `dired' buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'dired-mode)
        (revert-buffer)))))

(defun cam/around-dired-do-delete (fun &optional arg)
  "Around-advice for `dired-do-delete'. When deleting a file, check if its a directory; if so, and the directory is
deleted, ask to kill any buffers that were visiting files that were children of that directory."
  ;; point isn't necessarily on a file, e.g. when deleting marked files.
  (let* ((file (dired-get-filename nil :no-error-if-not-filep))
         (deleting-directory-p (and file (file-directory-p file))))
    (let ((result (funcall fun arg)))
      (when (and deleting-directory-p
                 (not (file-exists-p file))) ; check that file was actually deleted
        ;; make sure the name ends in a slash so deleting /a/foo doesn't match files in /a/foobar/
        (let ((directory (file-name-as-directory (expand-file-name file))))
          (dolist (buf (buffer-list))
            (when-let* ((buffer-file (buffer-file-name buf)))
              (when (string-prefix-p directory (expand-file-name buffer-file))
                (kill-buffer-ask buf))))))
      result)))

(setq dired-recursive-copies  'always
      dired-recursive-deletes 'always)

(advice-add #'dired-do-delete :around #'cam/around-dired-do-delete)

(advice-add #'dired-find-file :after #'cam/after-dired-find-file)

(advice-add #'dired-smart-shell-command :after
  (lambda (&rest _)
    (revert-buffer)))

(add-hook 'focus-in-hook #'cam/revert-dired-buffers)

(cam/tweak-package dired :minor-modes (dired-hide-details-mode))

(provide 'cam-dired)
