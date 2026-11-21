;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'magit)
(require 'subr-x)

(defun cam/magit-visit-pull-request-url ()
  "Visit the current git branch's PR on GitHub."
  (interactive)
  (browse-url (concat "http://github.com/"
                      ;; handle both SSH (git@github.com:owner/repo.git) and HTTPS remote URLs
                      (thread-last
                        (magit-get "remote" (magit-get-current-remote) "url")
                        (string-remove-suffix ".git")
                        (string-remove-prefix "git@github.com:")
                        (string-remove-prefix "https://github.com/"))
                      "/pull/"
                      (magit-get-current-branch))))

(defun cam/magit-shell-command ()
  "Replacement for `shell-command' in `magit-status-mode'.
Calls `magit-refresh' after the command finishes."
  (interactive)
  (call-interactively #'shell-command)
  (call-interactively #'magit-refresh))

(defun cam/refresh-magit-buffers ()
  "Refresh all `magit-status-mode' buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'magit-status-mode)
        (magit-refresh)))))

(defun cam/magit-buffer-p (buffer)
  (string-prefix-p "*magit" (buffer-name buffer)))

(defun cam/kill-all-magit-buffers-and-windows ()
  "Kill all magit-related buffers and windows."
  (interactive)
  ;; delete all the windows showing magit buffers
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (when (and (window-live-p window)
                 (cam/magit-buffer-p (window-buffer window)))
        ;; this fails if WINDOW is the only window in its frame. That's fine; it will show a different buffer once we
        ;; kill the magit one below.
        (ignore-errors
          (delete-window window)))))
  ;; ok, now kill all the magit buffers, visible or not
  (dolist (buffer (buffer-list))
    (when (cam/magit-buffer-p buffer)
      (kill-buffer buffer))))

;; Don't prompt to save buffers in the current repo before performing Magit actions
(setq magit-save-repository-buffers 'dontask)

(add-hook 'focus-in-hook #'cam/refresh-magit-buffers)

(cam/tweak-package magit
  :mode-name magit-status-mode
  :keys (("C-x 4 0" . #'cam/kill-all-magit-buffers-and-windows)
         ("M-!"     . #'cam/magit-shell-command)
         ("V"       . #'cam/magit-visit-pull-request-url)
         ("s-u"     . #'magit-refresh)))

(provide 'cam-magit)
