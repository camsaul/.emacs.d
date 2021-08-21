;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package))

(require 'magit)
(require 'subr-x)

(defun cam/magit-visit-pull-request-url ()
  "Visit the current git branch's PR on GitHub."
  (interactive)
  (browse-url (concat "http://github.com/"
                      (->> (magit-get "remote" (magit-get-current-remote) "url")
                           (string-remove-suffix ".git")
                           (string-remove-prefix "git@github.com:"))
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
  ;; kill all the visible magit buffers & their windows
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (let ((buffer (window-buffer window)))
        (when (cam/magit-buffer-p buffer)
          (with-current-buffer buffer
            (kill-buffer-and-window))))))
  ;; ok, now kill any magit buffers in the background
  (dolist (buffer (buffer-list))
    (when (cam/magit-buffer-p buffer)
      (kill-buffer buffer))))

(setq magit-auto-revert-mode-lighter     ""
      magit-last-seen-setup-instructions "1.4.0"
      magit-push-always-verify           nil
      ;; Don't prompt to save buffers in the current repo before performing Magit actions
      magit-save-repository-buffers      'dontask)

(add-hook 'focus-in-hook #'cam/refresh-magit-buffers)

(cam/tweak-package magit
  :mode-name magit-status-mode
  :keys (("C-x 4 0" . #'cam/kill-all-magit-buffers-and-windows)
         ("M-!"     . #'cam/magit-shell-command)
         ("V"       . #'cam/magit-visit-pull-request-url)
         ("s-u"     . #'magit-refresh)))

(provide 'cam-magit)
