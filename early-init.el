;;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;; (Things that need to happen as soon as this file starts loading)

;; allow Emacs to use unlimited memory on launch. Then we'll reign it back in once Emacs has finished launching.
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
  (lambda ()
    ;; By default GC starts around ~780kB. Since this isn't the 90s GC when we hit 512MB
    (setq gc-cons-threshold (* 512 1024 1024))))

(setq
 ;; load .el files if they're newer than .elc/.eld ones
 load-prefer-newer t
 ;; do native compilation whenever we do byte compilation.
 native-comp-deferred-compilation t)

;; this stuff adapted from https://github.com/emacscollective/auto-compile#setup
(defun cam/-package-directory (package-name)
  (let ((elpa-dir (expand-file-name (concat user-emacs-directory "elpa/"))))
    (when-let ((dir (car (file-name-all-completions package-name elpa-dir))))
      (concat elpa-dir dir))))

(defconst cam/autoloads-file (expand-file-name (concat user-emacs-directory "lisp/loaddefs.el")))

;; if the packed and auto-compile packages exist...
(when-let ((packed-dir (cam/-package-directory "packed"))
           (auto-compile-dir (cam/-package-directory "auto-compile")))
  ;; create a loaddefs.el file if one does not already exist
  (unless (file-exists-p cam/autoloads-file)
    (shell-command (concat  "touch " cam/autoloads-file)))
  ;; add packed and auto-compile to the load path and load auto-compile
  (add-to-list 'load-path packed-dir)
  (add-to-list 'load-path auto-compile-dir)
  (require 'auto-compile)
  ;; auto-compile should also do native compilation and update autoloads.
  (setq auto-compile-native-compile t)
  (setq auto-compile-update-autoloads t)
  (setq auto-compile-verbose t)
  ;; enable auto-compile
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; early-init.el ends here
