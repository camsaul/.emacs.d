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
 native-comp-jit-compilation t)

;; this stuff adapted from https://github.com/emacscollective/auto-compile#setup
(defun cam/-package-directory (package-name)
  (let ((elpa-dir (expand-file-name (concat user-emacs-directory "elpa/"))))
    ;; the elpa directory won't exist yet the first time Emacs is launched on a new machine.
    (when-let* (((file-directory-p elpa-dir))
                (dir (car (file-name-all-completions package-name elpa-dir))))
      (concat elpa-dir dir))))

;; if the auto-compile package exists...
(when-let* ((auto-compile-dir (cam/-package-directory "auto-compile")))
  ;; add auto-compile to the load path and load it
  (add-to-list 'load-path auto-compile-dir)
  (require 'auto-compile)
  ;; auto-compile should also do native compilation.
  (setq auto-compile-native-compile t)
  (setq auto-compile-verbose t)
  ;; recompile files when they get saved. `auto-compile-on-load-mode' gets enabled in init.el instead, once init.el
  ;; has compiled itself; enabling it here would make it try to recompile init.el before `load-path' is set up.
  (auto-compile-on-save-mode))

;;; early-init.el ends here
