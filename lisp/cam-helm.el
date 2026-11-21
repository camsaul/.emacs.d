;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rx))

(require 'helm)

;; these belong to helm libraries that get loaded on demand.
(defvar helm-buffers-fuzzy-matching)                        ; helm-buffers
(defvar helm-recentf-fuzzy-match)                           ; helm-for-files
(defvar helm-M-x-fuzzy-match)                               ; helm-command
(defvar helm-M-x-always-save-history)
(defvar helm-ff-skip-git-ignored-files)                     ; helm-files
(defvar helm-ff-skip-boring-files)

(setq
 ;; enable fuzzy matching for helm
 helm-buffers-fuzzy-matching t
 helm-recentf-fuzzy-match    t
 helm-M-x-fuzzy-match        t
 ;; Have C-x C-f skip files in .gitignore TODO -- disabled to now -- too hard to find these files when I need
 ;; to look at them.
 helm-ff-skip-git-ignored-files nil
 ;; Have C-x C-f skip "boring" files matching the regex below
 helm-ff-skip-boring-files t
 ;; Helm matches this against the base name of the file, not the entire path.
 helm-ff--boring-regexp (rx (or (and "." (or "o" "pch" "class" "elc") eol)
                                ;; skip .d dependency files, but only ones that don't start with a dot, so things like
                                ;; .emacs.d still show up. (Emacs regexes don't have lookahead, hence the `bol'.)
                                (and bol (not (any ".")) (0+ anything) ".d" eol)
                                (and "~" eol)
                                (and bol "#" (1+ anything) "#" eol)
                                (and bol ".#")))

 ;; save command in history even if it fails
 helm-M-x-always-save-history t)

(provide 'cam-helm)
