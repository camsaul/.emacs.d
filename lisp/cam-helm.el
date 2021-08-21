;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rx))

(require 'helm)

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
 ;; TODO -- this skips .emacs.d ??
 helm-ff--boring-regexp (rx (or (and "." (or "d" "o" "pch" "class" "elc") eol)
                                (and "~" eol)
                                (and bol "#" (1+ anything) "#" eol)
                                (and bol ".#"))))

(provide 'cam-helm)
