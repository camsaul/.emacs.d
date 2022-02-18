;;; -*- lexical-binding: t; -*-

(require 'find-things-fast)

(nconc ftf-filetypes '("*.clj"
                       "*.cljc"
                       "*.cljs"
                       "*.css"
                       "*.edn"
                       "*.el"
                       "*.html"
                       "*.js"
                       "*.jsx"
                       "*.java"
                       "*.lisp"
                       "*.md"
                       "*.mustache"
                       "*.pml"
                       "*.yaml"
                       "*.yml"))

;; normally ftf deduplicates stuff only based on the immediate parent directory; instead tweak it so it uses the
;; entire path relative to the project root.
(advice-add #'ftf-uniqueify :around
  (lambda (_f file-cons)
    ;; cache the value of `(ftf-project-directory) because it's way to slow to call
    ;; on a big list of a few hundred files.
    (unless (boundp 'cam/-ftf-project-directory)
      (setq-local cam/-ftf-project-directory (ftf-project-directory)))
    (setcar file-cons
            (concat (car file-cons)
                    ": "
                    (string-remove-prefix
                     cam/-ftf-project-directory
                     (file-name-directory (cdr file-cons)))))))

(provide 'cam-ftf)
