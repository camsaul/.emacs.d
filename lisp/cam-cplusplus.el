;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cam-tweak-package)
  (require 'cl-lib))

(require 'cc-mode)
(require 'cam-c)

(require 'eglot)
(require 'rtags)

(declare-function disaster "disaster")

(defun cam/c++-switch-between-header-and-impl ()
  "Toggle between the corresponding C++ header or implementation file for the current buffer. By default, raises an
error if the corresponding file does not exist; pass the prefix arg to suppress this error and visit the (new) file."
  (interactive)
  (let* ((header-extensions '("h" "hpp" "hxx" "hh" "H" "h++"))
         (impl-extensions '("cpp" "cxx" "cc" "C" "c++"))
         (possible-extensions (if (member (file-name-extension buffer-file-name) header-extensions)
                                  impl-extensions
                                header-extensions))
         found)
    (cl-labels ((file-name-with-extension
                 (extension)
                 (concat (file-name-sans-extension buffer-file-name) "." extension)))
      (cl-dolist (extension possible-extensions)
        (let ((new-file-name (file-name-with-extension extension)))
          (message "Trying %s..." new-file-name)
          (when (file-exists-p new-file-name)
            (find-file new-file-name)
            (setq found t))))
      (unless found
        (if current-prefix-arg
            (let ((default-extension (car possible-extensions)))
              (find-file (file-name-with-extension default-extension)))
          (user-error "Could not find related file for %s. Try again with C-u to create it" buffer-file-name))))))

(defun cam/insert-c++-log-message (text)
  (interactive "sstd::cout << ")
  (insert
   (concat
    "std::cout << "
    (if current-prefix-arg
        text
      (concat "\"" text " = \" << " text))
    " << std::endl;")))

(cam/tweak-package cc-mode
  :mode-name c++-mode
  :local-vars ((flycheck-highlighting-mode . nil))
  :setup ((cam/c-mode-setup)
          (add-to-list 'eglot-server-programs '((c++-mode) "clangd"))
          (eglot-ensure))
  :keys (("<f1>" . #'eldoc-doc-buffer)
         ("<f7>" . #'cam/c++-switch-between-header-and-impl)
         ("C-." . #'disaster)                     ; disassemble code at point
         ("C-j" . #'newline)
         ("M-." . #'rtags-find-symbol-at-point)
         ("<f10>" .  #'cam/insert-c++-log-message)))

(provide 'cam-cplusplus)
