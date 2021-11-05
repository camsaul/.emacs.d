;;; cam-theme -- Theme-related stuff, powerline, font/frame setup -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

(eval-when-compile
  (require 'subr-x))

;;; font setup

(defun cam/-hdpi-scale ()
  (or (when (and (boundp 'pgtk-initialized)
                 pgtk-initialized)
        (when-let ((scale (getenv "GDK_DPI_SCALE")))
          (string-to-number scale)))
      1))

;; use a different font size depending on whether we're rendering in pure GTK w/ display scaling or not.
(defun cam/-font ()
  (let* ((font-size (ceiling (/ 20 (cam/-hdpi-scale))))
         ;; use shell command `fc-list` to get the list of available fonts on the system.
         ;; M-x list-fontsets to list the fonts available to Emacs
         (font-name
          "DejaVuSansMono"
          ;; "CascadiaCode"
          ))
    (format "%s-%d" font-name font-size)))

;; (set-frame-font "-SAJA-Cascadia Code PL-*-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;;; theme setup

(declare-function moe-dark "moe-theme")

(defun cam/-set-theme ()
  (require 'moe-theme)
  (moe-dark))

(declare-function unicode-fonts-setup "unicode-fonts")

(defun cam/-set-frame-font ()
  (let ((font (cam/-font)))
    (message "Using font %s" font)
    ;; nil = don't worry about keeping the current frame size.
    ;; t = apply font to all frames going forward & save setting to custom.el (supposedly)
    (set-frame-font font nil t))
  ;; create unicode mappings for our font if needed.
  (require 'unicode-fonts)
  (unicode-fonts-setup))

(defun cam/setup-frame ()
  (cam/-set-frame-font)
  (cam/-set-theme)
  (set-fringe-style '(6 . 0))           ; ¾ width fringe on the left and none on the right
  (moe-theme-random-color)
  (set-face-foreground 'mode-line "#111111")
  (set-cursor-color (face-background 'mode-line))
  ;;  Don't show a blue background behind buffer name on modeline for deselected frames
  (set-face-background 'mode-line-buffer-id nil)
  ;; maximize the screen, unless we launched with it maximized.
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (toggle-frame-maximized)))

(advice-add #'make-frame-command :after #'cam/setup-frame)

;;; mode line setup

(require 'powerline)

(kill-local-variable 'mode-line-format)

(defun cam/theme--face-symbol (active-or-inactive where)
  (intern (format "cam/%s-%s" active-or-inactive where)))

;; define our various faces now
(dolist (group '((active . ((side-outer "gray70" "black")
                            (side-center "gray80" "black")
                            (side-inner "gray90" "black")
                            (center "white" "black")))
                 (inactive . ((side-outer "gray10" "gray60")
                              (side-center "gray20" "gray70")
                              (side-inner "gray30" "gray80")
                              (center "gray40" "gray90")))))
  (let ((active-or-inactive (car group))
        (faces (cdr group)))
    (dolist (face faces)
      (let ((symb (cam/theme--face-symbol active-or-inactive (car face)))
            (bg (cadr face))
            (fg (caddr face)))
        (face-spec-set symb (list (list t :background bg :foreground fg)))))))

(defvar cam/theme-mode-tag-function nil)

(defun cam/theme--mode-tag ()
  (if cam/theme-mode-tag-function
      (funcall cam/theme-mode-tag-function)
    "EMACS"))

(face-spec-set 'cam/theme--default-mode-face
               '((t :background "#cc6633" :foreground "white")))

(defvar cam/theme-mode-face-symbol-function nil)

(defun cam/theme--mode-face-symbol ()
  (if cam/theme-mode-face-symbol-function
      (funcall cam/theme-mode-face-symbol-function)
    'cam/theme--default-mode-face))

(setq-default
 mode-line-format
 '("%e"
   (:eval
    (let* ((active            (powerline-selected-window-active))
           (active-or-inactive (if active 'active 'inactive))
           (color-face         (if active
                                   (cam/theme--mode-face-symbol)
                                 (cam/theme--face-symbol active-or-inactive 'side-outer)))
           (side-outer-face    (cam/theme--face-symbol active-or-inactive 'side-outer))
           (side-center-face   (cam/theme--face-symbol active-or-inactive 'side-center))
           (side-inner-face    (cam/theme--face-symbol active-or-inactive 'side-inner))
           (center-face        (cam/theme--face-symbol active-or-inactive 'center))

           (lhs-outside
            (list
             (powerline-raw (concat (cam/theme--mode-tag) " ") color-face 'l)))

           (lhs-center
            (list
             (powerline-raw
              (concat
               ;; %b = buffer name
               " %b "
               (when (buffer-modified-p)
                 "[modified] "))
              side-center-face)
             (powerline-arrow-left side-center-face side-inner-face)))

           (lhs-inside
            (list
             (powerline-major-mode side-inner-face 'l)
             (powerline-process side-inner-face)
             (powerline-raw " " side-inner-face)))

           (center-left
            (list
             (powerline-arrow-left side-inner-face center-face)
             (powerline-minor-modes center-face 'l)
             (powerline-narrow center-face 'l)
             (powerline-raw " " center-face)))

           (center-right
            (list
             (when-let ((process (powerline-process)))
               (powerline-raw process center-face 'r))
             (powerline-arrow-right center-face side-inner-face)))

           (rhs-inside
            (list
             (powerline-raw
              (concat
               " "
               (powerline-encoding)
               (when buffer-read-only
                 " [readonly]"))
              side-inner-face
              'r)
             (powerline-arrow-right side-inner-face side-center-face)))

           (rhs-center
            (list
             (powerline-raw
              ;; %l = line number; %C = column number
              (concat
               " L%l/"
               (int-to-string (line-number-at-pos (point-max)))
               " C%C")
              side-center-face 'r)
             (powerline-arrow-right side-center-face side-outer-face)))

           (rhs-outside
            (list
             (when global-mode-string
               (powerline-raw global-mode-string side-outer-face 'r))
             (powerline-vc side-outer-face 'r)))

           (lhs (append lhs-outside lhs-center lhs-inside center-left))

           (rhs (append center-right rhs-inside rhs-center rhs-outside)))
      (concat
       (powerline-render lhs)
       (powerline-fill center-face (powerline-width rhs))
       (powerline-render rhs))))))

(set-face-bold 'mode-line-inactive nil)
(set-face-bold 'mode-line nil)

(provide 'cam-theme)
