;;; cam-commands -- General commands that don't need to be loaded right away -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'dash)
  (require 'loccur))

(cl-eval-when (load)
  (message "CAM COMMANDS AUTOLOADED!"))


;;; [[General Editing/Navigation Functions]] ------------------------------------------------------------

;;;###autoload
(defun cam/untabify-current-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun cam/join-next-line ()
  (interactive)
  (join-line -1))

;; TODO - why not just make this an autoload???
;;;###autoload
(defun cam/loccur ()
  (interactive)
  (require 'loccur)
  (call-interactively #'loccur))

;;;###autoload
(defun cam/backward-kill-line ()
  "Kill line from current cursor position to beginning of line."
  (interactive)
  (kill-line 0))

;;;###autoload
(defun cam/hungry-delete-forward ()
  "Delete all but one spaces after `point'."
  (interactive)
  (while (= (char-after (1+ (point))) 32)
    (delete-char 1)))

;;;###autoload
(defun cam/hungry-delete-backward ()
  "Delete all but one spaces before `point'."
  (interactive)
  (while (= (char-before (1- (point))) 32)
    (delete-char -1)))

;;;###autoload
(defun cam/windmove-left-or-other-frame ()
  (interactive)
  (condition-case _
      (call-interactively #'windmove-left)
    (error (call-interactively #'other-frame))))

;;;###autoload
(defun cam/windmove-right-or-other-frame ()
  (interactive)
  (condition-case _
      (call-interactively #'windmove-right)
    (error (call-interactively #'other-frame))))


;;; [[External Search Commands]] ------------------------------------------------------------

(defun cam/string-remove-text-properties (string)
  "Return a copy of STRING with all of its text properties removed."
  (let ((s (copy-sequence string)))
    (set-text-properties 0 (length s) nil s)
    s))

(defun cam/symbol-at-point-name ()
  "The `symbol-name' of the `symbol-at-point' with its text properties removed."
  (when (symbol-at-point)
    (cam/string-remove-text-properties (symbol-name (symbol-at-point)))))

(eval-when-compile
  (require 'url-util)) ; for url-hexify-string

;;;###autoload
(defun cam/instant-clojure-cheatsheet-search (search-term)
  "Open a browser window and search Instant Clojure Cheatsheet for SEARCH-TERM."
  (interactive (list (read-string "Search Instant Clojure Cheatsheet for: " (cam/symbol-at-point-name))))
  (browse-url (format "http://localhost:13370/#?q=%s" (url-hexify-string search-term))))

;;;###autoload
(defun cam/duckduckgo-search (search-term)
  "Open a browser window and search DuckDuckGo for SEARCH-TERM."
  (interactive (list (read-string "Search DuckDuckGo for: " (cam/symbol-at-point-name))))
  (browse-url (concat "https://duckduckgo.com/?q=" search-term)))

;;;###autoload
(defun cam/javadocs-search (search-term)
  "Open a browser window and search javadocs.org for SEARCH-TERM."
  (interactive (list (read-string "Search javadocs.org for: " (when (symbol-at-point)
                                                                (string-remove-suffix "." (cam/symbol-at-point-name))))))
  (browse-url (format "http://javadocs.org/%s" search-term)))


;;; [[cam/insert-spaces-to-goal-column]] ----------------------------------------------------------------------

(defvar cam/insert-spaces-goal-col nil)

;;;###autoload
(defun cam/insert-spaces-to-goal-column (arg)
  "Insert or remove spaces until we reach `cam/insert-spaces-goal-col'.
Called with a prefix ARG, set the value of `cam/insert-spaces-goal-col' to point."
  (interactive "P")
  (if arg
      (progn (setq-local cam/insert-spaces-goal-col (current-column))
             (message "Insert spaces to column %d." (current-column)))
    (unless cam/insert-spaces-goal-col
      (error "Don't know where to insert spaces to! Call this function with a prefix arg to set it"))
    (let ((num-spaces (- cam/insert-spaces-goal-col (current-column))))
      (if (< num-spaces 0) (delete-char num-spaces)
        (insert-char ?  num-spaces)))))


;;; [[cam/realign-eol-comment-current-line]] ------------------------------------------------------------

(cl-defun cam/realign-eol-comment-current-line ()
  "Realign the end-of-line comment for the current line to `comment-column'."
  (interactive)
  (unless comment-column
    (error "comment-column is not set!"))
  (save-excursion
    (beginning-of-line)
    (let ((current-line (line-number-at-pos (point))))
      (search-forward ";")
      (backward-char)
      ;; Check to make sure we're still on the same line
      (unless (= (line-number-at-pos (point)) current-line)
        (cl-return-from cam/realign-eol-comment-current-line)))
    ;; Make sure this is just a single ";"
    (when (= (char-after (1+ (point))) ?\;)
      (cl-return-from cam/realign-eol-comment-current-line))
    ;; calculate the number of spaces to add / remove and do so
    (let ((cols-difference (- comment-column
                              (current-column))))
      ;; TODO - should we make sure we don't delete any chars that aren't spaces ?
      (cond
       ((> cols-difference 0) (insert-char ?  cols-difference))
       ((< cols-difference 0) (delete-char cols-difference))))))

;;;###autoload
(defun cam/realign-eol-comments ()
  "Re-align end-of-line comments to `comment-column' in the current region
if it is active; otherwise re-align comments on the current line."
  (interactive)
  (if (not (region-active-p)) (cam/realign-eol-comment-current-line)
    (let ((last-line (line-number-at-pos (region-end)))
          (line (line-number-at-pos (region-beginning))))
      (save-excursion
        (goto-char (region-beginning))
        (while (<= (prog1 line (cl-incf line)) last-line)
          (cam/realign-eol-comment-current-line)
          (forward-line))))))

(provide 'cam-commands)
;;; cam-commands.el ends here
