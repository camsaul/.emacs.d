;;; cam-commands -- General commands that don't need to be loaded right away -*- lexical-binding: t; cam/byte-compile: t; cam/generate-autoloads: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

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

;;;###autoload
(defun cam/instant-clojure-cheatsheet-search (search-term)
  "Open a browser window and search Instant Clojure Cheatsheet for SEARCH-TERM."
  (interactive (list (read-string "Search Instant Clojure Cheatsheet for: " (when (symbol-at-point)
                                                                              (-> (symbol-at-point)
                                                                                  symbol-name
                                                                                  cam/string-remove-text-properties)))))
  (browse-url (format "http://localhost:13370/#?q=%s" search-term)))

;;;###autoload
(defun cam/bing-search (search-term)
  "Open a browser window and search BING for SEARCH-TERM."
  (interactive (list (read-string "Search Bing for: " (when (symbol-at-point)
                                                        (-> (symbol-at-point)
                                                            symbol-name
                                                            cam/string-remove-text-properties)))))
  (browse-url (format "http://bing.com/search?q=%s" search-term)))

;;;###autoload
(defun cam/browse-korma-dox ()
  "Open a browser window with the SQL Korma documentation."
  (interactive)
  (browse-url "http://www.sqlkorma.com/docs"))

;;;###autoload
(defun cam/javadocs-search (search-term)
  "Open a browser window and search javadocs.org for SEARCH-TERM."
  (interactive (list (read-string "Search javadocs.org for: " (when (symbol-at-point)
                                                                (->> (symbol-at-point)
                                                                     symbol-name
                                                                     cam/string-remove-text-properties
                                                                     (string-remove-suffix "."))))))
  (browse-url (format "http://javadocs.org/%s" search-term)))


;;; [[cam/insert-spaces-to-goal-column]] ----------------------------------------------------------------------

(defvar cam/insert-spaces-goal-col nil)

;;;###autoload
(defun cam/insert-spaces-to-goal-column (arg)
  "Insert or remove spaces until we reach `cam/insert-spaces-goal-col'.
Called with a prefix ARG, set the value of `cam/insert-spaces-goal-col' to point."
  (interactive "P")
  (if arg (progn (setq-local cam/insert-spaces-goal-col (current-column))
                 (message "Insert spaces to column %d." (current-column)))
    (progn
      (unless cam/insert-spaces-goal-col
        (error "Don't know where to insert spaces to! Call this function with a prefix arg to set it"))
      (let ((num-spaces (- cam/insert-spaces-goal-col (current-column))))
        (if (< num-spaces 0) (delete-char num-spaces)
          (insert-char ?  num-spaces))))))


;;; [[cam/align-map]] ------------------------------------------------------------

(defun cam/-align-map-get-max-col (&optional max)
  "Used internally by `cam/align-map'.
Get the maximum column for the start of all the value forms in the current map."
  (save-excursion
    (condition-case _
        (progn
          (backward-sexp 2)                          ; Move from end of val to beginning of key
          (forward-sexp)                             ; Move to end of key
          (let ((col (+ (current-column) 1)))        ; val should start one space after key
            (backward-sexp)                          ; Move back to start of key
            (cam/-align-map-get-max-col (max (or max 0) col)))) ; recurse until error is thrown when we reach the first key
      (error (message "Max column is %d" max)
             max))))

(defun cam/-align-map-args-to-column ()
  "Used internally by `cam/align-map'.
Align all the arguments in a map using `cam/insert-spaces-to-goal-column'."
  (save-excursion
    (ignore-errors
      (backward-sexp)                                ; move to start of val
      (cam/insert-spaces-to-goal-column nil)         ; insert spaces
      (backward-sexp)                                ; move to start of key
      (cam/-align-map-args-to-column))))              ; recurse until error is thrown when we reach the first sexp

;;;###autoload
(defun cam/align-map ()
  "Align the values in a Clojure map or similar data structure."
  (interactive)
  (save-excursion
    (when (paredit-in-string-p)                      ; If we're in a string jump out so we don't insert a } when calling (paredit-close-curly)
      (paredit-forward-up))
    (paredit-close-curly)                            ; jump to char after closing }
    (backward-char)                                  ; move back onto } -- end of last sexp
    (setq-local cam/insert-spaces-goal-col (cam/-align-map-get-max-col))
    (cam/-align-map-args-to-column))
  (paredit-reindent-defun))


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
