;;; -*- lexical-binding: t; -*-

(defconst sudoku-mode--original-board
  '(((0 . 0) . 5)
    ((0 . 1) . 3)
    ((0 . 4) . 7)
    ((1 . 0) . 6)
    ((1 . 3) . 1)
    ((1 . 4) . 9)
    ((1 . 5) . 5)
    ((2 . 1) . 9)
    ((2 . 2) . 8)
    ((2 . 7) . 6)
    ((2 . 8) . 7)
    ((3 . 0) . 8)
    ((3 . 4) . 6)
    ((3 . 8) . 3)
    ((4 . 0) . 4)
    ((4 . 3) . 8)
    ((4 . 5) . 3)
    ((4 . 8) . 1)
    ((5 . 0) . 7)
    ((5 . 4) . 2)
    ((5 . 8) . 6)
    ((6 . 1) . 6)
    ((6 . 6) . 2)
    ((6 . 7) . 8)
    ((7 . 3) . 4)
    ((7 . 4) . 1)
    ((7 . 5) . 9)
    ((7 . 8) . 5)
    ((8 . 4) . 8)
    ((8 . 7) . 7)
    ((8 . 8) . 9)))

(defvar-local sudoku-mode-board
  sudoku-mode--original-board)

(defun sudoku-mode--reset-board ()
  (setq sudoku-mode-board sudoku-mode--original-board))

(defun sudoku-mode--cell-original-value (row col)
  (cdr (assoc (cons row col) sudoku-mode--original-board)))

(defun sudoku-mode--get-cell (board row col)
  (cdr (assoc (cons row col) board)))

(defun sudoku-mode--set-cell (board row col new-value)
  (cons (cons (cons row col) new-value) board))

(defun sudoku-mode--print-horizontal-divider (type &optional where)
  (princ (case type
           ('light "++---+---+---++---+---+---++---+---+---++\n")
           ('heavy "++===+===+===++===+===+===++===+===+===++\n"))
         where))

(defun sudoku-mode--print-row (board row &optional where)
  (cl-loop for col from 0 to 8
           do
           (princ (if (zerop (mod col 3))
                      "||"
                    "|")
                  where)
           (princ " " where)
           (princ (or (sudoku-mode--get-cell board row col)
                      " ")
                  where)
           (princ " " where))
  (princ "||\n" where))

(defun sudoku-mode--print-board (board &optional where)
  (cl-loop for row from 0 to 8
           do
           (sudoku-mode--print-horizontal-divider (if (zerop (mod row 3))
                                                      'heavy
                                                    'light)
                                                  where)
           (sudoku-mode--print-row board row where))
  (sudoku-mode--print-horizontal-divider 'heavy where)
  nil)

(defun sudoku-mode--redisplay-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (sudoku-mode--print-board sudoku-mode-board (current-buffer)))
  (sudoku-mode--update-point))

(defun sudoku-mode--read-number-in-range (description lower-bound upper-bound)
  (let (n)
    (while (or (null n)
               (not (<= lower-bound n upper-bound)))
      (when n
        (message "%s must be between %d and %d, inclusive"
                 description lower-bound upper-bound)
        (sit-for 1))
      (setq n (read-number (format  "%s: " description))))
    n))

(defun sudoku-mode-update-cell (row col value)
  (interactive (list
                (sudoku-mode--read-number-in-range "Row" 0 8)
                (sudoku-mode--read-number-in-range "Col" 0 8)
                (sudoku-mode--read-number-in-range "Value" 1 9)))
  (cl-assert (<= 0 row 8))
  (cl-assert (<= 0 col 8))
  (cl-assert (or (null value)
                 (<= 1 value 9)))
  (when (sudoku-mode--cell-original-value row col)
    (user-error "Cell at row %d, column %d was set in the original board" row col))
  (setq sudoku-mode-board (sudoku-mode--set-cell sudoku-mode-board row col value))
  (sudoku-mode--redisplay-board))

(defun sudoku-mode-reset-board ()
  (interactive)
  (when (y-or-n-p "Really reset Sudoku board? ")
    (setq sudoku-mode-board sudoku-mode--original-board)
    (sudoku-mode--redisplay-board)))

(setq sudoku-mode-map
      (let ((map (make-sparse-keymap 'sudoku-mode-map)))
        (define-key map "r" 'sudoku-mode-reset-board)
        (define-key map "u" 'sudoku-mode-undo)
        (define-key map (kbd "<left>") 'sudoku-mode-move-left)
        (define-key map (kbd "<right>") 'sudoku-mode-move-right)
        (define-key map (kbd "<up>") 'sudoku-mode-move-up)
        (define-key map (kbd "<down>") 'sudoku-mode-move-down)
        (define-key map "1" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "2" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "3" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "4" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "5" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "6" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "7" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "8" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map "9" 'sudoku-mode-set-current-cell-to-last-key)
        (define-key map (kbd "<delete>") 'sudoku-mode-unset-current-cell)
        (define-key map (kbd "<backspace>") 'sudoku-mode-unset-current-cell)
        map))

(define-derived-mode sudoku-mode nil "Sudoku"
  "A mode for playing Sudoku."
  (setq buffer-read-only t))

(defun sudoku ()
  "Play Sudoku."
  (interactive)
  (let ((buffer-number 0)
        (buffer-name "*Sudoku*"))
    (while (get-buffer buffer-name)
      (setq buffer-number (1+ buffer-number))
      (setq buffer-name (format "*Sudoku* <%d>" buffer-number)))
    (switch-to-buffer-other-window buffer-name)
    (sudoku-mode)
    (sudoku-mode--redisplay-board)))

(defun sudoku-mode--move-point-to-cell (row col)
  (let ((n-lines (1+ (* row 2)))
        (n-cols (+ 3 (* col 4) (/ col 3))))
    (goto-char (point-min))
    (forward-line n-lines)
    (forward-char n-cols)))

(defvar-local sudoku-mode--current-row 0)
(defvar-local sudoku-mode--current-col 0)

(defun sudoku-mode--update-point ()
  (sudoku-mode--move-point-to-cell
   sudoku-mode--current-row
   sudoku-mode--current-col))

(defun sudoku-mode-move-left ()
  (interactive)
  (when (> sudoku-mode--current-col 0)
    (setq sudoku-mode--current-col (1- sudoku-mode--current-col)))
  (sudoku-mode--update-point))

(defun sudoku-mode-move-right ()
  (interactive)
  (when (< sudoku-mode--current-col 8)
    (setq sudoku-mode--current-col (1+ sudoku-mode--current-col)))
  (sudoku-mode--update-point))

(defun sudoku-mode-move-up ()
  (interactive)
  (when (> sudoku-mode--current-row 0)
    (setq sudoku-mode--current-row (1- sudoku-mode--current-row)))
  (sudoku-mode--update-point))

(defun sudoku-mode-move-down ()
  (interactive)
  (when (< sudoku-mode--current-row 8)
    (setq sudoku-mode--current-row (1+ sudoku-mode--current-row)))
  (sudoku-mode--update-point))

(defun sudoku-mode-update-current-cell (value)
  (interactive "nValue: ")
  (sudoku-mode-update-cell
   sudoku-mode--current-row
   sudoku-mode--current-col
   value))

(defun sudoku-mode-set-current-cell-to-last-key ()
  (interactive)
  (let ((key (this-command-keys)))
    (cl-assert (stringp key))
    (sudoku-mode-update-current-cell (string-to-number key))))

(defun sudoku-mode-unset-current-cell ()
  (interactive)
  (sudoku-mode-update-current-cell nil))

(defun sudoku-mode-undo ()
  (interactive)
  (when (eq sudoku-mode-board sudoku-mode--original-board)
    (user-error "Cannot undo any further"))
  (pop sudoku-mode-board)
  (sudoku-mode--redisplay-board))

(provide 'sudoku-mode)
