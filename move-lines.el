;; move-lines.el -- move and copy lines
;;

(defun copy-line (n)
  (let ((column (current-column))
        (line (buffer-substring (line-beginning-position) (+ (line-end-position) 1))))
    (beginning-of-line)
    (forward-line n)
    (insert line)
    (forward-line -1)
    (forward-char column)))
    
(defun copy-line-up ()
  "Copy the current line up"
  (interactive)
  (copy-line 0))

(defun copy-line-down ()
  "Copy the current line down"
  (interactive)
  (copy-line 1))

(defun move-line (n)
  (let ((column (current-column))
        (line (delete-and-extract-region (line-beginning-position) (+ (line-end-position) 1))))
    (forward-line n)
    (insert line)
    (forward-line -1)
    (forward-char column)))

(defun move-line-up ()
  "Move the current line up"
  (interactive)
  (move-line -1))

(defun move-line-down ()
  "Move the current line down"
  (interactive)  
  (move-line 1))
