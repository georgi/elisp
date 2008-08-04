;; buffer-cycle.el - cycle through buffer named without a *
;;

(defun filter (pred lst)
  (delq nil
        (mapcar (lambda (x) 
                  (and (funcall pred x) x)) lst)))

(defun filter-regexp (pattern lst)
  (filter (lambda (str)
            (string-match pattern str)) lst))

(defun cycle-buffer (&optional n)
  (let ((buffers (filter (lambda (buffer) 
                           (not (string-match "\*" (buffer-name buffer))))
                         (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
         (nth (+ (length buffers) n)
              buffers)
       (bury-buffer)
       (nth n buffers)))))

(defun cycle-buffer-next ()
  (interactive)
  (cycle-buffer -1))

(defun cycle-buffer-prev ()
  (interactive)
  (cycle-buffer 1))
