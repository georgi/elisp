;; window-management.el -- functions to resize windows conveniently
;;

(defun window-resize-down ()
  "Shrink"
  (interactive)
  (if (= 0 (second (window-edges)))
      (call-interactively 'enlarge-window)
    (call-interactively 'shrink-window)))
	 
(defun window-resize-up ()
  (interactive)
  (if (= 0 (second (window-edges)))
      (call-interactively 'shrink-window)
    (call-interactively 'enlarge-window)))
	

