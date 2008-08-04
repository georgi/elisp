;; imenu-from-pattern.el - Create imenu form pattern hook
;;  

(defmacro imenu-from-pattern (&rest patterns)
  "Create a hook function, which shows a imenu based on a list of PATTERNS."
  `(lambda ()
     (interactive)
     (setq imenu-create-index-function 
           (lambda ()
             (let (result) 
               (dolist (pattern (quote ,patterns))
                 (goto-char (point-min))
                 (while (re-search-forward pattern (point-max) t)
                   (push (cons (match-string 1) (match-beginning 1)) result)))
               (nreverse result))))
  (call-interactively 'imenu)))

