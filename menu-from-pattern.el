;; menu-from-pattern.el - Create dropdown menu from regexep pattern
;;  

(defun menu-from-pattern (&rest patterns)
  "Shows a dropdown menu based on a list of PATTERN."
  (let (list idx)
    (save-excursion
      (dolist (pattern patterns)
	(goto-char (point-max))
	(while (re-search-backward pattern (point-min) t)
	  (push (cons (concat (match-string 1) " " (match-string 2)) (match-beginning 1)) list))))
    (when (> (length list) 0)
	(setq list (sort list (lambda (x y) (string< (car x) (car y)))))
	(goto-char (cdr (nth (dropdown-list (mapcar 'car list)) list))))))


(defun show-javascript-menu ()
  (interactive)
  (menu-from-pattern "[ .]+\\([[:alnum:]_]+\\) *[=:] *function *\\((.*)\\)" 
		     "function *\\([[:alnum:]_]+\\) *\\((.*)\\)"))

(defun show-ruby-menu () 
  (interactive)
  (menu-from-pattern "def \\([[:alnum:]._]+\\) *\\((.*)\\)*"))

(defun show-css-menu () 
  (interactive)
  (menu-from-pattern "\\(.*\\) \{"))