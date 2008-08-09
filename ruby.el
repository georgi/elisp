;; ruby.el -- some functions for ruby-mode

(defun ruby-xmp-region (reg-start reg-end)
  "Pipe the region through Ruby's xmp utility and replace
     the region with the result."
  (interactive "r")
  (shell-command-on-region reg-start reg-end
			   "ruby -r xmp -n -e 'xmp($_, \"%l\t\t# %r\n\")'" t))

(defun ruby-send-block-or-line ()
  (interactive)
  (save-excursion
    (if (re-search-backward "[\n\t ]\\(.*\\)[\n\t ]" nil t)
	(let ((foo (match-string 0)))
	  (set-text-properties 0 (length foo) nil foo)
	  (if (string= foo "end")
	      (progn
	       (forward-line -1)
	       (ruby-send-block))
	    (ruby-send-region (line-beginning-position) (line-end-position)))))))
