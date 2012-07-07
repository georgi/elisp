(defun ffip-find-project-file (dir)
 (let ((f (expand-file-name ".git" dir))
       (parent (file-truename (expand-file-name ".." dir))))
   (cond ((string= dir parent) nil)
         ((file-exists-p f) (expand-file-name dir))
         (t (ffip-find-project-file parent)))))

(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((default-directory (concat (ffip-find-project-file default-directory) "/"))
	 (files (split-string (shell-command-to-string "git ls-files")))
	 (file (completing-read "Find file in project: " files)))
	(find-file (concat default-directory file))))

(provide 'find-file-in-project)
