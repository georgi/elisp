(defun ffip-find-project-file (dir)
 (let ((f (expand-file-name ".git" dir))
       (parent (file-truename (expand-file-name ".." dir))))
   (cond ((string= dir parent) nil)
         ((file-exists-p f) dir)
         (t (ffip-find-project-file parent)))))

(defun ffip-project-files ()
  (let* ((default-directory (concat (ffip-find-project-file default-directory) "/")))
    (split-string (shell-command-to-string "git ls-files"))))

;;;###autoload
(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((files (ffip-project-files))
         (file (completing-read "Find file in project: " files)))
    (find-file file)))


(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
