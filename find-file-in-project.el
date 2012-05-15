(defun ffip-uniqueify (file-cons)
  "Set the car of FILE-CONS to include the directory name plus the file name."
  ;; TODO: see if we can use uniquify.el
  (setcar file-cons
          (concat (cadr (reverse (split-string (cdr file-cons) "/"))) "/"
                  (car file-cons))))

(defun ffip-project-files ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (file-name-nondirectory file)
                                     (expand-file-name file))))
                (when (assoc (car file-cons) file-alist)
                  (ffip-uniqueify (assoc (car file-cons) file-alist))
                  (ffip-uniqueify file-cons))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string "git ls-files")))))

;;;###autoload
(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((project-files (ffip-project-files))
         (files (mapcar 'car project-files))
         (file (if (and (boundp 'ido-mode) ido-mode)
                   (ido-completing-read "Find file in project: " files)
                 (completing-read "Find file in project: " files))))
    (find-file (cdr (assoc file project-files)))))

;;;###autoload
(defalias 'ffip 'find-file-in-project)


(provide 'find-file-in-project)
;;; find-file-in-project.el ends here
