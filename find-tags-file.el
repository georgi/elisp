;; ********************************************************************************
;; Find TAGS file recursively
;;
(require 'cl)

(defun find-tags-file ()
  "recursively searches each parent directory for a file named `TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (labels
      ((find-tags-file-r (path)
         (let* ((parent (file-name-directory path))
                (file (concat parent "TAGS")))
           (cond
             ((file-exists-p file) (throw 'found-it file))
             ((equal path parent) (throw 'found-it nil))
             (t (find-tags-file-r (directory-file-name parent)))))))

    (if (buffer-file-name)
        (catch 'found-it 
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))


(defun find-files (directory pattern)
  (let ((default-directory directory))
    (call-process "find" nil "*Find*" nil "-name" pattern)
    (set-buffer "*Find*")
    (let ((files (buffer-substring-no-properties 1 (point-max))))
      (kill-buffer "*Find*")
      (split-string files))))


(defun find-files-newer-than (directory pattern mtime)
  (let ((files (find-files directory pattern))
	(default-directory directory))
    (setq files (mapcar 'expand-file-name files))
    (remove-if (lambda (file)
		 (let ((time (file-mtime file)))
		   (or (< (first time) (first mtime))
		       (and (= (first time) (first mtime))
			    (< (second time) (second mtime)))))) files)))


(defun file-mtime (file)
  (nth 5 (file-attributes file)))

(defun tags-table-append (tags-file file)
  (call-process "ctags" nil nil nil "-e" "-a" "-f" tags-file file))

(defun update-tags-table ()
  "Update tags table with recent changes."
  (interactive)
  (let* ((tags-file (read-file-name "tags table: "))
	 (pattern (read-string "pattern: "))
	 (mtime (or (file-mtime tags-file) '(0 0)))
	 (directory (file-name-directory tags-file))
	 (files (find-files-newer-than directory pattern mtime)))
    (dolist (file files)
      (tags-table-append tags-file file))))

(defun create-tags ()
  "Create tags table."
  (interactive)
  (let* ((tags-file (read-file-name "tags table: "))
	 (default-directory (file-name-directory tags-file)))
    (if (file-exists-p "exclude.tags")
	(shell-command "ctags -e --exclude=@exclude.tags -R")
      (shell-command "ctags -e -R"))))


(defun update-tags ()
  (let ((tags-file (find-tags-file)))
    (when tags-file
	(tags-table-append tags-file buffer-file-name))))

(add-hook 'after-save-hook 'update-tags)
