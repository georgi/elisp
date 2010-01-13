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

(defun tags-table-append (tags-file file)
  (call-process "ctags" nil nil nil "-e" "-a" "-f" tags-file file))

(defun create-tags ()
  "Create tags table."
  (interactive)
  (let* ((tags-file (or (find-tags-file) (read-file-name "tags table: ")))
	 (default-directory (file-name-directory tags-file)))
      (shell-command "ctags -e -R"))))

(defun update-tags ()
  (let ((tags-file (find-tags-file)))
    (when tags-file
	(tags-table-append tags-file buffer-file-name))))

;; (add-hook 'after-save-hook 'update-tags)
