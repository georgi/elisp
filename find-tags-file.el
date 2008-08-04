;; ********************************************************************************
;; Find TAGS file recursively
;;

(defun find-tags-file ()
  "recursively searches each parent directory for a file named `tags' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (labels
      ((find-tags-file-r (path)
         (let* ((parent (file-name-directory path))
                (possible-tags-file (concat parent "TAGS")))
           (cond
             ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
             ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
             (t (find-tags-file-r (directory-file-name parent)))))))

    (if (buffer-file-name)
        (catch 'found-it 
          (find-tags-file-r (buffer-file-name)))
        (error "buffer is not visiting a file"))))
