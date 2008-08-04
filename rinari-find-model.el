;; rinari-find-model.el - modified to not auto generate model
;;
(defun rinari-find-model ()
  "Go to the most logical model given the current location."
  (interactive)
  (let ((default-directory (concat (rinari-root) "app/models/")))
    (case (rinari-whats-my-type)
      (:model (rinari-find-file))
      (:unit (toggle-buffer))
      (t (let ((obj (rinari-whats-my-object)))
           (message (format "%S" obj))
           (if obj
               (or (rinari-open :model (singularize-string obj))
                   (rinari-open :model (pluralize-string obj))
                   (rinari-find-file))
             (rinari-find-file)))))))
