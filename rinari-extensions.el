;; rinari-extensions.el - modifications and extensions to rinari
;;

(defvar rails-base-url)

(add-to-list 'rinari-subdirs '(:migration . "db/migrate/*\.rb$"))

(defun rinari-root (&optional dir)
  (or dir (setq dir default-directory))
  (if dir
      (if (file-exists-p (concat dir "config/environment.rb"))
	  dir
	(let* ((dir (directory-file-name dir))
	       (parent (file-name-directory dir)))
	  (if (and parent (not (equal parent dir)))
	    (rinari-root parent))))))


;; modified to not auto generate model
(defun rinari-find-model ()
  "Go to the most logical model given the current location."
  (interactive)
  (let ((default-directory (concat (rinari-root) "app/models/")))
    (case (rinari-whats-my-type)
      (:model (rinari-find-file))
      (:unit (toggle-buffer))
      (t (let ((obj (rinari-whats-my-object)))
           (if obj
               (or (rinari-open :model (singularize-string obj))
                   (rinari-open :model (pluralize-string obj))
                   (rinari-find-file))
             (rinari-find-file)))))))


(defun rinari-find-controller-or-select()
  "Find controller or select controller file"
  (interactive)
  (or (rinari-find-controller)
      (string-match "_controller" buffer-file-name)
      (let ((default-directory (concat (rinari-root) "app/controllers/")))
	(rinari-find-file))))

(defun rinari-find-view-or-select()
  "Find view or select view file"
  (interactive)
  (or (rinari-find-view)
      (let ((default-directory (concat (rinari-root) "app/views/")))
	(rinari-find-file))))

(defun rinari-run-script (script type)
  (let ((default-directory (or (rinari-root) default-directory))
	(name (read-from-minibuffer (format "%s %s: " script type))))
    (shell-command (format "./script/%s %s %s" script type name) "*Rails-Command*")
    (rinari-open (intern (concat ":" type)) name)))

(defmacro rinari-script-key-hook (script type)
  `(lambda ()
     (interactive)
     (rinari-run-script ,script ,type)))


(provide 'rinari-extensions)