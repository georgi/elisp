;; rinari-extensions.el - modifications and extensions to rinari
;;

;; modified to not auto generate model
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



;; modified to use project-specific base url from variable rails-base-url.
;; also discarded non-restful urls, instead it just takes the controller path.
(defun rinari-browse-url ()
  "Browse the url of the current view, controller, test, or model
with `rinari-browse-url-func' which defaults to `browse-url'."
  (interactive)
  (unless (equal :view (rinari-whats-my-type))
    (rinari-find-view))
  (let* ((path (buffer-file-name))
	 (route (and (string-match "app/views/\\(.+\\)/" path)
		     (match-string 1 path)))
	 (base (concat (or rails-base-url "localhost:3000") "/" route))
	 (url (read-from-minibuffer "url: " (concat base "/"))))
    (eval (list rinari-browse-url-func url))))

(defun rinari-run-script (script type)
  (let ((default-directory (or (rinari-root) default-directory))
	(name (read-from-minibuffer (format "%s %s: " script type))))
    (shell-command (format "./script/%s %s %s" script type name) "*Rails-Command*")))

(defmacro rinari-script-key-hook (script type)
  `(lambda ()
     (interactive)
     (rinari-run-script ,script ,type)))