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


(defun rinari-launch ()
  "Run `rinari-minor-mode' if inside of a rails projcect,
otherwise turn `rinari-minor-mode' off if it is on."
  (interactive)
  (let* ((root (rinari-root)) (r-tags-path (concat root rinari-tags-file-name)))
    (if root
        (progn
          (setq toggle-mapping-style 'rspec)
          (setq toggle-mappings (toggle-style toggle-mapping-style))
          (setq toggle-which-function-command 'ruby-add-log-current-method)
          (setq toggle-method-format "def %s")
          (if (file-exists-p r-tags-path) (setq tags-file-name r-tags-path))
          (unless rinari-minor-mode (rinari-minor-mode t)))
      (if rinari-minor-mode (rinari-minor-mode)))))


(defun rinari-rgrep (&optional arg)
  "Search through the rails project for a string or `regexp'.
With optional prefix argument just run `rgrep'."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (funcall 'rgrep (read-from-minibuffer "search for: ")
             "*.rb *.rhtml *.yml *.erb *.liquid *.css *.js" (rinari-root))))


(provide 'rinari-extensions)