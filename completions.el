;; ********************************************************************************
;; Completion
;;
(require 'cl)
(require 'hippie-exp)
(require 'abbrev)
(require 'snippet)
(require 'etags)
(require 'ispell)
(require 'dropdown-list)

(eval-when-compile (require 'css-mode))

(load "javascript-symbols")

(setq dabbrev--abbrev-char-regexp "\\sw\\|\\s_")

(defun he-symbol-beginning ()
  (save-excursion
    (skip-syntax-backward "w_")
    (point)))

(defun he-word-beginning ()
  (let ((p
         (save-excursion
           (backward-word 1)
           (point))))
    p))

(defun try-expand-collection (old collection)
  (unless  old
    (he-init-string (he-symbol-beginning) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string collection) 'string-lessp)))
  (try-expand-completion old))

(defun try-expand-completion (old)
  (if (> (length he-expand-list) 10)
      (let ((idx (dropdown-list he-expand-list)))
        (he-substitute-string (nth idx he-expand-list)))
 
    (while (and he-expand-list 
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list)))

    (if (null he-expand-list)
        (when old (he-reset-string))

      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun find-all-dabbrev-expansions (abbrev)
  (let ((all-expansions nil)
        expansion)
    (save-excursion
      (while (setq expansion (he-dabbrev-search abbrev nil))
        (unless (member expansion all-expansions)
          (setq all-expansions (cons expansion all-expansions))))
      (while (setq expansion (he-dabbrev-search abbrev t))
        (unless (member expansion all-expansions)
          (setq all-expansions (cons expansion all-expansions))))
    all-expansions)))

(defun try-expand-dabbrev (old)
  (unless old
    (he-init-string (he-symbol-beginning) (point))
    (setq he-expand-list (sort (find-all-dabbrev-expansions he-search-string) 'string-lessp)))
  (try-expand-completion old))

(defun try-expand-abbrev (old)
  (expand-abbrev))

(defun try-expand-rng (old)
  (rng-complete))

(defun try-expand-tag (old)
  (if tags-file-name
       (try-expand-collection old 'tags-complete-tag)))

(defun try-complete-lisp-symbol (old)
  (he-init-string (he-symbol-beginning) (point))
  (setq he-expand-list (sort (all-completions 
                              he-search-string obarray
                              (function (lambda (sym)
                                          (or (boundp sym)
                                              (fboundp sym)
                                              (symbol-plist sym))))) 'string-lessp))
  (try-expand-completion old))

(defun try-expand-css-property (old)
  (try-expand-collection old cssm-properties))

(defun try-expand-javascript-symbol (old)
  (try-expand-collection old javascript-symbols))

(defun try-expand-ispell (old)
  (try-expand-collection old 'ispell-complete))

(defun ispell-complete (string predicate what)
  "Completion function for ispell."
  (let ((completions (lookup-words (concat string "*"))))
    (if predicate
        (remove-if-not predicate completions)
      completions)))


(defun indent-and-complete ()
  "Indent line and complete"
  (interactive)

  (cond
   ((and (boundp 'snippet) snippet)
    (snippet-next-field))

;;;    ((boundp 'nxml-completion-hook)
;;;     (nxml-complete))

   ;; ((looking-at "\\_>")
   ;;  (hippie-expand nil))
   
   ((looking-at "\\_>")
    (unless (ac-menu-live-p)
      (ac-fuzzy-complete))

   ((indent-for-tab-command))))
