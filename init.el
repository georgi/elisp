;; ********************************************************************************
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/ac-nrepl")
(add-to-list 'load-path "~/.emacs.d/ac-octave")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/auto-complete-clang")
(add-to-list 'load-path "~/.emacs.d/chuck-mode")
(add-to-list 'load-path "~/.emacs.d/cl-lib")
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/flymake")
(add-to-list 'load-path "~/.emacs.d/flymake-ruby")
(add-to-list 'load-path "~/.emacs.d/ghc-mod")
(add-to-list 'load-path "~/.emacs.d/haml-mode")
(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/nrepl.el")
(add-to-list 'load-path "~/.emacs.d/paredit")
(add-to-list 'load-path "~/.emacs.d/pig-mode")
(add-to-list 'load-path "~/.emacs.d/popup-el")
(add-to-list 'load-path "~/.emacs.d/rhtml")
(add-to-list 'load-path "~/.emacs.d/smart-compile-plus")
(add-to-list 'load-path "~/.emacs.d/scala-mode2")
(add-to-list 'load-path "~/.emacs.d/wgrep")
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/yasnippet")

;; ********************************************************************************
;; Requires

(require 'cl)
(require 'flymake)
(require 'flymake-ruby)
(require 'wgrep)
(require 'nrepl)
(require 'paredit)
(require 'ac-nrepl)
(require 'magit)
(require 'mouse)

(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)

(setq visible-bell 1)
(setq ring-bell-function (lambda() ()))

(setq solarized-env (getenv "SOLARIZED_MODE"))
(setq solarized-mode (intern (if solarized-env solarized-env "light")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/solarized")
(load-theme  'solarized 't)

(custom-set-variables
 '(help-at-pt-timer-delay 0.9)
 '(help-at-pt-display-when-idle '(flymake-overlay)))

;; ********************************************************************************
;; Emacs variables
;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq case-fold-search t)
(setq enable-recursive-minibuffers nil)
(setq inhibit-startup-screen t)
(setq standard-indent 2)
(setq tags-revert-without-query t)
(setq-default tab-width 4)

;; ********************************************************************************
;; Minor modes
;;
(setq initial-major-mode 'text-mode)

(tool-bar-mode 0)
(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(recentf-mode)
(xterm-mouse-mode t)
(unless (window-system)
  (menu-bar-mode 0))

;; ********************************************************************************
;; Evil
;;
(require 'evil)
(evil-mode 1)

;; ********************************************************************************
;; Helm
;;
(require 'helm-config)
(helm-mode)

;; ********************************************************************************
;; Autocomplete
;;
(require 'auto-complete-config)
(require 'auto-complete-clang)

(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-quick-help-delay 0.1)
(setq ac-auto-start nil)
(global-auto-complete-mode t)

;; ********************************************************************************
;; Yasnippet
;;
(require 'yasnippet)
(yas-global-mode 1)

;; ********************************************************************************
;; Common mode setup
;;
(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(defun init-mode ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'before-save-hook 'untabify-buffer nil t)
  (setq indent-tabs-mode nil)
  (make-local-variable 'tags-file-name))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; ********************************************************************************
;; Ibuffer
;;
(require 'ibuf-ext)
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-enable nil)
(setq ibuffer-expert t)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; ********************************************************************************
;; I-Menu
;;
(setq imenu-auto-rescan t)
(setq imenu-sort-function 'imenu--sort-by-name)

;; ********************************************************************************
;; Tags
;;
(setq tags-add-tables nil)
(setq etags-table-search-up-depth 5)

;; ********************************************************************************
;; Markdown Mode
;;
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))


;; ********************************************************************************
;; Python Mode
;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; ********************************************************************************
;; Octave Mode
;;
;;
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(defun octave-mode-on-init ()
  (init-mode)
  (require 'ac-octave)
  (setq ac-sources '(ac-source-octave)))

(add-hook 'octave-mode-hook 'octave-mode-on-init)

;; ********************************************************************************
;; Scala Mode
;;

(autoload 'scala-mode "scala-mode2" "Scala Mode." t)
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))

(defun scala-mode-on-init ()
  (init-mode)
  (setq ac-sources '(ensime-completions)))

(if (file-exists-p "~/.emacs.d/ensime/dist_2.10.2")
 (progn
   (add-to-list 'load-path "~/.emacs.d/ensime/dist_2.10.2/elisp")
   (require 'ensime)
   (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
   (add-hook 'scala-mode-hook 'scala-mode-on-init)))


;; ********************************************************************************
;; Chuck Mode
;;

(autoload 'chuck-mode "chuck-mode" "Chuck Mode." t)
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))


;; ********************************************************************************
;; Clojure Mode
;;

(autoload 'clojure-mode "clojure-mode" "Clojure Mode." t)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
                 '(add-to-list 'ac-modes 'nrepl-mode))

;; ********************************************************************************
;; Pig Mode
;;

(autoload 'pig-mode "pig-mode" "Pig Mode." t)
(add-to-list 'auto-mode-alist '("\\.pig\\'" . pig-mode))

;; ********************************************************************************
;; Ruby Mode
;;
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ruby/ri-emacs.rb"))
(autoload 'ri "~/.emacs.d/ruby/ri-ruby.el" nil t)
(autoload 'rdebug "rdebug" "ruby Debug" t)

(add-to-list 'auto-mode-alist  '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rake$" . ruby-mode))

(defun ruby-mode-on-init ()
  (init-mode)
  (flymake-ruby-load)

  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)

  (setq ruby-deep-indent-paren nil)
  (setq ruby-compilation-error-regexp "^\\([^: ]+\.rb\\):\\([0-9]+\\):")
  (setq ac-sources '(ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers)))

(add-hook 'ruby-mode-hook 'ruby-mode-on-init) ;

;; ********************************************************************************
;; RHTML Mode
;;
(autoload 'rhtml-mode "rhtml-mode" "RHTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))

(defun rhtml-mode-on-init ()
  (init-mode)
  (abbrev-mode nil))

(add-hook 'rhtml-mode-hook 'rhtml-mode-on-init)


;; ********************************************************************************
;; HAML Mode
;;
(autoload 'haml-mode "haml-mode" "HAML Mode" t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(defun haml-mode-init ()
  (init-mode))

(add-hook 'haml-mode-hook 'haml-mode-on-init)

;; ********************************************************************************
;; YAML Mode
;;
(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; ********************************************************************************
;; AS3 Mode
;;

(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; ********************************************************************************
;; JS2 Mode
;;
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" "JS2 Mode." t)

(setq js2-rebind-eol-bol-keys nil)

(defun js2-mode-on-init ()
  (init-mode)

  (setq ac-sources '(ac-source-semantic
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers))

  (setq js2-allow-keywords-as-property-names nil)
  (setq js2-allow-rhino-new-expr-initializer t)
  (setq js2-basic-offset 2)
  (setq js2-dynamic-idle-timer-adjust 2)
  (setq js2-highlight-level 4)
  (setq js2-idle-timer-delay 5)
  (setq js2-include-browser-externs t)
  (setq js2-include-rhino-externs t)
  (setq js2-language-version 150)
  (setq js2-missing-semi-one-line-override t)
  (setq js2-mode-show-overlay nil)
  (setq js2-mode-show-strict-warnings t)
  (setq js2-mirror-mode t)
  (setq js2-skip-preprocessor-directives t)
  (setq js2-strict-cond-assign-warning t)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-missing-semi-warning t)
  (setq js2-strict-trailing-comma-warning t)
  (setq js2-strict-var-hides-function-arg-warning t)
  (setq js2-strict-var-redeclaration-warning t)

  (define-key js2-mode-map (kbd "<return>") 'reindent-then-newline-and-indent))

(add-hook 'js2-mode-hook 'js2-mode-on-init)


;; ********************************************************************************
;; Haskell Mode
;;
(defun haskell-mode-on-init ()
  (init-mode)
  (flymake-mode)
  (ghc-init)
  (require 'inf-haskell)
  (define-key haskell-mode-map (kbd "M-RET") nil)
  (setq ac-sources '(ac-source-ghc-mod
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers))
  (turn-on-haskell-indentation))

(add-hook 'haskell-mode-hook 'haskell-mode-on-init)

(autoload 'ghc-init "ghc" nil t)
(autoload 'haskell-mode "haskell-mode" "Haskell Mode." t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

(setq haskell-program-name "ghci")


;; ********************************************************************************
;; Emacs-Lisp Mode
;;
(defun emacs-lisp-mode-on-init ()
  (init-mode)

  (setq ac-sources '(ac-source-features
                     ac-source-functions
                     ac-source-symbols
                     ac-source-variables
                     ac-source-words-in-buffer)))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-on-init)


;; ********************************************************************************
;; HTML Mode
;;
(setq auto-mode-alist (remove-if (lambda (item) (string-equal (car item) "\\.html$")) auto-mode-alist))
(add-to-list 'auto-mode-alist  '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist  '("\\.liquid$" . html-mode))

(defun html-mode-on-init ()
  (init-mode))

(add-hook 'html-mode-hook 'html-mode-on-init)


;; ********************************************************************************
;; CSS Mode
;;
(defconst css-imenu-generic-expression
  '((nil "^[ \t]*\\([[:word:].:#, \t]+\\)\\s-*{" 1))
  "Regular expression matching any selector. Used by imenu.")

(defun css-mode-on-init ()
  (init-mode)
  (setq cssm-indent-level 4)
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (set (make-local-variable 'imenu-generic-expression)
       css-imenu-generic-expression))

(add-hook 'css-mode-hook 'css-mode-on-init)

;; ********************************************************************************
;; C Mode
;;
(defun c-mode-on-init ()
  (init-mode)

  (make-local-variable 'standard-indent)
  (setq standard-indent 4)
  (setq ac-sources '(ac-source-clang)))

(add-hook 'c-mode-hook 'c-mode-on-init)

;; ********************************************************************************
;; C++ Mode
;;
(defun c++-mode-init()
  (init-mode)

  (make-local-variable 'standard-indent)
  (setq standard-indent 4)
  (setq ac-sources '(ac-source-clang)))

(add-hook 'c++-mode-hook 'c++-mode-init)

;; ********************************************************************************
;; Objective C
;;
(defun objc-mode-init()
  (init-mode)

  (auto-complete-mode)

  (make-local-variable 'standard-indent)
  (setq standard-indent 4)
  (setq ac-sources '(ac-source-clang)))

(add-hook 'objc-mode-hook 'objc-mode-init)



;; ********************************************************************************
;; Dired refresh hooks
;;

(defadvice switch-to-buffer-other-window (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(defadvice switch-to-buffer (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(defadvice display-buffer (after auto-refresh-dired (buffer &optional not-this-window frame)  activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))

(defadvice other-window (after auto-refresh-dired (arg &optional all-frame) activate)
  (if (equal major-mode 'dired-mode)
      (revert-buffer)))



;; ********************************************************************************
;; Smart Compile
;;
(require 'smart-compile+)

(add-to-list 'smart-compile-alist '("^Rakefile$"  . "rake -f %f")) ;
(add-to-list 'smart-compile-alist '("\\.js$"      . "node %f"))
(add-to-list 'smart-compile-alist '("\\.rb$"      . "ruby %f"))
(add-to-list 'smart-compile-alist '("\\.sc$"      . "scala %f"))
(add-to-list 'smart-compile-alist '("_spec\\.rb$" . "rspec %f"))
(add-to-list 'smart-compile-alist '(haskell-mode  . "ghc -o %n %f")) ;


;; ********************************************************************************
;; Global Key Bindings
;;

(defun insert-newline()
  (interactive)
  (save-excursion
    (end-of-line)
    (newline)))

;; paste with indentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil)))))

(defun git-grep()
  (interactive)
  (require 'grep)
  (require 'vc-git)
  (vc-git-grep (grep-read-regexp) "." (find-git-dir default-directory)))

(defun find-git-dir (dir)
 (let ((f (expand-file-name ".git" dir))
       (parent (file-truename (expand-file-name ".." dir))))
   (cond ((string= dir parent) nil)
         ((file-exists-p f) (expand-file-name dir))
         (t (find-git-dir parent)))))

(defun find-file-in-project ()
  "Prompt with a completing list of all files in the project to find one."
  (interactive)
  (let* ((default-directory (concat (find-git-dir default-directory) "/"))
     (files (split-string (shell-command-to-string "git ls-files")))
     (file (completing-read "Find file in project: " files)))
    (find-file (concat default-directory file))))

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Movement
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)

(global-set-key (kbd "DEL") 'backward-delete-char-untabify)

(define-key evil-motion-state-map (kbd "<up>") 'evil-previous-line)
(define-key evil-motion-state-map (kbd "<down>") 'evil-next-line)
(define-key evil-motion-state-map (kbd "<left>") 'evil-backward-char)
(define-key evil-motion-state-map (kbd "<right>") 'evil-forward-char)
(define-key evil-motion-state-map (kbd "RET") 'insert-newline)

(global-set-key (kbd "ESC <left>") 'evil-prev-buffer)
(global-set-key (kbd "ESC <right>") 'evil-next-buffer)

(global-set-key (kbd "C-c .") 'find-tag)
(global-set-key (kbd "C-c /") 'helm-do-grep)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c c") 'smart-compile)
(global-set-key (kbd "C-c d") 'dired)
(global-set-key (kbd "C-c e") 'helm-c-etags-select)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'git-grep)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c o") 'helm-occur)
(lobal-set-key (kbd "C-c r") 'recompile)

(global-set-key (kbd "M-RET") 'helm-mini)

(put 'dired-find-alternate-file 'disabled nil)
