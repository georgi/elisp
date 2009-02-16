;; ********************************************************************************
;; Load Path
;;
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/anything-config")
(add-to-list 'load-path "~/.emacs.d/as3-mode")
(add-to-list 'load-path "~/.emacs.d/flyparse")
(add-to-list 'load-path "~/.emacs.d/git")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/mozrepl")
(add-to-list 'load-path "~/.emacs.d/nxml-mode")
(add-to-list 'load-path "~/.emacs.d/rhtml")
(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/semantic")
(add-to-list 'load-path "~/.emacs.d/speedbar")
(add-to-list 'load-path "~/.emacs.d/yasnippet")


;; ********************************************************************************
;; Require libraries
;;
(require 'paren)
(require 'tabbar)
(require 'moz-update)

;; ********************************************************************************
;; Variables
;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq browse-url-browser-function 'browse-url-firefox)
(setq case-fold-search t)
(setq compilation-read-command t)
(setq compilation-window-height nil)
(setq emacs-goodies-el-defaults t)
(setq enable-recursive-minibuffers nil)
(setq inhibit-startup-screen t)
(setq list-directory-verbose-switches "")
(setq next-line-add-newlines nil)
(setq nxml-slash-auto-complete-flag t)
(setq session-initialize t)
(setq standard-indent 2)
(setq use-file-dialog t)
(setq tags-revert-without-query t)
(setq tab-width 4)


;; ********************************************************************************
;; Anything
;;

(require 'anything "anything")
(require 'anything-config)
;; (require 'anything-rcodetools)

(setq anything-sources
      (list anything-c-source-locate
	    anything-c-source-tracker-search))

;; Cua
(cua-mode t)
(setq cua-enable-region-auto-help t)
(setq cua-highlight-region-shift-only nil)

(setq current-language-environment "UTF-8")
(setq dabbrev-abbrev-char-regexp "\\\\sw")
(setq default-input-method "rfc1345")
(setq default-major-mode 'text-mode)

;; Desktop 
(desktop-save-mode t)
(setq desktop-globals-to-save nil)
(setq desktop-load-locked-desktop t)
(setq desktop-save t)

;; Ibuffer
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-enable nil)
(setq ibuffer-expert t)

;; Ido
(ido-mode t)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere nil)
(load "ido-goto-symbol")

;; I-Menu
(setq imenu-auto-rescan t)
(setq imenu-sort-function 'imenu--sort-by-name)

;; Modes
(setq initial-major-mode 'text-mode)
(column-number-mode t)
(mouse-wheel-mode t)
(partial-completion-mode t)
(tool-bar-mode nil)
(global-hl-line-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(tabbar-mode t)

;; Scrolling
(scroll-bar-mode nil)
(setq scroll-conservatively 5)
(setq scroll-step 1)

;; Woman
(setq woman-fill-column 80)
(setq woman-use-own-frame nil)


;; ********************************************************************************
;; Load my own elisp stuff
;;
(load "abbrevs")
(load "browser-help")
(load "completions")
(load "find-tags-file")
(load "menu-from-pattern")
(load "js2")
(load "move-lines")
(load "theme")
(load "window-management")



;; ********************************************************************************
;; Autoloads
;;
(autoload 'worklog "worklog" "Work log" t)
(autoload 'rdebug "rdebug" "Ruby Debug" t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(autoload 'git-status "git" "Git")

;; ********************************************************************************
;; Session
;;
(require 'session)
(setq session-initialize t)
(add-hook 'after-init-hook 'session-initialize)


;; ********************************************************************************
;; Git
;;
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)


;; ********************************************************************************
;; RI
;;
(setq ri-ruby-script (expand-file-name "~/elisp/ruby/ri-emacs.rb"))
(autoload 'ri "~/elisp/ruby/ri-ruby.el" nil t)


;; ********************************************************************************
;; Twitter
;;
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)


;; ********************************************************************************
;; Markdown Mode
;;
(eval-when-compile (require 'markdown-mode))

(defun markdown-mode-on-init ()
  (define-key markdown-mode-map (kbd "<tab>") 'indent-and-complete)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-ispell)))

(add-hook 'markdown-mode-hook 'markdown-mode-on-init)
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; ********************************************************************************
;; Python Mode
;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))



;; ********************************************************************************
;; Ruby Mode
;;

(require 'rinari)
(require 'rinari-extensions)

(setq ruby-compilation-error-regexp "^\\([^: ]+\.rb\\):\\([0-9]+\\):")

(add-to-list 'auto-mode-alist  '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rake$" . ruby-mode))  

(defun ruby-mode-on-init ()
  (require 'ruby-electric)
  (require 'inflections)

  (setq ffip-find-options "-not -regex '.*vendor.*'")

  (rinari-launch)
  (ruby-electric-mode t)

  (make-local-variable 'tags-file-name)

  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-dabbrev-all-buffers
	 try-expand-tag))

  (setq local-abbrev-table ruby-mode-abbrev-table)

  (case (rinari-whats-my-type)
    (:model      (setq local-abbrev-table rails-model-abbrev-table))
    (:controller (setq local-abbrev-table rails-controller-abbrev-table))
    (:functional (setq local-abbrev-table ruby-test-abbrev-table))
    (:unit (setq local-abbrev-table ruby-test-abbrev-table)))

  (define-key ruby-mode-map (kbd "C-c =") 'ruby-xmp-region)
  (define-key ruby-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key ruby-mode-map (kbd "<return>") 'newline-and-indent)
  )

(add-hook 'ruby-mode-hook 'ruby-mode-on-init)


;; ********************************************************************************
;; RHTML Mode
;;
(eval-when-compile (require 'rhtml-mode))

(autoload 'rhtml-mode "rhtml-mode" "RHTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(defun rhtml-mode-on-init ()
  (abbrev-mode nil)
  (make-local-variable 'tags-file-name)
  (set-face-attribute 'erb-delim-face nil :background "#fff")
  (set-face-attribute 'erb-face nil :background "#fff")
  (set-face-attribute 'erb-out-delim-face nil :foreground "#933")
  (setq local-abbrev-table rhtml-mode-abbrev-table)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key rhtml-mode-map (kbd "<tab>") 'indent-and-complete))

(defun abbrev-in-rhtml-mode (fn)
  (if (and (boundp 'rhtml-erb-tag-region) (rhtml-erb-tag-region))
      ()
    (funcall fn)))

(add-hook 'rhtml-mode-hook 'rhtml-mode-on-init)
(add-hook 'abbrev-expand-functions 'abbrev-in-rhtml-mode)


;; ********************************************************************************
;; IRB Mode
;;
(defun inferior-ruby-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key inferior-ruby-mode-map (kbd "<tab>") 'indent-and-complete))

(add-hook 'inferior-ruby-mode-hook 'inferior-ruby-mode-on-init)


;; ********************************************************************************
;; YAML Mode
;;
(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; ********************************************************************************
;; JS2 Mode
;;
(eval-when-compile (require 'js2-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js2-mode-on-init ()
  (make-local-variable 'tags-file-name)
  (moz-minor-mode 1)
  (setq js2-basic-offset 4)
  (setq js2-bounce-indent-flag nil)
  (setq js2-highlight-level 4)
  (setq js2-mode-show-overlay nil)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-javascript-symbol	 
	 try-expand-tag))
  (define-key js2-mode-map (kbd "<tab>") 'indent-and-complete))

(add-hook 'js2-mode-hook 'js2-mode-on-init)



;; ********************************************************************************
;; Actionscript 3 Mode
;;
(eval-when-compile (require 'as3-mode))

(autoload 'as3-mode "as3-mode" "Actionscript 3 Mode." t)
(add-to-list 'auto-mode-alist '("\\.as$" . as3-mode))

(setenv "CLASSPATH" 
	(concat (expand-file-name "~/elisp/flyparse/lib/flyparse_parsers.jar:" )
		(expand-file-name "~/elisp/flyparse/antlr-3.1.jar:")
		(getenv "CLASSPATH")))

(defun as3-mode-on-init ()
  (make-local-variable 'tags-file-name)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key as3-mode-map (kbd "<tab>") 'indent-and-complete))
  
(add-hook 'as3-mode-hook 'as3-mode-on-init)


;; ********************************************************************************
;; Actionscript Mode
;;

(eval-when-compile (require 'actionscript-mode))

(autoload 'actionscript-mode "actionscript-mode" "Actionscript Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

(defun actionscript-mode-on-init ()
  (make-local-variable 'tags-file-name)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key actionscript-mode-map (kbd "<tab>") 'indent-and-complete))
  
(add-hook 'actionscript-mode-hook 'actionscript-mode-on-init)

(load "ani-fcsh")

(setq *fcsh-path* "fcsh")
(setq *fcsh-mxmlc-output-path* "")

(defun compile-mxmlc ()
  (interactive)
  (or *fcsh-compile-active* fcsh-compile)
  (call-interactively 'compile))


;; ********************************************************************************
;; Emacs-Lisp Mode
;;
(defun emacs-lisp-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-complete-lisp-symbol))
  (define-key emacs-lisp-mode-map (kbd "<tab>") 'indent-and-complete))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-on-init)



;; ********************************************************************************
;; HTML Mode
;;
(add-to-list 'auto-mode-alist  '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist  '("\\.liquid$" . html-mode))  

(defun html-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key html-mode-map (kbd "<tab>") 'indent-and-complete))

(add-hook 'html-mode-hook 'html-mode-on-init)


;; ********************************************************************************
;; CSS Mode
;;
(eval-when-compile (require 'css-mode))

(defun css-mode-on-init ()
  (setq cssm-indent-level 4)
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-css-property 
	 try-expand-dabbrev))
  (define-key cssm-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key cssm-mode-map (kbd "<C-menu>") 'show-css-mode))

(add-hook 'css-mode-hook 'css-mode-on-init)


;; ********************************************************************************
;; C Mode
;;
(defun c-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key c-mode-map (kbd "<tab>") 'indent-and-complete))


(add-hook 'c-mode-hook 'c-mode-on-init)



;; ********************************************************************************
;; XML Mode
;;

(setq rng-schema-locating-file-schema-file "~/elisp/nxml-mode/schema/schemas.xml")

(autoload 'nxml-mode "nxml-mode" "XML Mode" t)
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mxml$" . nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))

(defun nxml-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-rng
	 try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key nxml-mode-map (kbd "<tab>") 'indent-and-complete))

(add-hook 'nxml-mode-hook 'nxml-mode-on-init)


;; ********************************************************************************
;; dired
;;
(require 'dired)
(require 'dired-single)
(require 'wdired)
(require 'sunrise-commander)

(defun joc-dired-up-directory()
  (interactive)
  (joc-dired-single-buffer ".."))

(define-key dired-mode-map (kbd "<return>") 'joc-dired-single-buffer)
(define-key dired-mode-map (kbd "<down-mouse-1>") 'joc-dired-single-buffer-mouse)
(define-key dired-mode-map (kbd "<C-up>") 'joc-dired-up-directory)

(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

(setq dired-listing-switches "-l")
(setq dired-omit-files "^\\.")
 



;; ********************************************************************************
;; Smart Compile
;;

(require 'smart-compile)

(add-to-list 'smart-compile-alist '("^Rakefile$"  . "rake -f %f"))
(add-to-list 'smart-compile-alist '("\\.rb$"      . "ruby %f"))
(add-to-list 'smart-compile-alist '("_spec\\.rb$" . "spec %f"))
(add-to-list 'smart-compile-alist '("\\.scm$"     . "scheme %f"))
(add-to-list 'smart-compile-alist '("\\.hx$"      . "haxe compile.hxml"))
(add-to-list 'smart-compile-alist '(haskell-mode  . "ghc -o %n %f"))
(add-to-list 'smart-compile-alist '("\\.mxml$"    . (compile-mxmlc)))
(add-to-list 'smart-compile-alist '("\\.as$"      . (compile-mxmlc)))


;; ********************************************************************************
;; Speedbar
(require 'sr-speedbar)

(setq speedbar-show-unknown-files t)

;; ********************************************************************************
;; Toggle fullscreen

(defun toggle-fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun tab-label (tab)
  (if tabbar--buffer-show-groups
      (format " [%s] " (tabbar-tab-tabset tab))
    (format " %s " (tabbar-tab-value tab))))

(setq tabbar-tab-label-function 'tab-label)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; ********************************************************************************
;; Global Key Bindings
;;

;; F keys
(global-set-key (kbd "<f1>") 'eshell)
(global-set-key (kbd "<f2>") 'grep)
(global-set-key (kbd "<f5>") 'joc-dired-magic-buffer)
(global-set-key (kbd "<f6>") 'ibuffer)
(global-set-key (kbd "<f7>") 'svn-status)
(global-set-key (kbd "<f8>") 'git-status)
(global-set-key (kbd "<f9>") 'smart-compile)
(global-set-key (kbd "<f10>") 'sr-speedbar-toggle)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<f12>") 'indent-buffer)

;; Help keys
(global-set-key (kbd "C-h C-d") 'dictionary-lookup)
(global-set-key (kbd "C-h C-h") 'html-help)
(global-set-key (kbd "C-h C-j") 'javascript-help)
(global-set-key (kbd "C-h C-r") 'ruby-help)
(global-set-key (kbd "C-h C-e") 'rails-help)


;; Window management
(require 'windmove)

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)

(global-set-key (kbd "ESC <up>") 'windmove-up)
(global-set-key (kbd "ESC <down>") 'windmove-down)
(global-set-key (kbd "ESC <left>") 'windmove-left)
(global-set-key (kbd "ESC <right>") 'windmove-right)

(global-set-key (kbd "<s-up>") 'window-resize-up)
(global-set-key (kbd "<s-down>") 'window-resize-down)

(defun kill-current-buffer()
  (interactive) 
  (kill-buffer (buffer-name)))

(global-set-key (kbd "M-k") 'kill-current-buffer)
(global-set-key (kbd "M-b") 'bury-buffer)

;; Tags search
(global-set-key (kbd "M-/") 'tags-search)

;; Menubar
(global-set-key (kbd "<M-menu>") 'menu-bar-mode)

;; Buffers
(global-set-key (kbd "<menu>") 'ido-switch-buffer)
(global-set-key (kbd "<apps>") 'ido-switch-buffer)
(global-set-key (kbd "<C-menu>") 'ido-goto-symbol)
(global-set-key (kbd "<C-apps>") 'ido-goto-symbol)

(global-set-key (kbd "<C-M-prior>") 'tabbar-backward-group)
(global-set-key (kbd "<C-M-next>") 'tabbar-forward-group)
(global-set-key (kbd "<C-prior>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-next>") 'tabbar-forward-tab)

(global-set-key (kbd "<C-M-up>") 'move-line-up)
(global-set-key (kbd "<C-M-down>") 'move-line-down)

(global-set-key (kbd "<M-S-up>") 'copy-line-up)
(global-set-key (kbd "<M-S-down>") 'copy-line-down)

(global-set-key (kbd "<M-SPC>") 'anything)

;; Rinari
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c m") 'rinari-find-model)
(global-set-key (kbd "C-c v") 'rinari-find-view-or-select)
(global-set-key (kbd "C-c c") 'rinari-find-controller-or-select)
(global-set-key (kbd "C-c j") 'rinari-find-javascript)
(global-set-key (kbd "C-c s") 'rinari-find-stylesheet)
(global-set-key (kbd "C-c t") 'rinari-find-test)
(global-set-key (kbd "C-c C-g m") (rinari-script-key-hook "generate" "model"))
(global-set-key (kbd "C-c C-g i") (rinari-script-key-hook "generate" "migration"))
(global-set-key (kbd "C-c C-g c") (rinari-script-key-hook "generate" "controller"))
(global-set-key (kbd "C-c C-g a") (rinari-script-key-hook "generate" "mailer"))
(global-set-key (kbd "C-c C-g o") (rinari-script-key-hook "generate" "observer"))
(global-set-key (kbd "C-c C-g r") (rinari-script-key-hook "generate" "resource"))
(global-set-key (kbd "C-c C-d m") (rinari-script-key-hook "destroy" "model"))
(global-set-key (kbd "C-c C-d i") (rinari-script-key-hook "destroy" "migration"))
(global-set-key (kbd "C-c C-d c") (rinari-script-key-hook "destroy" "controller"))
(global-set-key (kbd "C-c C-d a") (rinari-script-key-hook "destroy" "mailer"))
(global-set-key (kbd "C-c C-d o") (rinari-script-key-hook "destroy" "observer"))
(global-set-key (kbd "C-c C-d r") (rinari-script-key-hook "destroy" "resource"))
(global-set-key (kbd "C-x b") 'rinari-browse-url)
(global-set-key (kbd "C-x t") 'rinari-test)
(global-set-key (kbd "C-x r") 'rinari-rake)
(global-set-key (kbd "C-x c") 'rinari-console)
(global-set-key (kbd "C-x s") 'rinari-sql)
(global-set-key (kbd "C-x w") 'rinari-web-server)
(global-set-key (kbd "C-x b") 'rinari-browse-url)
(global-set-key (kbd "C-x g") 'rinari-rgrep)


(defvar current-font-size 8)
(defvar current-font-name)

(setq current-font-name "Monospace")

(defun set-font-size()
  (set-frame-font (format "%s-%s" current-font-name current-font-size)))
  
(defun decrease-font-size()
  (interactive)
  (decf current-font-size)
  (set-font-size))

(defun increase-font-size()
  (interactive)
  (incf current-font-size)
  (set-font-size))

(global-set-key (kbd "C--") 'decrease-font-size)
(global-set-key (kbd "C-=") 'increase-font-size)

(set-font-size)


