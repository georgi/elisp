;; ********************************************************************************
;; Recursive Load Path
;;
(let* ((dir "~/elisp/")
       (default-directory dir))
  (setq load-path (cons dir load-path))
  (normal-top-level-add-subdirs-to-load-path))


;; ********************************************************************************
;; Require libraries
;;
(require 'alacarte)
(require 'find-recursive)
(require 'windmove)
(require 'dired-single)
(require 'paren)
(require 'git)
(require 'vc-svn)
(require 'vc-git)
(require 'hippie-exp)
(require 'abbrev)
(require 'snippet)
(require 'etags)
(require 'rinari)
(require 'ruby-electric)
(require 'inflections)


;; ********************************************************************************
;; Load my own elisp stuff
;;
(load "abbrevs")
(load "browser-help")
(load "buffer-cycle")
(load "completions")
(load "find-tags-file")
(load "imenu-from-pattern")
(load "rinari-extensions")
(load "move-lines")
(load "window-management")



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

;; Rinari
(setq ffip-find-options "-not -regex '.*vendor.*'")

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
(setq ibuffer-elide-long-columns t)
(setq ibuffer-enable nil)
(setq ibuffer-expert t)

;; Ido
(ido-mode t)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere nil)

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

;; Scrolling
(scroll-bar-mode nil)
(setq scroll-conservatively 5)
(setq scroll-step 1)

;; Woman
(setq woman-fill-column 80)
(setq woman-use-own-frame nil)

;; ********************************************************************************
;; Autoloads
;;
(autoload 'worklog "worklog" "Work log" t)
(autoload 'rdebug "rdebug" "Ruby Debug" t)


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
(setq ri-ruby-script (expand-file-name "~/elisp/ruby/ri-emacs.rb"))
(autoload 'ri "~/elisp/ruby/ri-ruby.el" nil t)


;; ********************************************************************************
;; Python Mode
;;
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))



;; ********************************************************************************
;; Ruby Mode
;;
(add-to-list 'auto-mode-alist  '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rake$" . ruby-mode))  

(defun ruby-mode-on-init ()
  (rinari-launch)
  (ruby-electric-mode t)
  (make-local-variable 'tags-file-name)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (setq local-abbrev-table ruby-mode-abbrev-table)
  (case (rinari-whats-my-type)
    (:model      (setq local-abbrev-table rails-model-abbrev-table))
    (:controller (setq local-abbrev-table rails-controller-abbrev-table))
    (:functional (setq local-abbrev-table ruby-test-abbrev-table))
    (:unit (setq local-abbrev-table ruby-test-abbrev-table)))
  (define-key ruby-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key ruby-mode-map (kbd "<C-menu>")
    (imenu-from-pattern "class \\([[:alnum:].]+\\)"
			"def \\([[:alnum:].]+\\)")))

(add-hook 'ruby-mode-hook 'ruby-mode-on-init)


;; ********************************************************************************
;; RHTML Mode
;;
(autoload 'rhtml-mode "rhtml-mode" "RHTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))

(defun rhtml-mode-on-init ()
  (abbrev-mode nil)
  (make-local-variable 'tags-file-name)
  (setq local-abbrev-table rhtml-mode-abbrev-table)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key rhtml-mode-map (kbd "<tab>") 'indent-and-complete))

(defun abbrev-in-rhtml-mode (fn)
  (if (rhtml-erb-tag-region)
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
;; JS2 Mode
;;
(setq js2-basic-offset 4)
(setq js2-bounce-indent-flag nil)
(setq js2-highlight-level 4)
(setq js2-mode-show-overlay nil)

(autoload 'js2-mode "js2" "Javascript 2 Mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js2-mode-on-init ()
  (make-local-variable 'tags-file-name)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev	 
	 try-expand-javascript-symbol	 
	 try-expand-tag))
  (define-key js2-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key js2-mode-map (kbd "<C-menu>") 
    (imenu-from-pattern "\\b\\([[:alnum:].$]+\\) *[=:] *function" 
			"function \\([[:alnum:].]+\\)")))

(add-hook 'js2-mode-hook 'js2-mode-on-init)


;; ********************************************************************************
;; Actionscript Mode
;;
(autoload 'as3-mode "as3-mode" "Actionscript 3 Mode." t)
(add-to-list 'auto-mode-alist '("\\.as$" . as3-mode))

(setenv "CLASSPATH" 
	(concat (expand-file-name "~/elisp/flyparse/lib/flyparse_parsers.jar:" )
		(getenv "CLASSPATH")))

(defun as3-mode-on-init ()
  (make-local-variable 'tags-file-name)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key as3-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key as3-mode-map (kbd "<C-menu>") 'javascript-imenu))
  
(add-hook 'as3-mode-hook 'as3-mode-on-init)



;; ********************************************************************************
;; Emacs-Lips Mode
;;
(defun emacs-lisp-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-complete-lisp-symbol))
  (define-key emacs-lisp-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key emacs-lisp-mode-map (kbd "<C-menu>") 'imenu))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-on-init)



;; ********************************************************************************
;; HTML Mode
;;
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

(defun html-mode-on-init ()
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-tag))
  (define-key html-mode-map (kbd "<tab>") 'indent-and-complete))



;; ********************************************************************************
;; CSS Mode
;;
(defun css-mode-on-init ()
  (setq cssm-indent-level 4)
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-css-property 
	 try-expand-dabbrev))
  (define-key cssm-mode-map (kbd "<tab>") 'indent-and-complete)
  (define-key cssm-mode-map (kbd "<C-menu>")
    (imenu-from-pattern "\\(.*\\) \{")))

(add-hook 'css-mode-hook 'css-mode-on-init)



;; ********************************************************************************
;; XML Mode
;;
(autoload 'nxml-mode "xml-mode" "XML Mode" t)
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))



;; ********************************************************************************
;; ECB
;;
(setq ecb-expand-methods-switch-off-auto-expand nil)
(setq ecb-history-sort-method nil)
(setq ecb-kill-buffer-clears-history (quote auto))
(setq ecb-layout-name "leftright2")
(setq ecb-layout-window-sizes nil)
(setq ecb-options-version "2.32")
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
(setq ecb-show-sources-in-directories-buffer (quote never))
(setq ecb-sources-perform-read-only-check 'unless-remote)
(setq ecb-tip-of-the-day nil)
(setq ecb-use-speedbar-instead-native-tree-buffer nil)
(setq ecb-windows-width 0.2)

(defun ecb-on-activate ()
  (setq ecb-directories-menu-user-extension 
	'(("SVN"
	   (ecb-dir-popup-svn-status "Status"))
	  ("Git"
	   (ecb-dir-popup-git-status "Status"))))

  (tree-buffer-defpopup-command ecb-dir-popup-svn-status
    "Check status of directory."
    (svn-status (tree-node->data node))
    (switch-to-buffer "*svn-status*"))

  (tree-buffer-defpopup-command ecb-dir-popup-git-status
    "Check status of directory."
    (git-status (tree-node->data node))
    (switch-to-buffer "*git-status*")))

(add-hook 'ecb-activate-hook 'ecb-on-activate)



;; ********************************************************************************
;; Dired
;;
(setq dired-listing-switches "-l")

(defun joc-dired-up-directory()
  (interactive)
  (joc-dired-single-buffer ".."))

(defun dired-on-load ()
  (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "<C-up>") 'joc-dired-up-directory))

(add-hook 'dired-load-hook 'dired-on-load)



;; ********************************************************************************
;; Smart Compile
;;
(require 'smart-compile)
(add-to-list 'smart-compile-alist '("^Rakefile$"  . "rake -f %f"))
(add-to-list 'smart-compile-alist '("_spec\\.rb$" . "spec %f"))
(add-to-list 'smart-compile-alist '(haskell-mode  . "ghc -o %n %f"))



;; ********************************************************************************
;; Toggle fullscreen in X11

(defun toggle-fullscreen ()
  "Toggle between fullscreen and partial screen display on X11"
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))



;; ********************************************************************************
;; Global Key Bindings
;;

;; F keys
(global-set-key (kbd "<f1>") 'ri)
(global-set-key (kbd "<f2>") 'grep)
(global-set-key (kbd "<f5>") 'joc-dired-magic-buffer)
(global-set-key (kbd "<f6>") 'ibuffer)
(global-set-key (kbd "<f7>") 'svn-status)
(global-set-key (kbd "<f8>") 'git-status)
(global-set-key (kbd "<f9>") 'smart-compile)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; Help keys
(global-set-key (kbd "C-h C-h") 'html-help)
(global-set-key (kbd "C-h C-j") 'javascript-help)
(global-set-key (kbd "C-h C-r") 'ruby-help)
(global-set-key (kbd "C-h C-e") 'rails-help)


;; Window management
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)
(global-set-key (kbd "<M-left>") 'windmove-left)
(global-set-key (kbd "<M-right>") 'windmove-right)
(global-set-key (kbd "<s-up>") 'window-resize-up)
(global-set-key (kbd "<s-down>") 'window-resize-down)


;; Tags search
(global-set-key (kbd "M-/") 'tags-search)

;; Menubar
(global-set-key (kbd "<M-menu>") 'menu-bar-open)

;; Buffers
(global-set-key (kbd "<menu>") 'ido-switch-buffer)
(global-set-key (kbd "C-w") (lambda () (interactive) (kill-buffer (buffer-name))))

(global-set-key (kbd "<C-prior>") 'cycle-buffer-prev)
(global-set-key (kbd "<C-next>") 'cycle-buffer-next)

(global-set-key (kbd "<C-M-up>") 'move-line-up)
(global-set-key (kbd "<C-M-down>") 'move-line-down)

(global-set-key (kbd "<M-S-up>") 'copy-line-up)
(global-set-key (kbd "<M-S-down>") 'copy-line-down)

;; Rinari
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c m") 'rinari-find-model)
(global-set-key (kbd "C-c v") 'rinari-find-view)
(global-set-key (kbd "C-c c") 'rinari-find-controller)
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



;; ********************************************************************************
;; Start emacs server
(toggle-fullscreen)
(server-start)

