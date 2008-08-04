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
(require 'vc-svn)
(require 'hippie-exp)
(require 'abbrev)
(require 'snippet)


;; ********************************************************************************
;; Load my own elisp stuff
;;
(load "abbrevs")
(load "browser-help")
(load "buffer-cycle")
(load "completions")
(load "find-tags-file")
(load "imenu-from-pattern")
(load "rinari-find-model")
(load "move-lines")



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
(add-to-list 'load-path "/usr/share/doc/git-core/contrib/emacs")
(require 'vc-git)
(require 'git)
(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)


;; ********************************************************************************
;; RI
(setq ri-ruby-script "~/elisp/ruby/ri-emacs.rb")
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
(add-hook 'ruby-mode-hook
          (lambda()
	    (require 'rinari)
	    (require 'ruby-electric)
	    (require 'inflections)
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
                                  "def \\([[:alnum:].]+\\)"))
            ))



;; ********************************************************************************
;; RHTML Mode
;;
(autoload 'rhtml-mode "rhtml-mode" "RHTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-hook 'rhtml-mode-hook
          (lambda ()
            (abbrev-mode nil)
            (make-local-variable 'tags-file-name)
            (setq local-abbrev-table rhtml-mode-abbrev-table)
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-abbrev
                   try-expand-dabbrev
                   try-expand-tag))
            (define-key rhtml-mode-map (kbd "<tab>") 'indent-and-complete)
            ))

(add-hook 'abbrev-expand-functions
          (lambda (fn)
            (if (rhtml-erb-tag-region)
                ()
              (funcall fn))))



;; ********************************************************************************
;; IRB Mode
;;
(add-hook 'inferior-ruby-mode-hook
          (lambda()
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-abbrev
                   try-expand-dabbrev
                   try-expand-tag))
            (define-key inferior-ruby-mode-map (kbd "<tab>") 'indent-and-complete)
            ))




;; ********************************************************************************
;; JS2 Mode
;;
(setq js2-basic-offset 4)
(setq js2-bounce-indent-flag nil)
(setq js2-highlight-level 4)
(autoload 'js2-mode "js2" "Javascript 2 Mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
          (lambda()
            (make-local-variable 'tags-file-name)
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-abbrev
                   try-expand-dabbrev
                   try-expand-tag))
            (define-key js2-mode-map (kbd "<tab>") 'indent-and-complete)
            (define-key js2-mode-map (kbd "<C-menu>") 
              (imenu-from-pattern "\\b\\([[:alnum:].$]+\\) *[=:] *function" 
                                  "function \\([[:alnum:].]+\\)"))
            ))



;; ********************************************************************************
;; Actionscript Mode
;;
(autoload 'as3-mode "as3-mode" "Actionscript 3 Mode." t)
(add-to-list 'auto-mode-alist '("\\.as$" . as3-mode))
(setenv "CLASSPATH" (concat "/home/matti/elisp/flyparse/lib/flyparse_parsers.jar:" (getenv "CLASSPATH")))
(add-hook 'as3-mode-hook
          (lambda()
            (make-local-variable 'tags-file-name)
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-abbrev
                   try-expand-dabbrev
                   try-expand-tag))
            (define-key as3-mode-map (kbd "<tab>") 'indent-and-complete)
            (define-key as3-mode-map (kbd "<C-menu>") 'javascript-imenu)
            ))



;; ********************************************************************************
;; Emacs-Lips Mode
;;
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-abbrev
                   try-expand-dabbrev
                   try-complete-lisp-symbol))
            (define-key emacs-lisp-mode-map (kbd "<tab>") 'indent-and-complete)
            (define-key emacs-lisp-mode-map (kbd "<C-menu>") 'imenu)
            ))


;; ********************************************************************************
;; HTML Mode
;;
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-hook 'html-mode-hook
          (lambda()
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-abbrev
                   try-expand-dabbrev
                   try-expand-tag))
            (define-key html-mode-map (kbd "<tab>") 'indent-and-complete)))



;; ********************************************************************************
;; CSS Mode
;;
(add-hook 'css-mode-hook
          (lambda()
            (setq cssm-indent-level 4)
            (setq cssm-indent-function #'cssm-c-style-indenter)
            (set (make-local-variable 'hippie-expand-try-functions-list)
                 '(try-expand-css-property 
                   try-expand-dabbrev))
            (define-key cssm-mode-map (kbd "<tab>") 'indent-and-complete)
            (define-key cssm-mode-map (kbd "<C-menu>")
              (imenu-from-pattern "\\(.*\\) \{"))
            ))



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

(add-hook 'ecb-activate-hook
          (lambda ()
            (setq ecb-directories-menu-user-extension 
                  '(("SVN"
                     (ecb-dir-popup-svn-status "Status"))))
            (tree-buffer-defpopup-command ecb-dir-popup-svn-status
              "Check status of directory."
              (svn-status (tree-node->data node))
              (switch-buffer "*svn-status*"))))



;; ********************************************************************************
;; Dired
;;
(setq dired-listing-switches "-l")
(add-hook 'dired-load-hook
          (lambda ()
            (define-key dired-mode-map [return] 'joc-dired-single-buffer)
            (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
            (define-key dired-mode-map (kbd "<C-up>")
              (function
               (lambda nil (interactive) (joc-dired-single-buffer ".."))))))



;; ********************************************************************************
;; Smart Compile
;;
(require 'smart-compile)
(setq smart-compile-alist
      '(("\\.c\\'"              .       "gcc -Wall %f -lm -o %n")
        ("\\.[Cc]+[Pp]*\\'"     .       "g++ -Wall %f -lm -o %n")
        ("\\.java$"             .       "javac %f")
	("_spec\\.rb$"          .       "spec %f")
	("\\.rb$"               .       "ruby %f")
        (emacs-lisp-mode        .       (emacs-lisp-byte-compile))
        (html-mode              .       (browse-url-of-buffer))
        (html-helper-mode       .       (browse-url-of-buffer))
        (haskell-mode           .       "ghc -o %n %f")))


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
(global-set-key (kbd "<f8>") 'flymake-goto-next-error)
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


;; Tags search
(global-set-key (kbd "M-/") 'tags-search)


;; Alacarte Menu
(global-set-key (kbd "<M-menu>") 'alacarte-execute-menu-command)


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
(global-set-key (kbd "C-c e") 'rinari-find-environment)
(global-set-key (kbd "C-c i") 'rinari-find-migration)
(global-set-key (kbd "C-c j") 'rinari-find-javascript)
(global-set-key (kbd "C-c s") 'rinari-find-stylesheet)
(global-set-key (kbd "C-c t") 'rinari-find-test)
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

