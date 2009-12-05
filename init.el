;; ********************************************************************************
;; Load Path
;;
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/cedet")
(add-to-list 'load-path "~/.emacs.d/ecb")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/mozrepl")
(add-to-list 'load-path "~/.emacs.d/nxml-mode")
(add-to-list 'load-path "~/.emacs.d/rhtml")
(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/python")

;; ********************************************************************************
;; Variables
;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; (setq browse-url-browser-function 'browse-url-firefox)
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
(setq tooltip-delay 0)

;; ********************************************************************************
;; CEDET.
(load-file "~/.emacs.d/cedet/common/cedet.el")

(global-ede-mode 1)

;; ********************************************************************************
;; Semantic
(semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers)
;; (semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-all-exuberent-ctags-support)


(require 'ecb)
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
(setq ecb-history-make-buckets 'mode)
(setq ecb-add-path-for-not-matching-files (quote (nil)))
(setq ecb-auto-activate nil)
(setq ecb-clear-caches-before-activate t)
(setq ecb-compile-window-height 20)
(setq ecb-options-version "2.40")
(setq ecb-tip-of-the-day nil)
(setq ecb-vc-supported-backends (quote ((ecb-vc-dir-managed-by-SVN . ecb-vc-state) (ecb-vc-dir-managed-by-GIT . ecb-vc-state))))


(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)



;; ********************************************************************************
;; Anything
;;

(require 'anything "anything")
(require 'anything-config)

(setq anything-sources
      (list anything-c-source-complex-command-history
	    anything-c-source-emacs-commands))

;; Cua
(cua-mode t)
(setq cua-enable-region-auto-help t)
(setq cua-highlight-region-shift-only nil)

(setq current-language-environment "UTF-8")
(setq dabbrev-abbrev-char-regexp "\\\\sw")
(setq default-input-method "rfc1345")
(setq default-major-mode 'text-mode)

;; Desktop 
;; (desktop-save-mode t)
;; (setq desktop-globals-to-save nil)
;; (setq desktop-load-locked-desktop t)
;; (setq desktop-save t)

;; Ibuffer
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-enable nil)
(setq ibuffer-expert t)

;; Ido
(ido-mode t)
(setq ido-case-fold t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere nil)
(setq ido-enable-tramp-completion nil)
(setq ido-separator "  ")
(setq ido-use-filename-at-point (quote guess))

(load "ido-goto-symbol")

;; I-Menu
(setq imenu-auto-rescan t)
(setq imenu-sort-function 'imenu--sort-by-name)

;; Modes
(setq initial-major-mode 'text-mode)
(column-number-mode t)
(mouse-wheel-mode t)
(partial-completion-mode t)
(menu-bar-mode nil)
(tool-bar-mode nil)
(menu-bar-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(transient-mark-mode t)

(set-scroll-bar-mode 'right)

(setq scroll-conservatively 5)
(setq scroll-step 1)

;; Woman
(setq woman-fill-column 80)
(setq woman-use-own-frame nil)


;; ********************************************************************************
;; Tabbar

(require 'tabbar)

(tabbar-mode t)

(defun tab-label (tab)
  (if tabbar--buffer-show-groups
      (format " [%s] " (tabbar-tab-tabset tab))
    (format " %s " (tabbar-tab-value tab))))

(setq tabbar-tab-label-function 'tab-label)

(remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)



;; ********************************************************************************
;; Load my own elisp stuff
;;
(require 'ruby-mode)

(load "abbrevs")
(load "browser-help")
(load "completions")
(load "find-tags-file")
(load "menu-from-pattern")
(load "move-lines")
(load "theme")
(load "window-management")



;; ********************************************************************************
;; Autoloads
;;
(autoload 'worklog "worklog" "work log" t)
(autoload 'rdebug "rdebug" "ruby Debug" t)
(autoload 'magit-status "magit" "magit" t)


;; ********************************************************************************
;; Tags
;;
(require 'etags-table)
(require 'etags-select)

(setq tags-add-tables nil)
(setq etags-table-search-up-depth 5)
(setq etags-table-alist
      (list
       '(".*\\.rb$" "/usr/lib/ruby/1.8/TAGS")
       '(".*\\.rb$" "/usr/lib/ruby/gems/1.8/gems")
       ))


;; ********************************************************************************
;; Mozilla
;;
;; (require 'moz-update)

;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)


;; ********************************************************************************
;; Session
;;
(require 'session)

(setq session-initialize t)
(add-hook 'after-init-hook 'session-initialize)


;; ********************************************************************************
;; Org Mode
;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; ********************************************************************************
;; RI
;;
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ruby/ri-emacs.rb"))
(autoload 'ri "~/.emacs.d/ruby/ri-ruby.el" nil t)


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
;; Chuck Mode
;;

(autoload 'chuck-mode "chuck-mode" "Chuck Mode." t)
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))



;; ********************************************************************************
;; Ruby Mode
;;

(require 'rinari)
(require 'rinari-extensions)

(setq ruby-compilation-error-regexp "^\\([^: ]+\.rb\\):\\([0-9]+\\):")

;; (setq ruby-deep-indent-paren t)

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

  (setq toggle-mapping-style 'rspec)

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
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))

(defun rhtml-mode-on-init ()
  (abbrev-mode nil)
  (make-local-variable 'tags-file-name)
  (set-face-attribute 'erb-delim-face nil :background "#fff")
  (set-face-attribute 'erb-face nil :background "#fff")
  (set-face-attribute 'erb-out-delim-face nil :foreground "#933")
  (setq local-abbrev-table rhtml-mode-abbrev-table)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
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
;; HAML Mode
;;
(autoload 'haml-mode "haml-mode" "HAML Mode." t)

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))


;; ********************************************************************************
;; JS2 Mode
;;
(eval-when-compile (require 'js2-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-mode-dev-mode-p t)
(setq js2-mode-must-byte-compile nil)

(defun js2-mode-on-init ()
  (make-local-variable 'tags-file-name)
  (setq js2-allow-keywords-as-property-names nil)
  (setq js2-allow-rhino-new-expr-initializer t)
  (setq js2-basic-offset 4)
  (setq js2-bounce-indent-flag nil)
  (setq js2-dynamic-idle-timer-adjust 2)
  (setq js2-highlight-level 4)
  (setq js2-idle-timer-delay 5)
  (setq js2-include-browser-externs t)
  (setq js2-include-rhino-externs t)
  (setq js2-language-version 150)
  (setq js2-missing-semi-one-line-override t)
  (setq js2-mode-show-overlay nil)
  (setq js2-mode-show-strict-warnings t)
  (setq js2-skip-preprocessor-directives t)
  (setq js2-strict-cond-assign-warning t)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-missing-semi-warning t)
  (setq js2-strict-trailing-comma-warning t)
  (setq js2-strict-var-hides-function-arg-warning t)
  (setq js2-strict-var-redeclaration-warning t)

  (set (make-local-variable 'hippie-expand-try-functions-list)
       '(try-expand-abbrev
	 try-expand-dabbrev
	 try-expand-javascript-symbol	 
	 try-expand-tag))
  (define-key js2-mode-map (kbd "<tab>") 'indent-and-complete))

(add-hook 'js2-mode-hook 'js2-mode-on-init)



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
(eval-when-compile (require 'regexp-opt))

(defconst css-imenu-generic-expression
  '((nil "^[ \t]*\\([[:word:].:#, \t]+\\)\\s-*{" 1))
  "Regular expression matching any selector. Used by imenu.")

(defun css-mode-on-init ()
  (setq cssm-indent-level 4)
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (set (make-local-variable 'imenu-generic-expression)
       css-imenu-generic-expression)
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

(setq rng-schema-locating-file-schema-file "/home/matti/.emacs.d/nxml-mode/schema/schemas.xml")

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
(setq speedbar-use-images t)


;; ********************************************************************************
;; Toggle fullscreen

(defun toggle-fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))



(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; ********************************************************************************
;; Global Key Bindings
;;

;; Function keys
(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "<f2>") 'rgrep)
(global-set-key (kbd "<f5>") 'joc-dired-magic-buffer)
(global-set-key (kbd "<f6>") 'ibuffer)
(global-set-key (kbd "<f7>") 'create-tags)
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "<f9>") 'svn-status)
(global-set-key (kbd "<f10>") 'smart-compile)
(global-set-key (kbd "<f11>") 'ecb-toggle-compile-window)
(global-set-key (kbd "<f12>") 'ecb-toggle-ecb-windows)

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

(defun save-and-exit()
  (interactive)
  (save-buffer)
  (save-buffers-kill-terminal))

(global-set-key (kbd "C-x C-c") 'save-and-exit)


(defun kill-current-buffer()
  (interactive) 
  (kill-buffer (buffer-name)))

(global-set-key (kbd "H-k") 'kill-current-buffer)
(global-set-key (kbd "H-b") 'bury-buffer)

(global-set-key (kbd "M-s") 'sort-lines)

;; Tags
(global-set-key (kbd "M-/") 'tags-search)
(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-.") 'etags-select-find-tag)

;; Hyper Mapping
(global-set-key (kbd "H-/") 'tags-search)
(global-set-key (kbd "H-?") 'etags-select-find-tag-at-point)
(global-set-key (kbd "H-.") 'etags-select-find-tag)
(global-set-key (kbd "H-;") 'comment-dwim)
(global-set-key (kbd "H-a") 'align-string)
(global-set-key (kbd "H-s") 'save-buffer)
(global-set-key (kbd "H-z") 'undo)
(global-set-key (kbd "H-c") 'copy-region-as-kill)
(global-set-key (kbd "H-v") 'yank)
(global-set-key (kbd "H-g") 'goto-line)

(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-prior>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-next>") 'tabbar-forward-tab)


(require 'magit)
(define-key magit-mode-map (kbd "M-1") nil)
(define-key magit-mode-map (kbd "M-2") nil)
(define-key magit-mode-map (kbd "M-3") nil)
(define-key magit-mode-map (kbd "M-4") nil)


(global-set-key (kbd "<C-M-up>") 'move-line-up)
(global-set-key (kbd "<C-M-down>") 'move-line-down)

(global-set-key (kbd "<M-S-up>") 'copy-line-up)
(global-set-key (kbd "<M-S-down>") 'copy-line-down)

(global-set-key (kbd "<C-delete>") 'kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "<M-SPC>") 'ido-switch-buffer)
(global-set-key (kbd "<H-M-SPC>") 'find-file-in-project)

(global-set-key (kbd "M-n")  (lambda () (interactive) (scroll-up   40)))
(global-set-key (kbd "M-p")  (lambda () (interactive) (scroll-down 40)))

(global-set-key (kbd "C-c r") 'revert-buffer)

(define-key isearch-mode-map (kbd "C-o") 
  (lambda () (interactive) 
    (let ((case-fold-search isearch-case-fold-search)) 
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;; Rinari
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c m") 'rinari-find-model)
(global-set-key (kbd "C-c v") 'rinari-find-view-or-select)
(global-set-key (kbd "C-c c") 'rinari-find-controller-or-select)
(global-set-key (kbd "C-c j") 'rinari-find-javascript)
(global-set-key (kbd "C-c s") 'rinari-find-stylesheet)
(global-set-key (kbd "C-c t") 'rinari-find-test)
(global-set-key (kbd "C-x t") 'rinari-test)
(global-set-key (kbd "C-x r") 'rinari-rake)
(global-set-key (kbd "C-x c") 'rinari-console)
(global-set-key (kbd "C-x s") 'rinari-sql)
(global-set-key (kbd "C-x w") 'rinari-web-server)
(global-set-key (kbd "C-x g") 'rinari-rgrep)

(global-set-key (kbd "C-c o") 'toggle-buffer)
(global-set-key (kbd "C-c i") 'indent-buffer)

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (comint-send-string (inferior-moz-process)
                                      "BrowserReload();")))

(defvar current-font-size 80)

(defun decrease-font-size()
  (interactive)
  (setq current-font-size (- current-font-size 10))
  (set-face-attribute 'default nil :height current-font-size))

(defun increase-font-size()
  (interactive)
  (setq current-font-size (+ current-font-size 10))
  (set-face-attribute 'default nil :height current-font-size))


(global-set-key (kbd "C--") 'decrease-font-size)
(global-set-key (kbd "C-=") 'increase-font-size)

(set-face-attribute 'default nil :height current-font-size)
