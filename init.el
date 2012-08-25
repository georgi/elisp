;; ********************************************************************************
;; Load Path
;;
(add-to-list 'load-path "~/.emacs.d")
;; (add-to-list 'load-path "~/.emacs.d/apel")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(add-to-list 'load-path "~/.emacs.d/erlang")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/expand-region")
(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/haskell-mode")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/mark-multiple")
(add-to-list 'load-path "~/.emacs.d/nxml-mode")
(add-to-list 'load-path "~/.emacs.d/rhtml")
(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/python-mode")
(add-to-list 'load-path "~/.emacs.d/yasnippet")

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

;; ********************************************************************************
;; Requires

(require 'evil)
(require 'cl)
(require 'toggle)
(require 'helm-config)
(require 'find-file-in-project)
(require 'session)
(require 'smart-compile)
(require 'flymake)
(require 'flymake-cursor)
(require 'color-theme-solarized)
(require 'expand-region)
(require 'rvm)
(require 'magit)
(require 'wgrep)
(require 'go-mode-load)

;; ********************************************************************************
;; Color theme
;;
(set-face-background 'header-line nil)
(set-face-background 'flymake-errline "#ffcccc")

(setq solarized-contrast 'high)
(color-theme-solarized-light)

(setq visible-bell 1)
(setq ring-bell-function (lambda() ()))

;; ********************************************************************************
;; Emacs variables
;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq case-fold-search t)
(setq enable-recursive-minibuffers nil)
(setq inhibit-startup-screen t)
(setq session-initialize t)
(setq standard-indent 2)
(setq tags-revert-without-query t)
(setq-default tab-width 4)
(setq icicle-Completions-text-scale-decrease 0)


;; ********************************************************************************
;; Minor modes
;;
(setq initial-major-mode 'text-mode)

(evil-mode 1)
(tool-bar-mode 0)
(helm-mode)
(column-number-mode t)
(show-paren-mode t)
(transient-mark-mode t)
(recentf-mode)

(if (boundp 'mouse-wheel-mode)
	(mouse-wheel-mode t))

(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode 0))

(unless (window-system)
  (menu-bar-mode 0))

(setq-default mode-line-format nil)

;; ********************************************************************************
;; Autocomplete
;;
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)

(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)
(global-auto-complete-mode t))

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
;; Desktop
;;
(desktop-save-mode t)
(setq desktop-globals-to-save nil)
(setq desktop-load-locked-desktop t)
(setq desktop-save t)

;; ********************************************************************************
;; Ibuffer
;;
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-enable nil)
(setq ibuffer-expert t)

;; ********************************************************************************
;; I-Menu
;;
(setq imenu-auto-rescan t)
(setq imenu-sort-function 'imenu--sort-by-name)

;; ********************************************************************************
;; Tags
;;
(load "find-tags-file")
(setq tags-add-tables nil)
(setq etags-table-search-up-depth 5)


;; ********************************************************************************
;; Session
;;
(setq session-initialize t)
(add-hook 'after-init-hook 'session-initialize)

;; ********************************************************************************
;; Erlang Mode
;;
(autoload 'erlang-mode "erlang-mode.el" "Major mode for editing erlang files" t)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws\\'" . erlang-mode))

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
;; Chuck Mode
;;

(autoload 'chuck-mode "chuck-mode" "Chuck Mode." t)
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))


;; ********************************************************************************
;; Ruby Mode
;;

(rvm-use-default)

(setq ri-ruby-script (expand-file-name "~/.emacs.d/ruby/ri-emacs.rb"))
(autoload 'ri "~/.emacs.d/ruby/ri-ruby.el" nil t)
(autoload 'rdebug "rdebug" "ruby Debug" t)

(add-to-list 'auto-mode-alist  '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rake$" . ruby-mode))

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(setq toggle-mapping-styles
      '((rspec
		 . (("app/models/\\1.rb"      . "spec/models/\\1_spec.rb")
			("app/controllers/\\1.rb" . "spec/controllers/\\1_spec.rb")
			("app/views/\\1.rb"       . "spec/views/\\1_spec.rb")
			("app/helpers/\\1.rb"     . "spec/helpers/\\1_spec.rb")
			("app/processors/\\1.rb"  . "spec/processors/\\1_spec.rb")
			("app/resources/\\1.rb"   . "spec/resources/\\1_spec.rb")
			("app/services/\\1.rb"   . "spec/services/\\1_spec.rb")
			("spec/lib/\\1_spec.rb"   . "lib/\\1.rb")))
		(ruby
		 . (("lib/\\1.rb"             . "test/test_\\1.rb")
			("\\1.rb"                 . "test_\\1.rb")))))

(defun spec-run-single-file (spec-file &rest opts)
  "Runs spec with the specified options"
  (compile (concat (first rvm--current-ruby-binary-path) "ruby ~/.emacs.d/spin " spec-file " " (mapconcat (lambda (x) x) opts " ")))
  (end-of-buffer-other-window 0))

(defun spec-verify ()
  "Runs the specified spec for the current buffer."
  (interactive)
  (spec-run-single-file (buffer-file-name) "--format" "nested"))

(defun spec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (spec-run-single-file (buffer-file-name) "--format" "nested" "--line " (number-to-string (line-number-at-pos))))

(defun ruby-mode-on-init ()
  (init-mode)
  (flymake-mode)

  (setq ruby-deep-indent-paren nil)
  (setq ruby-compilation-error-regexp "^\\([^: ]+\.rb\\):\\([0-9]+\\):")

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers)))

(add-hook 'ruby-mode-hook 'ruby-mode-on-init) ;

(defun ruby-imenu-create-index-in-block (prefix beg end)
  (let ((index-alist '()) (case-fold-search nil)
        name next pos decl sing)
    (goto-char beg)
    (while (re-search-forward "^\\s *\\(\\(class\\s +\\|\\(class\\s *<<\\s *\\)\\|module\\s +\\)\\([^\(<\n ]+\\)\\|\\(def\\|alias\\)\\s +\\([^\(\n ]+\\)\\)" end t)
      (setq sing (match-beginning 3))
      (setq decl (match-string 5))
      (setq next (match-end 0))
      (setq name (or (match-string 4) (match-string 6)))
      (setq pos (match-beginning 0))
      (cond
       ((string= "alias" decl)
        (push (cons name pos) index-alist))
       ((string= "def" decl)
        (push (cons name pos) index-alist)
        (ruby-accurate-end-of-block end))
       (t
        (ruby-accurate-end-of-block end)
        (setq beg (point))
        (setq index-alist
              (nconc (ruby-imenu-create-index-in-block
                      (concat name (if sing "." "#"))
                      next beg) index-alist))
        (goto-char beg))))
    index-alist))

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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" "JS2 Mode." t)

(setq js2-mode-dev-mode-p t)
(setq js2-mode-must-byte-compile nil)
(setq js2-rebind-eol-bol-keys nil)

(defun js2-mode-on-init ()
  (init-mode)

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-semantic
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
  (init-mode))

(add-hook 'haskell-mode-hook 'haskell-mode-on-init)
(add-to-list 'auto-mode-alist  '("\\.hs$" . haskell-mode))


;; ********************************************************************************
;; Emacs-Lisp Mode
;;
(defun emacs-lisp-mode-on-init ()
  (init-mode)

  (setq ac-sources '(ac-source-yasnippet
					 ac-source-features
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
;; Go Mode
;;
(defun go-mode-on-init ()
  (init-mode)

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers)))

(add-hook 'go-mode-hook 'go-mode-on-init)


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
;; SASS Mode
;;
(defun sass-mode-on-init ()
  (init-mode))

(add-hook 'sass-mode-hook 'sass-mode-on-init)
(autoload 'sass-mode "sass-mode" "Sass Mode" t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))


;; ********************************************************************************
;; C Mode
;;
(defun c-mode-on-init ()
  (init-mode)

  (make-local-variable 'standard-indent)
  (setq standard-indent 4)
  (setq ac-sources '(ac-source-yasnippet
					 ac-source-semantic
					 ac-source-words-in-buffer)))

(add-hook 'c-mode-hook 'c-mode-on-init)


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

(add-to-list 'smart-compile-alist '("^Rakefile$"  . "rake -f %f")) ;
(add-to-list 'smart-compile-alist '("\\.js$"      . "node %f"))
(add-to-list 'smart-compile-alist '("\\.go$"      . "go run %f"))
(add-to-list 'smart-compile-alist '("\\.rb$"      . "ruby %f"))
(add-to-list 'smart-compile-alist '("_spec\\.rb$" . "spec %f"))
(add-to-list 'smart-compile-alist '("\\.scm$"     . "scheme %f"))
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
  (let ((dir (read-directory-name "In directory: ")))
    (vc-git-grep (grep-read-regexp) "." dir)))


(define-key global-map (kbd "RET") 'newline-and-indent)

;; Movement
(global-set-key (kbd "<up>") 'evil-previous-line)
(global-set-key (kbd "<down>") 'evil-next-line)
(global-set-key (kbd "<left>") 'evil-backward-char)
(global-set-key (kbd "<right>") 'evil-forward-char)
(global-set-key (kbd "<C-left>") 'evil-backward-word-begin)
(global-set-key (kbd "<C-right>") 'evil-forward-word-begin)
(global-set-key (kbd "<C-up>") 'evil-backward-paragraph)
(global-set-key (kbd "<C-down>") 'evil-forward-paragraph)

(define-key evil-motion-state-map (kbd "RET") 'insert-newline)
(define-key evil-motion-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-motion-state-map (kbd "C-j") 'evil-forward-paragraph)
(define-key evil-insert-state-map (kbd "C-g") 'evil-force-normal-state)
(define-key evil-motion-state-map (kbd "SPC") 'er/expand-region)

;; (global-set-key (kbd "C-c p") 'mark-previous-like-this)
;; (global-set-key (kbd "C-c n") 'mark-next-like-this)

(global-set-key (kbd "C-c RET") 'helm-mini)
(global-set-key (kbd "C-c c") 'smart-compile)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c .") 'find-tag)
(global-set-key (kbd "C-c /") 'helm-do-grep)
(global-set-key (kbd "C-c e") 'helm-c-etags-select)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "C-c g") 'git-grep)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c C-o") 'helm-multi-occur)
(global-set-key (kbd "C-c k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c v") 'spec-verify)
(global-set-key (kbd "C-c t") 'toggle-buffer)
(global-set-key (kbd "C-c C-s") 'spec-verify-single)

(require 'inline-string-rectangle)
(require 'mark-more-like-this)

(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
