;; ********************************************************************************
;; Load Path
;;
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/apel")
(add-to-list 'load-path "~/.emacs.d/solarized")
(add-to-list 'load-path "~/.emacs.d/erlang")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/icicles")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/nxml-mode")
(add-to-list 'load-path "~/.emacs.d/rhtml")
(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/python")
(add-to-list 'load-path "~/.emacs.d/yasnippet")


;; ********************************************************************************
;; Requires

(require 'evil)
(require 'cl)
(require 'toggle)
(require 'icicles)
(require 'elscreen)
(require 'find-file-in-project)
(require 'browse-kill-ring)
(require 'etags-table)
(require 'etags-select)
(require 'session)
(require 'smart-compile)
(require 'sr-speedbar)
(require 'color-theme-solarized)

(setq solarized-contrast 'high)
(setq visible-bell 1)
(setq ring-bell-function (lambda() ()))

;; ********************************************************************************
;; Variables
;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq case-fold-search t)
(setq enable-recursive-minibuffers nil)
(setq inhibit-startup-screen t)
(setq nxml-slash-auto-complete-flag t)
(setq session-initialize t)
(setq standard-indent 2)
(setq tags-revert-without-query t)
(setq tab-width 4)
(setq toggle-mapping-style 'rspec)
(setq icicle-Completions-text-scale-decrease 0)

(evil-mode 1)
(icy-mode)
(cua-mode 0)
(tool-bar-mode 0)
(global-hl-line-mode 0)

(setq toggle-mapping-styles
      '((rspec
	 . (("app/models/\\1.rb"      . "spec/models/\\1_spec.rb")
	    ("app/controllers/\\1.rb" . "spec/controllers/\\1_spec.rb")
	    ("app/views/\\1.rb"       . "spec/views/\\1_spec.rb")
	    ("app/helpers/\\1.rb"     . "spec/helpers/\\1_spec.rb")
	    ("app/processors/\\1.rb"  . "spec/processors/\\1_spec.rb")
	    ("spec/lib/\\1_spec.rb"   . "lib/\\1.rb")))
	(ruby
	 . (("lib/\\1.rb"             . "test/test_\\1.rb")
	    ("\\1.rb"                 . "test_\\1.rb")))))

(toggle-style "rspec")

(defvar autocomplete-initialized nil)

(defun init-autocomplete ()
  (require 'yasnippet)
  (require 'auto-complete-config)
  (require 'auto-complete-yasnippet)

  (yas/initialize)
  (yas/load-directory "~/.emacs.d/yasnippet/snippets")
  (yas/load-directory "~/.emacs.d/snippets/")
  (setq yas/trigger-key "TAB")
  (add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (setq ac-auto-start nil)
  (global-auto-complete-mode t))

(defun init-mode ()
  (unless autocomplete-initialized
    (init-autocomplete)
    (setq autocomplete-initialized t))

  (add-hook 'before-save-hook 'untabify-buffer nil t)
  (setq indent-tabs-mode nil)
  (make-local-variable 'tags-file-name))

;; ********************************************************************************
;; Defuns

(defun lgrep-from-isearch ()
  (interactive)
  (let ((shk-search-string isearch-string))
    (grep-compute-defaults)
    (lgrep (if isearch-regexp shk-search-string (regexp-quote shk-search-string))
           (format "*.%s" (file-name-extension (buffer-file-name)))
           default-directory)
    (isearch-abort)))

(defun occur-from-isearch ()
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-O") 'lgrep-from-isearch)
(define-key isearch-mode-map (kbd "C-o") 'occur-from-isearch)

(defun popup-yank-menu()
  (interactive)
  (let ((x-y (posn-x-y (posn-at-point (point)))))
    (popup-menu 'yank-menu (list (list (+ (car x-y) 10)
                                       (+ (cdr x-y) 20))
                                 (selected-window)))))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))


(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))



(browse-kill-ring-default-keybindings)
(setq kill-ring-max 20)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Desktop
;; (desktop-save-mode t)
;; (setq desktop-globals-to-save nil)
;; (setq desktop-load-locked-desktop t)
;; (setq desktop-save t)

;; ********************************************************************************
;; ibuffer
;;
(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-enable nil)
(setq ibuffer-expert t)

;; ********************************************************************************
;; I-Menu
;;
(setq imenu-auto-rescan t)
(setq imenu-sort-function 'imenu--sort-by-name)

;; Modes
(setq initial-major-mode 'text-mode)
(column-number-mode t)
(mouse-wheel-mode t)
(if (boundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
;;(partial-completion-mode nil)
(show-paren-mode t)
(transient-mark-mode t)
(recentf-mode)

(setq scroll-conservatively 5)
(setq scroll-step 1)

(load "find-tags-file")

;; ********************************************************************************
;; Autoloads
;;
(autoload 'rdebug "rdebug" "ruby Debug" t)


;; ********************************************************************************
;; Tags
;;
(setq tags-add-tables nil)
(setq etags-table-search-up-depth 5)


;; ********************************************************************************
;; Session
;;
(setq session-initialize t)
(add-hook 'after-init-hook 'session-initialize)

;; ********************************************************************************
;; RI
;;
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ruby/ri-emacs.rb"))
(autoload 'ri "~/.emacs.d/ruby/ri-ruby.el" nil t)


;; ********************************************************************************
;; Erlang Mode
;;
(autoload 'erlang-mode "erlang-mode.el" "Major mode for editing erlang files" t)
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl\\'" . erlang-mode))


;; ********************************************************************************
;; Markdown Mode
;;
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; ********************************************************************************
;; Python Mode
;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))


;; ********************************************************************************
;; PHP Mode
;;

(autoload 'php-mode "php-mode" "PHP Mode." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

(defun php-mode-on-init ()
  (init-mode)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (c-set-style "k&r")
  )

(add-hook 'php-mode-hook 'php-mode-on-init)



;; ********************************************************************************
;; Chuck Mode
;;

(autoload 'chuck-mode "chuck-mode" "Chuck Mode." t)
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))


;; ********************************************************************************
;; Erlang Mode
;;
(add-to-list 'auto-mode-alist '("\\.yaws\\'" . erlang-mode))



;; ********************************************************************************
;; Ruby Mode
;;

(setq enh-ruby-program "/Users/soundcloud/.rvm/rubies/ruby-1.9.2-p290/bin/ruby")

(add-to-list 'auto-mode-alist  '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rake$" . ruby-mode))

(defun spec-run-single-file (spec-file &rest opts)
  "Runs spec with the specified options"
  (compile (concat "~/.emacs.d/spin " spec-file " " (mapconcat (lambda (x) x) opts " ")))
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

  ;; (define-key ruby-mode-map (kbd "<return>") 'ruby-reindent-then-newline-and-indent)

  (setq ruby-deep-indent-paren nil)
  (setq ruby-compilation-error-regexp "^\\([^: ]+\.rb\\):\\([0-9]+\\):")

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers))
  )

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
;; IRB Mode
;;
(defun inferior-ruby-mode-on-init ())

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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'js2-mode "js2-mode" "JS2 Mode." t)

(setq js2-mode-dev-mode-p t)
(setq js2-mode-must-byte-compile nil)

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

  ;; (define-key js2-mode-map (kbd "<return>") 'reindent-then-newline-and-indent)
  )

(add-hook 'js2-mode-hook 'js2-mode-on-init)

(defun js-mode-on-init ()
  (init-mode)

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-semantic
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers))


  (add-hook 'js-mode-hook 'js-mode-on-init))

;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))


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
		     ac-source-words-in-buffer))

  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-on-init))



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

  (setq ac-sources '(ac-source-yasnippet
		     ac-source-semantic
		     ac-source-words-in-buffer)))

(add-hook 'c-mode-hook 'c-mode-on-init)



;; ********************************************************************************
;; XML Mode
;;
(autoload 'nxml-mode "nxml-mode" "XML Mode" t)
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(defun nxml-mode-on-init ()
  (init-mode)
  (setq ac-sources '(ac-source-yasnippet
		     ac-source-semantic
		     ac-source-words-in-buffer)))

(add-hook 'nxml-mode-hook 'nxml-mode-on-init)


;; ********************************************************************************
;; dired
;;
(defun joc-dired-up-directory()
  (interactive)
  (joc-dired-single-buffer ".."))

(defun dired-mode-on-init ()
  (require 'dired-single)
  (require 'wdired)

  (define-key dired-mode-map (kbd "<return>") 'joc-dired-single-buffer)
  (define-key dired-mode-map (kbd "<down-mouse-1>") 'joc-dired-single-buffer-mouse)
  (define-key dired-mode-map (kbd "<C-up>") 'joc-dired-up-directory)

  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

  (setq dired-backup-overwrite t)
  (setq dired-listing-switches "-al")
  ;; (setq dired-omit-files "^\\.")
  )

(add-hook 'dired-mode-hook 'dired-mode-on-init)

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
(add-to-list 'smart-compile-alist '("\\.rb$"      . "ruby %f"))
(add-to-list 'smart-compile-alist '("_spec\\.rb$" . "spec %f"))
(add-to-list 'smart-compile-alist '("\\.scm$"     . "scheme %f"))
(add-to-list 'smart-compile-alist '(haskell-mode  . "ghc -o %n %f")) ;

(setq ffip-patterns
      '("*.haml" "*.sass" "*.html" "*.org" "*.txt" "*.md" "*.el" "*.clj" "*.py" "*.rb" "*.js" "*.pl"
	"*.sh" "*.erl" "*.hs" "*.ml" "*.yml" "*.css"))


(setq speedbar-show-unknown-files t)
(setq sr-speedbar-width 40)


(defun speedbar-toggle()
  (interactive)
  (sr-speedbar-toggle)
  (sr-speedbar-select-window))


(color-theme-solarized-dark)

(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)

(set-face-attribute 'elscreen-tab-control-face nil
		    :underline nil
		    :foreground "#CCCCCC"
		    :background "#000000")

(set-face-attribute 'elscreen-tab-other-screen-face nil
		    :underline nil
		    :foreground "#52676f"
		    :background "#999999")

(set-face-attribute 'elscreen-tab-current-screen-face nil
		    :foreground "#52676f"
		    :background "#fcf4dc")



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

(defun elscreen-find-file-in-project ()
  (interactive)
  (let* ((project-files (ffip-project-files))
         (files (mapcar 'car project-files))
         (file (completing-read "Find file in project: " files)))
    (elscreen-find-and-goto-by-buffer (find-file-noselect (cdr (assoc file project-files))) 'create)))

(elscreen-set-prefix-key (kbd "C-a"))

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

(define-key evil-motion-state-map "e" 'evil-backward-word-begin)
(define-key evil-motion-state-map " " 'evil-visual-line)

(define-key evil-motion-state-map (kbd "<M-return>") 'insert-newline)
(define-key evil-motion-state-map (kbd "C-k") 'evil-forward-word-begin)
(define-key evil-motion-state-map (kbd "C-j") 'evil-backward-word-begin)
(define-key evil-motion-state-map (kbd "M-j") 'evil-forward-paragraph)
(define-key evil-motion-state-map (kbd "M-k") 'evil-backward-paragraph)
(define-key evil-insert-state-map (kbd "C-g") 'evil-force-normal-state)

(global-set-key (kbd "C-c c") 'smart-compile)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c /") 'tags-search)
(global-set-key (kbd "C-c ?") 'etags-select-find-tag-at-point)
(global-set-key (kbd "C-c .") 'etags-select-find-tag)
(global-set-key (kbd "C-c [") 'start-kbd-macro)
(global-set-key (kbd "C-c ]") 'end-kbd-macro)
(global-set-key (kbd "C-c \\") 'call-last-kbd-macro)
(global-set-key (kbd "C-c g") 'rgrep)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c d") 'color-theme-solarized-dark)
(global-set-key (kbd "C-c l") 'color-theme-solarized-light)
(global-set-key (kbd "C-c s") 'speedbar-toggle)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c v") 'spec-verify)
(global-set-key (kbd "C-c t") 'toggle-buffer)
(global-set-key (kbd "C-c C-s") 'spec-verify-single)

(global-set-key (kbd "s-d") 'split-window-horizontally)
(global-set-key (kbd "s-D") 'split-window-vertically)

(global-set-key (kbd "<C-return>") 'icicle-buffer)

(global-set-key (kbd "s-j") 'elscreen-previous)
(global-set-key (kbd "s-t") 'elscreen-create)
(global-set-key (kbd "s-k") 'elscreen-next)
(global-set-key (kbd "s-w") 'elscreen-kill)
(global-set-key (kbd "s-d") 'elscreen-dired)
(global-set-key (kbd "s-f") 'elscreen-find-file-in-project)

(global-set-key (kbd "<s-left>") 'elscreen-previous)
(global-set-key (kbd "<s-right>") 'elscreen-next)


(defun elscreen-goto-1() (interactive) (elscreen-goto 1))
(defun elscreen-goto-2() (interactive) (elscreen-goto 2))
(defun elscreen-goto-3() (interactive) (elscreen-goto 3))
(defun elscreen-goto-4() (interactive) (elscreen-goto 4))

(global-set-key (kbd "s-1") 'elscreen-goto-1)
(global-set-key (kbd "s-2") 'elscreen-goto-2)
(global-set-key (kbd "s-3") 'elscreen-goto-3)
(global-set-key (kbd "s-4") 'elscreen-goto-4)

;; (global-set-key (kbd "<C-delete>") 'kill-word)
;; (global-set-key (kbd "<C-backspace>") 'backward-kill-word)
