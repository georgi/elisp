;; ********************************************************************************
;; Load Path
;;
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/apel")
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(add-to-list 'load-path "~/.emacs.d/nxml-mode")
(add-to-list 'load-path "~/.emacs.d/rhtml")
(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/python")
(add-to-list 'load-path "~/.emacs.d/yasnippet")

;; ********************************************************************************
;; Variables
;;
(fset 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)
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
(setq tooltip-delay 10)


(set-face-attribute 'default nil
                    :background "grey20"
                    :foreground "grey90")

(set-face-attribute 'cursor nil
                    :background "white")

(set-face-attribute 'font-lock-builtin-face nil
                    :foreground "grey60")

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "grey60")

(set-face-attribute 'font-lock-constant-face nil
                    :foreground "grey60")

(set-face-attribute 'font-lock-keyword-face nil
                    :foreground "white")

(set-face-attribute 'font-lock-string-face nil
                    :foreground "white")

(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "lightblue")

(set-face-attribute 'region nil
                    :background "#111")
                    

 '(default ((t (:stipple nil :background "grey20" :foreground "grey90" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 116 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "white"))))
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "grey60"))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background dark)) (:foreground "grey70"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) nil)))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "white"))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) nil)))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) nil)))
 '(fringe ((((class color) (background dark)) nil)))
 '(highlight ((t (:background "#111"))))




(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/snippets/")
(setq yas/trigger-key "TAB")


(require 'auto-complete-config)
(require 'auto-complete-yasnippet)
(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/ac-dict"))
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq kill-ring-max 20)

(require 'sr-speedbar)
(global-set-key (kbd "M-t") 'sr-speedbar-toggle)
(global-set-key (kbd "M-w") 'sr-speedbar-select-window)

;; Cua
(cua-mode t)
(setq cua-enable-region-auto-help t)
(setq cua-highlight-region-shift-only nil)

(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

(load "elscreen" "ElScreen" t)

(elscreen-set-prefix-key (kbd "C-."))

;; Desktop 
(desktop-save-mode t)
(setq desktop-globals-to-save nil)
(setq desktop-load-locked-desktop t)
(setq desktop-save t)

;; ibuffer
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
(partial-completion-mode nil)
(tool-bar-mode nil)
(show-paren-mode t)
(transient-mark-mode t)
(recentf-mode)

;; (if window-system
;;     (global-hl-line-mode t))

(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))

(setq scroll-conservatively 5)
(setq scroll-step 1)

;; Woman
(setq woman-fill-column 80)
(setq woman-use-own-frame nil)

(load "elscreen" "ElScreen" t)

;; (load "abbrevs")c
;; (load "completions")
;; (load "menu-from-pattern")

(load "find-tags-file")
(load "move-lines")


;; ********************************************************************************
;; Autoloads
;;
(autoload 'worklog "worklog" "work log" t)
(autoload 'rdebug "rdebug" "ruby Debug" t)


;; ********************************************************************************
;; Tags
;;
(require 'etags-table)
(require 'etags-select)

(setq tags-add-tables nil)
(setq etags-table-search-up-depth 5)


;; ********************************************************************************
;; Session
;;
(require 'session)

(setq session-initialize t)
(add-hook 'after-init-hook 'session-initialize)

;; ********************************************************************************
;; RI
;;
(setq ri-ruby-script (expand-file-name "~/.emacs.d/ruby/ri-emacs.rb"))
(autoload 'ri "~/.emacs.d/ruby/ri-ruby.el" nil t)


;; ********************************************************************************
;; Markdown Mode
;;
(eval-when-compile (require 'markdown-mode))

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
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-style "k&r")
  )

(add-hook 'php-mode-hook 'php-mode-on-init)



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
(require 'rspec-mode)

(setq ruby-deep-indent-paren nil)
(setq ruby-compilation-error-regexp "^\\([^: ]+\.rb\\):\\([0-9]+\\):")

;; (setq ruby-deep-indent-paren t)

(add-to-list 'auto-mode-alist  '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist  '("\\.rake$" . ruby-mode))  

(defun ruby-mode-on-init ()
  (require 'inflections)

  (setq ffip-find-options "-not -regex '.*vendor.*'")

  (rinari-launch)
  (setq indent-tabs-mode nil)  
  (setq ruby-deep-indent-paren nil)

  (make-local-variable 'tags-file-name)

  (set (make-local-variable 'tab-width) 2)

  (setq toggle-mapping-style 'rspec)

  (add-hook 'before-save-hook 'untabify-buffer)

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-words-in-buffer))
 
  (define-key ruby-mode-map (kbd "C-c =") 'ruby-xmp-region)
  (define-key ruby-mode-map (kbd "C-c C-v") 'rspec-verify)
  (define-key ruby-mode-map (kbd "C-c C-s") 'rspec-verify-single)
  (define-key ruby-mode-map (kbd "C-c C-a") 'rspec-verify-single)
  (define-key ruby-mode-map (kbd "C-c C-d") 'rspec-toggle-example-pendingness)
  (define-key ruby-mode-map (kbd "C-c C-t") 'rspec-toggle-spec-and-target)
  )

(add-hook 'ruby-mode-hook 'ruby-mode-on-init)

(defun rspec-run-single-file (spec-file &rest opts)
  "Runs spec with the specified options"
  (rspec-register-verify-redo (cons 'rspec-run-single-file (cons spec-file opts)))
  (compile (concat "rspec " spec-file " --drb " (mapconcat (lambda (x) x) opts " ")))
  (end-of-buffer-other-window 0))

(defun rspec-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name))))

(defun rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name)) (concat "--line " (number-to-string (line-number-at-pos)))))
 
(defun rspec-verify-all ()
  "Runs the 'spec' rake task for the project of the current file."
  (interactive)
  (let ((default-directory (or (rspec-project-root) default-directory)))
    (rspec-run "--format=progress")))



;; ********************************************************************************
;; RHTML Mode
;;
(eval-when-compile (require 'rhtml-mode))

(autoload 'rhtml-mode "rhtml-mode" "RHTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.rhtml$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))

(defun rhtml-mode-on-init ()
  (abbrev-mode nil)
  (add-hook 'before-save-hook 'untabify-buffer)
  (setq indent-tabs-mode nil)
  (make-local-variable 'tags-file-name)
  )

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
  (setq indent-tabs-mode nil)

  (add-hook 'before-save-hook 'untabify-buffer)

  ;; (define-key js2-mode-map (kbd "<return>") 'reindent-then-newline-and-indent)
  )

(add-hook 'js2-mode-hook 'js2-mode-on-init)

;; (defun js-mode-on-init ()
;;   (make-local-variable 'tags-file-name)
;;   (auto-complete-mode t)

;;   (setq ac-sources '(ac-source-yasnippet
;;                      ac-source-semantic
;;                      ac-source-words-in-buffer))

;;   (add-hook 'before-save-hook 'untabify-buffer))

;; (add-hook 'js-mode-hook 'js-mode-on-init)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; ********************************************************************************
;; Emacs-Lisp Mode
;;
(defun emacs-lisp-mode-on-init ()

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-features
                     ac-source-functions
                     ac-source-symbols
                     ac-source-variables
                     ac-source-words-in-buffer))

  )

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-on-init)



;; ********************************************************************************
;; HTML Mode
;;
(add-to-list 'auto-mode-alist  '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist  '("\\.liquid$" . html-mode))  

(defun html-mode-on-init ()
  (add-hook 'before-save-hook 'untabify-buffer)
  )

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
  (add-hook 'before-save-hook 'untabify-buffer)

  (setq cssm-indent-level 4)
  (setq cssm-indent-function #'cssm-c-style-indenter)
  (set (make-local-variable 'imenu-generic-expression)
       css-imenu-generic-expression)
  )

(add-hook 'css-mode-hook 'css-mode-on-init)


;; ********************************************************************************
;; C Mode
;;
(defun c-mode-on-init ()
  (add-hook 'before-save-hook 'untabify-buffer)

  (setq ac-sources '(ac-source-yasnippet
                     ac-source-semantic
                     ac-source-words-in-buffer))

  )


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
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-semantic
                     ac-source-words-in-buffer))

  )

(add-hook 'nxml-mode-hook 'nxml-mode-on-init)


;; ********************************************************************************
;; dired
;;
(require 'dired)
(require 'dired-single)
(require 'wdired)

(defun joc-dired-up-directory()
  (interactive)
  (joc-dired-single-buffer ".."))

(define-key dired-mode-map (kbd "<return>") 'joc-dired-single-buffer)
(define-key dired-mode-map (kbd "<down-mouse-1>") 'joc-dired-single-buffer-mouse)
(define-key dired-mode-map (kbd "<C-up>") 'joc-dired-up-directory)

(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

(setq dired-backup-overwrite t)
(setq dired-listing-switches "-al")
;; (setq dired-omit-files "^\\.")

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
;; Defuns
(defun popup-yank-menu()
  (interactive)
  (let ((x-y (posn-x-y (posn-at-point (point)))))
    (popup-menu 'yank-menu (list (list (+ (car x-y) 10)
                                       (+ (cdr x-y) 20))
                                 (selected-window)))))

(defun save-and-exit()
  (interactive)
  (save-buffer)
  (save-buffers-kill-terminal))

(defun kill-current-buffer()
  (interactive) 
  (kill-buffer (buffer-name)))

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
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


;; ********************************************************************************
;; Global Key Bindings
;;

;; Function keys
(global-set-key (kbd "<f10>") 'smart-compile)

(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c s") 'eshell)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

(global-set-key (kbd "M-/") 'tags-search)
(global-set-key (kbd "M-?") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-.") 'etags-select-find-tag)

(global-set-key (kbd "M-a") 'align-regexp)
(global-set-key (kbd "M-i") 'indent-buffer)
(global-set-key (kbd "M-s") 'shell)
(global-set-key (kbd "M-r") 'rgrep)

(global-set-key (kbd "<M-return>") 'ido-switch-buffer)
(global-set-key (kbd "<C-M-return>") 'recentf-ido-find-file)

(global-set-key (kbd "C-x C-c") 'save-and-exit)

(global-set-key (kbd "M-1") (lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "M-2") (lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "M-3") (lambda () (interactive) (elscreen-goto 2)))

(global-set-key (kbd "M-c") 'elscreen-create)
(global-set-key (kbd "M-k") 'elscreen-kill)
(global-set-key (kbd "M-d") 'elscreen-dired)
(global-set-key (kbd "<M-left>") 'elscreen-previous)
(global-set-key (kbd "<M-right>") 'elscreen-next)
(global-set-key (kbd "<M-up>") 'tabbar-backward-group)
(global-set-key (kbd "<M-down>") 'tabbar-forward-group)

(global-set-key (kbd "<C-delete>") 'kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

(global-set-key (kbd "<C-left>") 'backward-word)
(global-set-key (kbd "<C-right>") 'forward-word)

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
;; (global-set-key (kbd "C-c s") 'rinari-find-stylesheet)
;; (global-set-key (kbd "C-c t") 'rinari-find-test)
;; (global-set-key (kbd "C-c t") 'rinari-find-test)
;; (global-set-key (kbd "C-x t") 'rinari-test)
;; (global-set-key (kbd "C-x r") 'rinari-rake)
(global-set-key (kbd "C-x c") 'rinari-console)
(global-set-key (kbd "C-x s") 'rinari-sql)
(global-set-key (kbd "C-x w") 'rinari-web-server)
(global-set-key (kbd "C-x g") 'rinari-rgrep)

(global-set-key (kbd "C-c o") 'toggle-buffer)

(defvar current-font-size 120)

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

(require 'server)

(when (and (= emacs-major-version 23)
           (= emacs-minor-version 1)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                        ; ~/.emacs.d/server is unsafe"
                                        ; on windows.


; hide mode line, from 
; http://dse.livejournal.com/66834.html / http://webonastick.com
(autoload 'hide-mode-line "hide-mode-line" nil t)

(server-start)
