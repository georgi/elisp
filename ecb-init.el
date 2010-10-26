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
(setq ecb-toggle-layout-sequence '("top1" "left2" "leftright2"))

;; (setq ecb-vc-supported-backends (quote ((ecb-vc-dir-managed-by-SVN . ecb-vc-state) (ecb-vc-dir-managed-by-GIT . ecb-vc-state))))

(setq ecb-common-directories-menu
      '((ecb-grep-directory "Grep Directory")
        (ecb-grep-find-directory "Grep Directory recursive")
        (ecb-dired-directory "Open in Dired")
        (ecb-dired-directory-other-window "Open in Dired other window")
        ("---")
        (ecb-create-source "Create Sourcefile")
        (ecb-create-directory "Create Child Directory")
        (ecb-delete-directory "Delete Directory")
        ("---")
        (ecb-add-source-path-node "Add Source Path")))

(setq ecb-directories-menu ecb-common-directories-menu)


(setq ecb-history-menu
      (append
      '((ecb-grep-directory "Grep Directory")
        (ecb-grep-find-directory "Grep Directory recursive")
        (ecb-dired-directory "Open in Dired")
        (ecb-dired-directory-other-window "Open in Dired other window")
        ("---")
         ("Filter"
          (ecb-popup-history-filter-by-ext "Filter by extension")
          (ecb-popup-history-filter-by-regexp "Filter by regexp")
          (ecb-popup-history-filter-all-existing "No filter"))
         ("---")
         (ecb-history-kill-buffer "Kill Buffer")
         (ecb-delete-source "Delete Sourcefile"))
       ecb-history-common-menu))


(setq ecb-sources-menu
      '((ecb-grep-directory "Grep Directory")
        (ecb-grep-find-directory "Grep Directory recursive")
        (ecb-dired-directory "Open in Dired")
        (ecb-dired-directory-other-window "Open in Dired other window")
        ("Filter"
         (ecb-popup-sources-filter-by-ext "Filter by extension")
         (ecb-popup-sources-filter-by-regexp "Filter by a regexp")
         (ecb-popup-sources-filter-none "No filter"))
        ("---")        
        (ecb-create-source "Create Sourcefile")
        (ecb-delete-source "Delete Sourcefile")
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))



