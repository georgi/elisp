;; ********************************************************************************
;; My theme
;;

(set-face-attribute 'default nil 
		    :foreground "#333"
		    :background "#ffffff")

(set-face-attribute 'hl-line nil 
		    :background "#fffff0")

(set-face-attribute 'fringe nil
		    :background "#ffffff")

(set-face-attribute 'cursor nil
		    :background "#000")

(set-face-attribute 'dropdown-list-face nil  
		    :foreground "#000"
		    :background "#eef")

(set-face-attribute 'dropdown-list-selection-face nil
		    :background "#fee")

(set-face-attribute 'vertical-border nil 
		    :foreground "#eee")

(set-face-attribute 'region nil
		    :background "#ffffcc")

(set-face-attribute 'mode-line nil 
		    :foreground "#666" 
		    :background "#eee"
		    :box '(:line-width 1 :color "#eee"))

(set-face-attribute 'mode-line-inactive nil 
		    :foreground "#999" 
		    :background "#fff"
		    :box '(:line-width 1 :color "#fff"))

(set-face-attribute 'mode-line-buffer-id nil
		    :foreground "#111")

(set-face-attribute 'mode-line-highlight nil 
		    :foreground "#333" 
		    :background "#ccc" 
		    :box '(:line-width 1 :color "#444"))

(set-face-attribute 'tabbar-default nil
		    :foreground "#333" 
		    :background "#eee"
		    :height 1.0
		    :inherit 'default)

(set-face-attribute 'tabbar-selected nil
		    :foreground "#333"
		    :background "#ffffff"
		    :weight 'bold
		    :box nil)

(set-face-attribute 'tabbar-unselected nil
		    :foreground "#666"
		    :background "#f0f0f0"
		    :box nil)

(set-face-attribute 'tabbar-button nil
		    :foreground "#666"
		    :box '(:line-width 1 :color "#fff"))

(set-face-attribute 'font-lock-builtin-face nil
		    :foreground "#909")

(set-face-attribute 'font-lock-keyword-face nil
		    :foreground "#009")

(set-face-attribute 'font-lock-function-name-face nil
		    :foreground "#090")

(set-face-attribute 'font-lock-variable-name-face nil
		    :foreground "#099")

(set-face-attribute 'font-lock-comment-face nil
		    :foreground "#c90")

(set-face-attribute 'font-lock-string-face nil
		    :foreground "#c93")


(add-hook 'rhtml-mode-hook 
	  (lambda ()
	    (set-face-attribute 'erb-exec-delim-face nil
				:weight 'normal)

	    (set-face-attribute 'erb-out-delim-face nil
				:weight 'normal)))

