;;=============================================================================
;;
;; hide-html-mode: implement the functionality of WriteRoom.
;;
;; Basically, automatically hides the mode-line if all of the following
;; are true:
;; - there is only one frame.
;; - there is only one window displayed in that frame.
;; - there is no minibuffer.
;; - the hide-mode-line variable is set.
;; and automatically shows the mode-line when any of the above isn't true.
;;
;;=============================================================================
;;
;; HOW TO USE
;;
;; Just put this file in your emacs library directory and add this line to
;; your ~/.emacs:
;;
;;   (autoload 'hide-mode-line "hide-mode-line" nil t)
;; 
;; and use M-x hide-mode-line to toggle.  Setting the hide-mode-line variable
;; won't automatically update the buffers' mode-line visibilities.
;;
;;=============================================================================
;;
;; This is rough draft code.  Consider it GPL2-licensed.  I'll deal with
;; formalities and documentation conventions and such later.  ;)
;;
;; AUTHOR: Darren Embry -- dse at webonastick dot com
;;
;;=============================================================================
;;
;; BUG: every once in a while a few lines of text will be hidden for some
;; reason until you do a redraw-display.  See if you can reproduce this in a
;; reliable fashion!
;;
;; BUG: not specific to this module, but...  load linum, run M-x linum-mode,
;; then (setq mode-line-format nil) this triggers display problems more
;; reproducibly: sometimes the last line in the buffer doesn't have the line
;; number show up; and sometimes the cursor line or the one after it doesn't
;; have the line number show up.  May be related to above bug.
;;
;; CAVEAT: this code does not instruct your window system to make the window
;; full-screen.
;;
;; TODO: briefly show modeline for (example) 2 seconds when the following
;; happens:
;; - hide-mode-line is about to be activated
;; - you switch to another buffer
;;
;; TODO: emacs 21 does not implement window-tree.
;;
;; BUG: if the hide-mode-line-window-configuration-change-hook function
;; displays a (message "moo") before it does its work, the screen is blanked
;; when you resize the window until you hit C-l.
;;
;; SEE ALSO:
;; http://www.emacswiki.org/cgi-bin/wiki/LineNumbers
;; http://www.emacswiki.org/cgi-bin/wiki/WriteRoom
;;
;;=============================================================================

(defvar hide-mode-line-saved-mode-line-format nil)
(make-variable-buffer-local 'hide-mode-line-saved-mode-line-format)
; TODO: add a hook of some kind when setting mode-line-format.

(defvar hide-mode-line nil)
; TODO: add a hook to run hide-mode-line-update when setting hide-mode-line.
; [or just use M-x hide-mode-line for now]

(defcustom hide-mode-line-unaffected-by-minibuffer nil
  "If non-nil, the presence of a minibuffer by itself does not
force the mode line to be shown."
  :group 'hide-mode-line
  :type  'boolean)

(defun there-is-only-one-frame ()
  "Returns non-nil if there is only one frame, nil otherwise."
  (let ((frames (frames-on-display-list)))
    (if (= (length frames) 1)
	(car frames)
      nil)))
(defun there-is-only-one-window-in (frame)
  "Returns non-nil if there is only one window in the specified FRAME,
nil otherwise."
  (let ((root (car (window-tree frame)))) ;FIXME: does not work with emacs21
    (not (listp root))))
(defun there-is-only-one-frame-and-one-window ()
  "Returns non-nil if there is only one frame and there is only
one window in that frame, nil otherwise."
  (let ((the-only-frame (there-is-only-one-frame)))
    (and the-only-frame
	 (or hide-mode-line-unaffected-by-minibuffer
	     (= (minibuffer-depth) 0))
	 (there-is-only-one-window-in the-only-frame))))

(defun hide-mode-line-in (buffer)
  "Hide the specified BUFFER's mode line, saving its previous
mode-line-format value if it's not already hidden."
  (with-current-buffer buffer
    (if (not hide-mode-line-saved-mode-line-format)
	(progn (setq hide-mode-line-saved-mode-line-format 
		     (list mode-line-format))
	       (setq mode-line-format nil)))))
(defun show-mode-line-in (buffer)
  "If the specified BUFFER's mode line is hidden, restores its
mode-line-format from what was saved when hide-mode-line-in was
called."
  (with-current-buffer buffer
    (if hide-mode-line-saved-mode-line-format
	(progn (setq mode-line-format
		     (car hide-mode-line-saved-mode-line-format))
	       (setq hide-mode-line-saved-mode-line-format nil)))))

(defun hide-mode-lines ()
  "Hide all buffers' mode lines using hide-mode-line-in."
  (mapcar 'hide-mode-line-in (buffer-list)))
(defun show-mode-lines ()
  "Show all buffers' mode lines using show-mode-line-in."
  (mapcar 'show-mode-line-in (buffer-list)))

(defun hide-mode-line-update ()
  "Update the state of all buffers' mode lines, using
hide-mode-lines or show-mode-lines."
  (if hide-mode-line
      (if (there-is-only-one-frame-and-one-window)
	  (hide-mode-lines)
	(show-mode-lines))
    (show-mode-lines)))

(defun hide-mode-line-minibuffer-setup-hook ()
  (hide-mode-line-update))
(defun hide-mode-line-minibuffer-exit-hook ()
  (hide-mode-line-update))
(defun hide-mode-line-make-frame-function (new-frame)
  (hide-mode-line-update))
(defun hide-mode-line-delete-frame-function (dead-frame-walking)
  (hide-mode-line-update))
(defun hide-mode-line-window-configuration-change-hook ()
  (hide-mode-line-update))

(defun hide-mode-line-add-hooks ()
  (interactive)
  (add-hook 'minibuffer-setup-hook            
	    'hide-mode-line-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook             
	    'hide-mode-line-minibuffer-exit-hook)
  (add-hook 'after-make-frame-functions       
	    'hide-mode-line-make-frame-function)
  (add-hook 'delete-frame-functions           
	    'hide-mode-line-delete-frame-function)
  (add-hook 'window-configuration-change-hook 
	    'hide-mode-line-window-configuration-change-hook))

(defun hide-mode-line-remove-hooks ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook            
	       'hide-mode-line-minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook             
	       'hide-mode-line-minibuffer-exit-hook)
  (remove-hook 'after-make-frame-functions       
	       'hide-mode-line-make-frame-function)
  (remove-hook 'delete-frame-functions           
	       'hide-mode-line-delete-frame-function)
  (remove-hook 'window-configuration-change-hook 
	       'hide-mode-line-window-configuration-change-hook))

(defun hide-mode-line ()
  "Toggle the hide-mode-line functionality."
  (interactive)
  (if (functionp 'window-tree)
      (progn
	(setq hide-mode-line (not hide-mode-line))
	(hide-mode-line-update))
    (error (concat "Your Emacs does not provide the window-tree function.  "
		   "Please upgrade to GNU Emacs 22 "
		   "or to some other version of Emacs that provides it."))))

(hide-mode-line-add-hooks)

(provide 'hide-mode-line)
