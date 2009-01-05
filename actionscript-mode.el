;;; actionscript-mode.el --- major mode for editing actionscript code

;; Copyright (c) 2005,2006 Pet Tomato, Inc.
;; Portions Copyright (c) 2004 David Lindquist <david@lindquist.net>

;; Author: Austin Haas <austin@pettomato.com>
;; Based on: ecmascript-mode by David Lindquist <david@lindquist.net>
;; Keywords: languages actionscript

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'font-lock)
(require 'cc-mode)
(eval-when-compile
  (require 'regexp-opt))

(defconst actionscript-mode-version "3.0"
  "Actionscript Mode version number.")

(defgroup actionscript nil
  "Major mode for editing Actionscript code."
  :group 'languages
  :prefix "actionscript-")

(defcustom actionscript-mode-hook nil
  "Hook for customizing `actionscript-mode'."
  :group 'actionscript
  :type 'hook)

(defvar actionscript-mode-map (c-make-inherited-keymap)
  "Keymap used in `actionscript-mode' buffers.")

;;;###autoload
(define-derived-mode actionscript-mode java-mode "Actionscript"
  "Major mode for editing Actionscript code.

This mode is derived from `java-mode'; see its documentation for further
information.

\\{actionscript-mode-map}"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((;; comment out the lines below to adjust
           ;; syntax highlighting gaudiness
           ;actionscript-font-lock-keywords-1
           ;actionscript-font-lock-keywords-2
           actionscript-font-lock-keywords-3
           )
          nil nil ((?_ . "w") (?$ . "w")) nil))

  (define-key actionscript-mode-map [(control meta a)] 'as-beginning-of-defun)
  (define-key actionscript-mode-map [(control meta e)] 'as-end-of-defun)
  (define-key actionscript-mode-map [(control meta h)] 'as-mark-defun)

  (easy-menu-define c-actionscript-menu actionscript-mode-map
    "Actionscript Mode Commands" (c-mode-menu "Actionscript")))

(defvar actionscript-font-lock-default-face 'actionscript-font-lock-default-face)

(defconst actionscript-font-lock-keywords-1
  (append
   java-font-lock-keywords-1
   (list

    '("\\<\\(function\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; need to fix this to handle: var a, b;
    '("\\<\\(var\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
    ))
  "Subdued level highlighting for Actionscript mode.")

(defconst actionscript-font-lock-keywords-2
  (append
   java-font-lock-keywords-2
   actionscript-font-lock-keywords-1
   (list

    "\\<\\(debugger\\|delete\\|export\\|in\\|typeof\\|with\\)\\>"

    (list (concat
           "\\<\\("
           (mapconcat 'identity java-font-lock-extra-types nil)
           "\\)\\>\\.")
          '(1 font-lock-type-face nil t))

    ;; In Java, `void' is a type. In Actionscript, it is an operator.
    ;; This overrides the inherited notion of keyword `void'.
    '("\\<\\(void\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face t)
      (2 actionscript-font-lock-default-face t t))

    ;; Value properties of the global object
    '("\\<\\(Infinity\\|NaN\\|undefined\\)\\>" 0 font-lock-constant-face t)

    ;; Properties of the Number constructor
    (list (concat
           "\\<Number\\."
           (regexp-opt
            '("MAX_VALUE" "MIN_VALUE" "NaN" "NEGATIVE_INFINITY"
              "POSITIVE_INFINITY") t)
           "\\>")
          '(1 font-lock-constant-face))

    ;; Value properties of the Math object
    (list (concat
           "\\<Math\\."
           (regexp-opt
            '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2") t)
           "\\>")
          '(1 font-lock-constant-face))
    ))
  "Medium level highlighting for Actionscript mode.")

(defconst actionscript-font-lock-keywords-3
  (append
   java-font-lock-keywords-3
   actionscript-font-lock-keywords-2
   (list

    ;; Properties of the Date constructor
    '("\\<Date\\.\\(parse\\|UTC\\)\\>" 1 font-lock-builtin-face)

    ;; Function properties of the Math object
    (list (concat
           "\\<Math\\."
           (regexp-opt
            '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
              "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
           "\\>")
          '(1 font-lock-builtin-face))

    (list (regexp-opt
           '(;; URI handling function properties
             "decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
             ;; Function properties of the global object
             "trace" "eval" "isFinite" "isNaN" "parseFloat" "parseInt") 'words)
          '(0 font-lock-builtin-face))

    (list (concat
           "\\."
           (regexp-opt
            '(;; Properties of the Object prototype object
              "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
              "toLocaleString" "toString" "valueOf"
              ;; Properties of the Function prototype object
              "apply" "call"
              ;; Properties of the Array prototype object
              "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
              "splice" "unshift"
              ;; Properties of the String prototype object
              "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
              "localeCompare" "match" "replace" "search" "split" "substring"
              "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
              "toUpperCase"
              ;; Properties of the Number prototype object
              "toExponential" "toFixed" "toPrecision"
              ;; Properties of the Date prototype object
              "getDate" "getDay" "getFullYear" "getHours" "getMilliseconds"
              "getMinutes" "getMonth" "getSeconds" "getTime"
              "getTimezoneOffset" "getUTCDate" "getUTCDay" "getUTCFullYear"
              "getUTCHours" "getUTCMilliseconds" "getUTCMinutes" "getUTCMonth"
              "getUTCSeconds" "setDate" "setFullYear" "setHours"
              "setMilliseconds" "setMinutes" "setMonth" "setSeconds" "setTime"
              "setUTCDate" "setUTCFullYear" "setUTCHours" "setUTCMilliseconds"
              "setUTCMinutes" "setUTCMonth" "setUTCSeconds" "toDateString"
              "toLocaleDateString" "toLocaleString" "toLocaleTimeString"
              "toTimeString" "toUTCString"
              ;; Properties of the RegExp prototype object
              "exec" "test"
              ) t)
           "\\>")
          '(1 font-lock-builtin-face))
    ))
  "Gaudy level highlighting for Actionscript mode.")

(defun as-get-function-re(&optional function-name)
	"Returns a regular expression that will match the function signature containing the supplied function-name.
If function-name is omitted, the regexp will match any function."
	(unless function-name
		(setq function-name "[a-zA-Z0-9_$]*"))
	(format "\\(^[ \t\n]*\\)\\(static \\)?\\(?:private \\)?\\(?:public \\)?\\(static \\)?function \\(%s\\)([ \t\n]*\\(.*?\\)[ \t\n]*)[ \t\n]*\\(:[a-zA-Z_0-9$]*\\)?[ \t\n]*{" function-name))

(defconst as-function-re (as-get-function-re)
	"A regexp that matches a function signature in Actionscript.")

(defun as-get-beginning-of-defun()
	;; Returns the position.
	(save-excursion
		(when	(re-search-backward as-function-re nil t)
			(match-end 1))))

(defun as-get-end-of-defun()
	;; This only works if we are inside a defun.
	(save-excursion
		(when	(re-search-backward as-function-re nil t)
			(goto-char (match-end 0))
			;; Move back a char, so that point is right on
			;; the opening brace.
			(backward-char)
			(forward-list)
			(point))))

(defun as-get-end-of-defun2()
	;; This should work if we are not inside any defuns.
	(save-excursion
		(beginning-of-line) ; hack, in case point is currently inside a function sig.
		(when	(re-search-forward as-function-re nil t)
			(goto-char (match-end 0))
			;; Move back a char, so that point is right on
			;; the opening brace.
			(backward-char)
			(forward-list)
			(point))))

(defun as-beginning-of-defun()
	(interactive)
	(let ((pos (as-get-beginning-of-defun)))
		(if pos
				(goto-char pos)
			(message "Can't find any functions."))))

(defun as-inside-defun?()
	(let ((cur (point))
				(start (as-get-beginning-of-defun))
				(end (as-get-end-of-defun)))
		(and start
				 end
				 (> cur start)
				 (< cur end))))

(defun as-end-of-defun()
	(interactive)
	(if (as-inside-defun?)
			(goto-char (as-get-end-of-defun))
		(let ((pos (as-get-end-of-defun2)))
			(if pos
					(goto-char pos)
				(message "Can't find any functions.")))))

(defun as-mark-defun()
	(interactive)
	(let ((start (as-get-beginning-of-defun))
				(end (as-get-end-of-defun)))
		(if (not (or start end))
				(message "Can't find any functions.")
			(set-mark end)
			(goto-char start)
			(beginning-of-line))))

(provide 'actionscript-mode)
