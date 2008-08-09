;;; Commentary:

;; ------------------------------------------------------------------------
;; Copyright (C) 2006-2007 Jens Peter Secher
;;
;; This software is provided 'as-is', without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this software.
;;
;; Permission is granted to anyone to use this software for any
;; purpose, including commercial applications, and to alter it and
;; redistribute it freely, subject to the following restrictions:
;;
;; 1. The origin of this software must not be misrepresented; you must
;;    not claim that you wrote the original software.  If you use this
;;    software in a product, an acknowledgment in the product
;;    documentation would be appreciated but is not required.
;; 2. Altered source versions must be plainly marked as such, and must
;;    not be misrepresented as being the original software.
;; 3. This notice may not be removed or altered from any source
;;    distribution.
;; ------------------------------------------------------------------------

;; This is haxe-mode, an Emacs major mode for the haXe programming
;; language (http://haxe.org).

;; haxe-mode is built on top of the excellent cc-mode, inspired by the
;; guide http://cc-mode.sourceforge.net/derived-mode-ex.el.

;; haxe-mode is NOT part of GNU Emacs.

;;; Versions:
;;
;;    0.1.0 - Initial release.
;;    0.1.1 - Fixed typedef indentation.
;;            Fixed lexical analysis so that type names can contain digits.
;;    0.2.0 - Base on java-mode instead of c++-mode.
;;            Added compile-error parser for the haXe compiler output.
;;            Loads of improvements.
;;    0.2.1 - Fix buffer-local comment-start-skip problem.
;;
;; TODO: Figure out a way to fix indentations after: typedef Foo = Int

;;; Usage:
;;
;; Include something like this in your .emacs:
;; (require 'haxe-mode)
;; (defconst my-haxe-style
;;   '("java" (c-offsets-alist . ((case-label . +)
;;                                (arglist-intro . +)
;;                                (arglist-close . 0)
;;                                (cpp-macro . 0))))
;;   "My haXe Programming Style")
;; (add-hook 'haxe-mode-hook
;;   (function (lambda () (c-add-style "haxe" my-haxe-style t))))
;; (add-hook 'haxe-mode-hook
;;           (function
;;            (lambda ()
;;              (setq tab-width 4)
;;              (setq indent-tabs-mode t)
;;              (setq fill-column 80)
;;              (local-set-key [(return)] 'newline-and-indent))))


;;; Code:

(require 'cc-mode)
(require 'cc-fonts)
(require 'cc-langs)
(require 'cc-bytecomp)
(require 'compile)

;; The language constants are needed when compiling.
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)
    (load "cc-bytecomp" nil t)))

(eval-and-compile
  ;; Tell the language constant system about haXe and base it on Java.
  (c-add-language 'haxe-mode 'java-mode))

;;; Lexer-level syntax (identifiers, tokens etc).

;; No other operators in identifiers.
(c-lang-defconst c-after-id-concat-ops
  haxe nil)

;; Conditional compilation prefix.
(c-lang-defconst c-opt-cpp-prefix
  haxe "\\s *#")

;; No strings in conditional compilation.
(c-lang-defconst c-cpp-message-directives
  haxe nil)

;; No file name in angle brackets or quotes in conditional compilation.
(c-lang-defconst c-cpp-include-directives
  haxe nil)

;; No macro definition in conditional compilation.
(c-lang-defconst c-opt-cpp-macro-define
  haxe nil)

;; Conditional compilation directives followed by expressions.
(c-lang-defconst c-cpp-expr-directives
  haxe '("if" "else"))

;; No functions in conditional compilation.
(c-lang-defconst c-cpp-expr-functions
  haxe nil)

;; haXe operators.
(c-lang-defconst c-operators
  haxe `(
         ;; Preprocessor.
         (prefix "#")
         ;; Standard operators.
         ,@(c-lang-const c-identifier-ops)
         ;; Generics.
         (postfix-if-paren "<" ">")
         ;; Postfix.
         (left-assoc "." "->")
         (postfix "++" "--" "[" "]" "(" ")")
         ;; Unary.
         (prefix "++" "--" "+" "-" "!" "~" "new")
         ;; Multiplicative.
         (left-assoc "*" "/" "%")
         ;; Additive.
         (left-assoc "+" "-")
         ;; Shift.
         (left-assoc "<<" ">>" ">>>")
         ;; Relational.
         (left-assoc "<" ">" "<=" ">=")
         ;; Iteration.
         (left-assoc "...")
         ;; Equality.
         (left-assoc "==" "!=" "===" "!==")
         ;; Bitwise and.
         (left-assoc "&")
         ;; Bitwise exclusive or.
         (left-assoc "^")
         ;; Bitwise or.
         (left-assoc "|")
         ;; Logical and.
         (left-assoc "&&")
         ;; Logical or.
         (left-assoc "||")
         ;; Assignment.
         (right-assoc ,@(c-lang-const c-assignment-operators))
         ;; Exception.
         (prefix "throw")
         ;; Sequence.
         (left-assoc ",")))

;; No overloading.
(c-lang-defconst c-overloadable-operators
  haxe nil)
(c-lang-defconst c-opt-op-identitier-prefix
  haxe nil)

;;; Keywords.

;; I will treat types uniformly below since they all start with capital
;; letters.
(c-lang-defconst c-primitive-type-kwds
  haxe nil)

;; TODO: check double occurrence of enum.
;; Type-introduction is straight forward in haXe.
(c-lang-defconst c-class-decl-kwds
  haxe '( "class" "interface" "enum" "typedef" ))

;; Recognises enum constants.
;; TODO: find a way to also recognise parameterised constants.
(c-lang-defconst c-brace-list-decl-kwds
  haxe '( "enum" ))

;; Keywords introducing declarations where the (first) identifier (declarator)
;; follows directly after the keyword, without any type.
(c-lang-defconst c-typeless-decl-kwds
  haxe (append '( "function" "var" )
               (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds)))
  
;; Definition modifiers.
(c-lang-defconst c-modifier-kwds
  haxe '( "private" "public" "static" ))
(c-lang-defconst c-other-decl-kwds
  haxe nil)

;; Namespaces.
(c-lang-defconst c-ref-list-kwds
 haxe '( "import" "package"))

;; Statement keywords followed directly by a substatement.
(c-lang-defconst c-block-stmt-1-kwds
  haxe '( "do" "else" "try" ))

;; Statement keywords followed by a paren sexp and then by a substatement.
(c-lang-defconst c-block-stmt-2-kwds
  haxe '( "for" "if" "switch" "while" "catch" ))

;; Statement keywords followed by an expression or nothing.
(c-lang-defconst c-simple-stmt-kwds
  haxe '( "break" "continue" "return" "default" "new" ))

;; No ';' inside 'for'.
(c-lang-defconst c-paren-stmt-kwds
  haxe nil)

;; Keywords for constants.
(c-lang-defconst c-constant-kwds
  haxe '( "false" "true" "null" ))

;; Keywords for expressions.
(c-lang-defconst c-primary-expr-kwds
  haxe '( "this" "super" ))

(c-lang-defconst c-decl-hangon-kwds
  haxe '( "in" ))

;; No other labels.
(c-lang-defconst c-before-label-kwds
  haxe nil)

;; No classes inside expressions.
(c-lang-defconst c-inexpr-class-kwds
  haxe nil)

;; No brace lists inside expressions.
(c-lang-defconst c-inexpr-brace-list-kwds
  haxe nil)

;; No ellipsis.
;(c-lang-defconst c-opt-type-suffix-key
;  haxe nil)

;(c-lang-defconst c-recognize-colon-labels
;  haxe nil)

;; Allow '=' in typedefs so they are treated as classes.
(c-lang-defconst c-block-prefix-disallowed-chars
  haxe (set-difference (c-lang-const c-block-prefix-disallowed-chars java)
                       '(?=)))

;; All identifiers starting with a capital letter are types.
(c-lang-defconst c-cpp-matchers
  haxe (append
        (c-lang-const c-cpp-matchers c)
        '(("\\<\\([A-Z][A-Za-z0-9_]*\\)\\>" 1 font-lock-type-face))))

;; Generic types.
(c-lang-defconst c-recognize-<>-arglists
  haxe t)

;; Fontification degrees.
(defconst haxe-font-lock-keywords-1 (c-lang-const c-matchers-1 haxe)
  "Minimal highlighting for haxe mode.")
(defconst haxe-font-lock-keywords-2 (c-lang-const c-matchers-2 haxe)
  "Fast normal highlighting for haxe mode.")
(defconst haxe-font-lock-keywords-3 (c-lang-const c-matchers-3 haxe)
  "Accurate normal highlighting for haxe mode.")
(defvar haxe-font-lock-keywords haxe-font-lock-keywords-3
  "Default expressions to highlight in haxe mode.")
(defun haxe-font-lock-keywords-1 ()
  (c-compose-keywords-list haxe-font-lock-keywords-1))
(defun haxe-font-lock-keywords-2 ()
  (c-compose-keywords-list haxe-font-lock-keywords-2))
(defun haxe-font-lock-keywords-3 ()
  (c-compose-keywords-list haxe-font-lock-keywords-3))
(defun haxe-font-lock-keywords ()
  (c-compose-keywords-list haxe-font-lock-keywords))

(defvar haxe-mode-syntax-table nil
  "Syntax table used in HaXe mode buffers.")
(or haxe-mode-syntax-table
    (setq haxe-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table haxe))))

(defvar haxe-mode-abbrev-table nil
  "Abbreviation table used in haxe mode buffers.")
(c-define-abbrev-table 'haxe-mode-abbrev-table
  ;; Keywords that, if they occur first on a line, might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar haxe-mode-map ()
  "Keymap used in haxe mode buffers.")
(if haxe-mode-map
    nil
  (setq haxe-mode-map (c-make-inherited-keymap)))

(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))

;; Tell compilation-mode how to parse error messages.  You need to set
;; compilation-error-screen-columns to nil to get the right
;; interpretation of tabs.
(add-to-list 'compilation-error-regexp-alist
             '("^\\([^: ]+\\):\\([0-9]+\\): characters \\([0-9]+\\)-[0-9]+ : "
               1 2 3))

(defcustom haxe-mode-hook nil
  "*Hook called by `haxe-mode'."
  :type 'hook
  :group 'c)

(defun haxe-mode ()
  "Major mode for editing haXe code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `haxe-mode-hook'.

Key bindings:
\\{haxe-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table haxe-mode-syntax-table)
  (setq major-mode 'haxe-mode
        mode-name "haXe"
        local-abbrev-table haxe-mode-abbrev-table
        abbrev-mode t)
  (use-local-map haxe-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars haxe-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'haxe-mode)
  ;; For some reason, comment-start-skip has to be set manually.
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  ;; TODO: (easy-menu-add haxe-menu)
  (run-hooks 'c-mode-common-hook 'haxe-mode-hook)
  (c-update-modeline))

(provide 'haxe-mode)

;;; haxe-modex.el ends here.
