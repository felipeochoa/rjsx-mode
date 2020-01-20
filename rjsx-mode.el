;;; rjsx-mode.el --- Real support for JSX    -*- lexical-binding: t -*-

;; Copyright (C) 2016 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/rjsx-mode/
;; Package-Requires: ((emacs "24.4") (js2-mode "20170504"))
;; Version: 1.1
;; Keywords: languages

;;; Commentary:
;; Defines a major mode `rjsx-mode' based on `js2-mode' for editing
;; JSX files.  `rjsx-mode' extends the parser in `js2-mode' to support
;; the full JSX syntax.  This means you get all of the `js2' features
;; plus proper syntax checking and highlighting of JSX code blocks.
;;
;; Some features that this mode adds to js2:
;;
;; - Highlighting JSX tag names and attributes (using the rjsx-tag and
;;   rjsx-attr faces)
;; - Highlight undeclared JSX components
;; - Parsing the spread operator {...otherProps}
;; - Parsing && and || in child expressions {cond && <BigComponent/>}
;; - Parsing ternary expressions {toggle ? <ToggleOn /> : <ToggleOff />}
;;
;; Additionally, since rjsx-mode extends the js2 AST, utilities using
;; the parse tree gain access to the JSX structure.

;;; Code:

;;;; Basic mode definitions

(require 'cl-lib)
(require 'js2-mode)
(eval-when-compile (require 'subr-x))
(require 'newcomment)
(require 'sgml-mode)

(defgroup rjsx-mode nil
  "Support for JSX."
  :group 'js2-mode)

(defcustom rjsx-max-size-for-frequent-reparse 100000
  "Buffers with fewer than this many characters will be parsed more frequently.
Set this to 0 to disable the reparsing altogether.  The frequent
parsing supports the magic `rjsx-electric-lt' and
`rjsx-delete-creates-full-tag' behaviors."
  :group 'rjsx-mode
  :type 'integer)

;;;###autoload
(define-derived-mode rjsx-mode js2-mode "RJSX"
  "Major mode for editing JSX files."
  :lighter ":RJSX"
  :group 'rjsx-mode
  (setq-local comment-use-syntax nil)
  (setq-local comment-start-skip "[[:space:]]*\\(//+\\|{?/\\*+\\)")
  ;; \n is included to get arround `comment-normalize-vars' and `comment-only-p'
  (setq-local comment-end-skip "\\(\\*+/}?[[:space:]]*\\)\n?\\|\n")
  (setq-local comment-region-function 'rjsx-comment-region-function)
  (setq-local uncomment-region-function 'rjsx-uncomment-region-function)
  (setq-local comment-quote-nested-function 'rjsx-comment-quote-nested-function)
  (setq-local indent-line-function 'rjsx-indent-line)
  (setq-local indent-region-function 'rjsx-indent-region))

;;;###autoload
(define-minor-mode rjsx-minor-mode
  "Minor mode for parsing JSX syntax into an AST."
  :lighter " rjsx"
  (if rjsx-minor-mode
      (js2-minor-mode 1)
    (js2-minor-mode 0)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(defun rjsx-parse-xml-initializer (orig-fun)
  "Dispatch the xml parser based on variable `rjsx-mode' being active or not.
This function is used to advise `js2-parse-xml-initializer' (ORIG-FUN) using
the `:around' combinator.  JS2-PARSER is the original XML parser."
  (if (or (eq major-mode 'rjsx-mode) rjsx-minor-mode)
      (rjsx-parse-top-xml)
    (apply orig-fun nil)))

(advice-add 'js2-parse-xml-initializer :around #'rjsx-parse-xml-initializer)

(defun rjsx-unadvice-js2 ()
  "Remove the rjsx advice on the js2 parser.  This will cause rjsx to stop working globally."
  (advice-remove 'js2-parse-xml-initializer #'rjsx-parse-xml-initializer)
  (advice-remove 'js--looking-at-operator-p #'rjsx--js--looking-at-operator-p-advice))


(defface rjsx-tag
  '((t . (:inherit font-lock-function-name-face)))
  "`rjsx-mode' face used to highlight JSX tag names."
  :group 'rjsx-mode)

(defface rjsx-attr
  '((t . (:inherit font-lock-variable-name-face)))
  "`rjsx-mode' face used to highlight JSX attribute names."
  :group 'rjsx-mode)

(defface rjsx-text
  '((t . (:inherit font-lock-string-face)))
  "`rjsx-mode' face used to highlight JSX text."
  :group 'rjsx-mode)

(defface rjsx-tag-bracket-face
  '((t . (:inherit default)))
  "`rjsx-mode' face used to highlight `<', `/', and `>'."
  :group 'rjsx-mode)


;;;; Parser constants struct definitions

;; Token types for XML nodes. We need to re-use some unused values to
;; not mess up the vectors that js2 has set up
(defvar rjsx-JSX            js2-ENUM_INIT_KEYS)
(defvar rjsx-JSX-CLOSE      js2-ENUM_INIT_VALUES)
(defvar rjsx-JSX-IDENT      js2-ENUM_INIT_ARRAY)
(defvar rjsx-JSX-MEMBER     js2-ENUM_NEXT)
(defvar rjsx-JSX-ATTR       js2-ENUM_ID)
(defvar rjsx-JSX-SPREAD     js2-REF_NS_MEMBER)
(defvar rjsx-JSX-TEXT       js2-ESCXMLTEXT)
(defvar rjsx-JSX-EXPRESSION js2-ESCXMLATTR)

(dolist (sym '(rjsx-JSX rjsx-JSX-CLOSE rjsx-JSX-IDENT rjsx-JSX-MEMBER rjsx-JSX-ATTR
                        rjsx-JSX-SPREAD rjsx-JSX-TEXT rjsx-JSX-EXPRESSION))
  (aset js2-token-names (symbol-value sym) (downcase (substring (symbol-name sym) 5)))
  (puthash sym (symbol-value sym) js2-token-codes))

(js2-msg "msg.bad.jsx.ident" "invalid JSX identifier")
(js2-msg "msg.invalid.jsx.string" "invalid JSX string (cannot contain delimiter in string body)")
(js2-msg "msg.mismatched.close.tag" "mismatched closing JSX tag; expected `%s'")
(js2-msg "msg.no.gt.in.opener" "missing `>' in opening tag")
(js2-msg "msg.no.gt.in.closer" "missing `>' in closing tag")
(js2-msg "msg.no.gt.after.slash" "missing `>' after `/' in self-closing tag")
(js2-msg "msg.no.rc.after.spread" "missing `}' after spread-prop")
(js2-msg "msg.no.value.after.jsx.prop" "missing value after prop `%s'")
(js2-msg "msg.no.dots.in.prop.spread" "missing `...' in spread prop")
(js2-msg "msg.no.rc.after.expr" "missing `}' after expression")
(js2-msg "msg.empty.expr" "empty `{}' expression")


(cl-defstruct (rjsx-node
               (:include js2-node (type rjsx-JSX))
               (:constructor nil)
               (:constructor make-rjsx-node
                             (&key (pos (js2-current-token-beg))
                                   len
                                   name
                                   rjsx-props
                                   kids)))
  name         ; AST node containing the parsed xml name or nil for fragments
  rjsx-props   ; linked list of AST nodes (both attributes and spreads)
  kids         ; linked list of child xml nodes
  closing-tag) ; AST node with the tag closer


(js2--struct-put 'rjsx-node 'js2-visitor 'rjsx-node-visit)
(js2--struct-put 'rjsx-node 'js2-printer 'rjsx-node-print)
(defun rjsx-node-visit (ast callback)
  "Visit the `rjsx-node' children of AST, invoking CALLBACK on them."
  (let ((name (rjsx-node-name ast)))
    (when name (js2-visit-ast name callback)))
  (dolist (prop (rjsx-node-rjsx-props ast))
    (js2-visit-ast prop callback))
  (dolist (prop (rjsx-node-kids ast))
    (js2-visit-ast prop callback))
  (when (rjsx-node-closing-tag ast)
    (js2-visit-ast (rjsx-node-closing-tag ast) callback)))

(defun rjsx-node-print (node indent-level)
  "Print the `rjsx-node' NODE at indent level INDENT-LEVEL."
  (insert (js2-make-pad indent-level) "<")
  (let ((name-n (rjsx-node-name node)))
   (when name-n (js2-print-ast name-n 0)))
  (dolist (attr (rjsx-node-rjsx-props node))
    (insert " ")
    (js2-print-ast attr 0))
  (let ((closer (rjsx-node-closing-tag node)))
    (if (null closer)
        (insert "/>")
      (insert ">")
      (dolist (child (rjsx-node-kids node))
          (js2-print-ast child 0))
      (js2-print-ast closer indent-level))))

(defun rjsx-node-opening-tag-name (node)
  "Return a string with NODE's opening tag including any namespace and member operations."
  (let ((name-n (rjsx-node-name node)))
    (cond
     ((null name-n) "")                 ; fragment
     ((rjsx-member-p name-n) (rjsx-member-full-name name-n))
     ((rjsx-identifier-p name-n) (rjsx-identifier-full-name name-n))
     (t ""))))                          ; js2-error-node

(defun rjsx-node-push-prop (n rjsx-prop)
  "Extend rjsx-node N's rjsx-props with js2-node RJSX-PROP.
Sets JSX-PROPS's parent to N."
  (let ((rjsx-props (rjsx-node-rjsx-props n)))
    (if rjsx-props
        (setcdr rjsx-props (nconc (cdr rjsx-props) (list rjsx-prop)))
      (setf (rjsx-node-rjsx-props n) (list rjsx-prop))))
  (js2-node-add-children n rjsx-prop))

(defun rjsx-node-push-child (n kid)
  "Extend rjsx-node N's children with js2-node KID.
Sets KID's parent to N."
  (let ((kids (rjsx-node-kids n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (setf (rjsx-node-kids n) (list kid))))
  (js2-node-add-children n kid))


(cl-defstruct (rjsx-closing-tag
               (:include js2-node (type rjsx-JSX-CLOSE))
               (:constructor nil)
               (:constructor make-rjsx-closing-tag (&key pos len name)))
  name) ; An rjsx-identifier or rjsx-member node or nil for fragments

(js2--struct-put 'rjsx-closing-tag 'js2-visitor 'rjsx-closing-tag-visit)
(js2--struct-put 'rjsx-closing-tag 'js2-printer 'rjsx-closing-tag-print)

(defun rjsx-closing-tag-visit (ast callback)
  "Visit the `rjsx-closing-tag' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-closing-tag-name ast) callback))

(defun rjsx-closing-tag-print (node indent-level)
  "Print the `rjsx-closing-tag' NODE at INDENT-LEVEL."
  (insert (js2-make-pad indent-level) "</" (rjsx-closing-tag-full-name node) ">"))

(defun rjsx-closing-tag-full-name (n)
  "Return the string with N's fully-namespaced name, or just name if it's not namespaced."
  (let ((child (rjsx-closing-tag-name n)))
    (cond
     ((null child) "")                  ; fragment
     ((rjsx-member-p child) (rjsx-member-full-name child))
     ((rjsx-identifier-p child) (rjsx-identifier-full-name child))
     (t ""))))

(cl-defstruct (rjsx-identifier
               (:include js2-node (type rjsx-JSX-IDENT))
               (:constructor nil)
               (:constructor make-rjsx-identifier (&key (pos (js2-current-token-beg))
                                                           len namespace name)))
  (namespace nil)
  name)  ; js2-name-node

(js2--struct-put 'rjsx-identifier 'js2-visitor 'rjsx-identifier-visit)
(js2--struct-put 'rjsx-identifier 'js2-printer 'rjsx-identifier-print)

(defun rjsx-identifier-visit (n callback)
  "Visit N's children can call CALLBACK on them."
  (js2-visit-ast (rjsx-identifier-name n) callback))

(defun rjsx-identifier-print (node indent-level)
  "Print the `rjsx-identifier' NODE at INDENT-LEVEL."
  (insert (js2-make-pad indent-level) (rjsx-identifier-full-name node)))

(defun rjsx-identifier-full-name (n)
  "Return the string with N's fully-namespaced name, or just name if it's not namespaced."
  (if (rjsx-identifier-namespace n)
      (format "%s:%s" (rjsx-identifier-namespace n) (js2-name-node-name (rjsx-identifier-name n)))
    (js2-name-node-name (rjsx-identifier-name n))))

(cl-defstruct (rjsx-member
               (:include js2-node (type rjsx-JSX-MEMBER))
               (:constructor nil)
               (:constructor make-rjsx-member (&key pos len dots-pos idents)))
  dots-pos  ; List of positions of each dot
  idents)   ; List of rjsx-identifier nodes

(js2--struct-put 'rjsx-member 'js2-visitor 'rjsx-member-visit)
(js2--struct-put 'rjsx-member 'js2-printer 'rjsx-member-print)

(defun rjsx-member-visit (n callback)
  "Visit N's children and call CALLBACK on them."
  (dolist (ident (rjsx-member-idents n))
    (js2-visit-ast ident callback)))

(defun rjsx-member-print (node indent-level)
  "Print the `rjsx-member' NODE at INDENT-LEVEL."
  (insert (js2-make-pad indent-level) (rjsx-member-full-name node)))

(defun rjsx-member-full-name (n)
  "Return the string with N's combined names together."
  (mapconcat #'rjsx-identifier-full-name
             ;; Fix #89. There could be an error node here
             (cl-remove-if-not #'rjsx-identifier-p (rjsx-member-idents n))
             "."))

(cl-defstruct (rjsx-attr
               (:include js2-node (type rjsx-JSX-ATTR))
               (:constructor nil)
               (:constructor make-rjsx-attr (&key (pos (js2-current-token-beg))
                                                     len name value)))
  name    ; a rjsx-identifier
  value)  ; a js2-expression

(js2--struct-put 'rjsx-attr 'js2-visitor 'rjsx-attr-visit)
(js2--struct-put 'rjsx-attr 'js2-printer 'rjsx-attr-print)

(defun rjsx-attr-visit (ast callback)
  "Visit the `rjsx-attr' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-attr-name ast) callback)
  (js2-visit-ast (rjsx-attr-value ast) callback))

(defun rjsx-attr-print (node indent-level)
  "Print the `rjsx-attr' NODE at INDENT-LEVEL."
  (js2-print-ast (rjsx-attr-name node) indent-level)
  (unless (js2-empty-expr-node-p (rjsx-attr-value node))
    (insert "=")
    (js2-print-ast (rjsx-attr-value node) 0)))

(cl-defstruct (rjsx-spread
               (:include js2-node (type rjsx-JSX-SPREAD))
               (:constructor nil)
               (:constructor make-rjsx-spread (&key pos len expr)))
  expr)  ; a js2-expression

(js2--struct-put 'rjsx-spread 'js2-visitor 'rjsx-spread-visit)
(js2--struct-put 'rjsx-spread 'js2-printer 'rjsx-spread-print)

(defun rjsx-spread-visit (ast callback)
  "Visit the `rjsx-spread' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-spread-expr ast) callback))

(defun rjsx-spread-print (node indent-level)
  "Print the `rjsx-spread' NODE at INDENT-LEVEL."
  (insert (js2-make-pad indent-level) "{...")
  (js2-print-ast (rjsx-spread-expr node) 0)
  (insert "}"))

(cl-defstruct (rjsx-wrapped-expr
               (:include js2-node (type rjsx-JSX-TEXT))
               (:constructor nil)
               (:constructor make-rjsx-wrapped-expr (&key pos len child)))
  child)

(js2--struct-put 'rjsx-wrapped-expr 'js2-visitor 'rjsx-wrapped-expr-visit)
(js2--struct-put 'rjsx-wrapped-expr 'js2-printer 'rjsx-wrapped-expr-print)

(defun rjsx-wrapped-expr-visit (ast callback)
  "Visit the `rjsx-wrapped-expr' child of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-wrapped-expr-child ast) callback))

(defun rjsx-wrapped-expr-print (node indent-level)
  "Print the `rjsx-wrapped-expr' NODE at INDENT-LEVEL."
  (insert (js2-make-pad indent-level) "{")
  (js2-print-ast (rjsx-wrapped-expr-child node) indent-level)
  (insert "}"))

(cl-defstruct (rjsx-text
               (:include js2-node (type rjsx-JSX-TEXT))
               (:constructor nil)
               (:constructor make-rjsx-text (&key (pos (js2-current-token-beg))
                                                     (len (js2-current-token-len))
                                                     value)))
  value)  ; a string

(js2--struct-put 'rjsx-text 'js2-visitor 'js2-visit-none)
(js2--struct-put 'rjsx-text 'js2-printer 'rjsx-text-print)

(defun rjsx-text-print (node _indent-level)
  "Print the `rjsx-text' NODE at INDENT-LEVEL."
  ;; Text nodes include whitespace
  (insert (rjsx-text-value node)))


;;;; Recursive descent parsing
(defvar rjsx-print-debug-message nil "If t will print out debug messages.")
;(setq rjsx-print-debug-message t)
(defmacro rjsx-maybe-message (&rest args)
  "If debug is enabled, call `message' with ARGS."
  `(when rjsx-print-debug-message
     (message ,@args)))


(defvar rjsx-text-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    ;; `js-indent-line' assumes that \n is not whitespace
    ;; Since it's not a comment delimiter in JSX text, punctuation
    ;; is the only other (semi) logical choice
    (modify-syntax-entry ?\n "." table)
    table))

(js2-deflocal rjsx-in-xml nil "Variable used to track which xml parsing function is the outermost one.")

(define-error 'rjsx-eof-while-parsing "RJSX: EOF while parsing")

(defmacro rjsx-handling-eof (&rest body)
  "Execute BODY and return the result of the last form.
If BODY signals `rjsx-eof-while-parsing', instead report a syntax
error and return a `js2-error-node'."
  `(let ((beg (js2-current-token-beg)))
     (condition-case nil
         (progn ,@body)
       (rjsx-eof-while-parsing
        (let ((len (- js2-ts-cursor beg)))
          (rjsx-maybe-message (format "Handling eof from %d" beg))
          (js2-report-error "msg.syntax" nil beg len)
          (make-js2-error-node :pos beg :len len))))))

(defsubst rjsx-record-tag-bracket-face ()
  "Fontify the current token with `rjsx-tag-bracket-face'."
  (js2-set-face (js2-current-token-beg) (js2-current-token-end) 'rjsx-tag-bracket-face 'record))

(defsubst rjsx-record-token-class (cls)
  "Record an 'rjsx-class text property with value CLS for the current token."
  (js2-record-text-property (js2-current-token-beg) (js2-current-token-end)
                            'rjsx-class cls))

(defun rjsx-parse-top-xml ()
  "Parse a top level XML fragment.
This is the entry point when ‘js2-parse-unary-expr’ finds a '<' character"
  (if rjsx-in-xml
      (rjsx-parse-xml)
    (let ((rjsx-in-xml t))
      (rjsx-handling-eof (rjsx-parse-xml)))))

(defun rjsx-parse-xml ()
  "Parse a complete xml node from start to end tag."
  (rjsx-record-token-class '<)
  (rjsx-record-tag-bracket-face)
  (let ((pn (make-rjsx-node)) self-closing name-n name-str child child-name-str is-fragment)
    (rjsx-maybe-message "Starting rjsx-parse-xml after <")
    (if (setq child (rjsx-parse-empty-tag))
        child
      (if (eq (js2-peek-token) js2-GT)
          (setq is-fragment t)
        (setf (rjsx-node-name pn) (setq name-n (rjsx-parse-member-or-ns 'rjsx-tag)))
        (if (js2-error-node-p name-n)
            (progn (rjsx-maybe-message "could not parse tag name")
                   (make-js2-error-node :pos (js2-node-pos pn) :len (1+ (js2-node-len name-n))))
          (js2-node-add-children pn name-n)
          (if js2-highlight-external-variables
              (let ((name-node (rjsx-identifier-name
                                (if (rjsx-member-p name-n)
                                    (car (rjsx-member-idents name-n))
                                  name-n)))
                    (case-fold-search nil))
                (when (string-match-p "^[[:upper:]]" (js2-name-node-name name-node))
                  (js2-record-name-node name-node)))))
        (rjsx-maybe-message "cleared tag name: '%s'" name-str)
        ;; Now parse the attributes
        (rjsx-parse-attributes pn)
        (rjsx-maybe-message "cleared attributes"))
      (setq name-str (rjsx-node-opening-tag-name pn))
      (progn
        ;; Now parse either a self closing tag or the end of the opening tag
        (rjsx-maybe-message "next type: `%s'" (js2-peek-token))
        (if (setq self-closing (js2-match-token js2-DIV))
            (progn
              (rjsx-record-token-class 'self-closing-slash)
              (rjsx-record-tag-bracket-face)
              ;; TODO: How do we un-mark old slashes?
              (when (js2-must-match js2-GT "msg.no.gt.after.slash"
                                    (js2-node-pos pn) (- (js2-current-token-end) (js2-node-pos pn)))
                (rjsx-record-token-class '>)
                (rjsx-record-tag-bracket-face)))
          (when (js2-must-match js2-GT "msg.no.gt.in.opener" (js2-node-pos pn) (js2-node-len pn))
            (rjsx-record-token-class '>)
            (rjsx-record-tag-bracket-face)))
        (rjsx-maybe-message "cleared opener closer, self-closing: %s" self-closing)
        (if self-closing
            (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
          (while (not (rjsx-closing-tag-p (setq child (rjsx-parse-child is-fragment))))
            ;; rjsx-parse-child calls our scanner, which always moves
            ;; forward at least one character. If it hits EOF, it
            ;; signals to our caller, so we don't have to worry about infinite loops here
            (rjsx-maybe-message "parsed child")
            (rjsx-node-push-child pn child)
            (if (= 0 (js2-node-len child)) ; TODO: Does this ever happen?
                (js2-get-token)))
          (setq child-name-str (rjsx-closing-tag-full-name child))
          (unless (string= name-str child-name-str)
            (js2-report-error "msg.mismatched.close.tag" name-str (js2-node-pos child) (js2-node-len child)))
          (rjsx-maybe-message "cleared children for `%s'" name-str)
          (js2-node-add-children pn child)
          (setf (rjsx-node-closing-tag pn) child))
        (rjsx-maybe-message "Returning completed XML node")
        (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
        pn))))

(defun rjsx-parse-empty-tag ()
  "Check if we are in an empty tag of the form `</>' and consume it if so.
Returns a `js2-error-node' if we are in one or nil if not."
  (let ((beg (js2-current-token-beg)))
    (when (js2-match-token js2-DIV)
        (if (js2-match-token js2-GT)
            (progn ; We're in a </> block, likely created by us in `rjsx-electric-lt'
              ;; We only highlight the < to reduce the visual impact
              (js2-report-error "msg.syntax" nil beg 1)
              (make-js2-error-node :pos beg :len (- (js2-current-token-end) beg)))
          ;; TODO: This is probably an unmatched closing tag. We should
          ;; consume it, mark it an error, and move on
          (js2-unget-token)
          nil))))

(defun rjsx-parse-attributes (parent)
  "Parse all attributes, including key=value and {...spread}, and add them to PARENT."
  ;; Getting this function to not hang in the loop proved tricky. The
  ;; key is that `rjsx-parse-spread' and `rjsx-parse-single-attr' both
  ;; return `js2-error-node's if they fail to consume any tokens,
  ;; which signals to us that we just need to discard one token and
  ;; keep going.
  (let (attr
        (loop-terminators (list js2-DIV js2-GT js2-EOF js2-ERROR)))
    (while (not (memql (js2-peek-token) loop-terminators))
      (rjsx-maybe-message "Starting loop. Next token type: %s\nToken pos: %s" (js2-peek-token) (js2-current-token-beg))
      (setq attr
            (if (js2-match-token js2-LC)
                (or (rjsx-check-for-empty-curlies t)
                    (prog1 (rjsx-parse-spread)
                      (rjsx-maybe-message "Parsed spread")))
              (rjsx-maybe-message "Parsing single attr")
              (rjsx-parse-single-attr)))
      (when (js2-error-node-p attr) (js2-get-token))
                                        ; TODO: We should make this conditional on
                                        ; `js2-recover-from-parse-errors'
      (rjsx-node-push-prop parent attr))))


(cl-defun rjsx-check-for-empty-curlies (&optional dont-consume-rc &key check-for-comments warning)
  "If the following token is '}' set empty curly errors.
If DONT-CONSUME-RC is non-nil, the matched right curly token
won't be consumed.  Returns a `js2-error-node' if the curlies are
empty or nil otherwise.  If CHECK-FOR-COMMENTS (a &KEY argument)
is non-nil, this will check for comments inside the curlies and
returns a `js2-empty-expr-node' if any are found.  If WARNING (a
&key argument) is non-nil, reports the empty curlies as a warning
and not an error and also returns a `js2-empty-expr-node'.
Assumes the current token is a '{'."
  (let ((beg (js2-current-token-beg)) end len)
    (when (js2-match-token js2-RC)
      (setq end (js2-current-token-end))
      (setq len (- end beg))
      (when dont-consume-rc
        (js2-unget-token))
      (if check-for-comments (rjsx-maybe-message "Checking for comments between %d and %d" beg end))
      (unless (and check-for-comments
                   (dolist (comment js2-scanned-comments)
                     (rjsx-maybe-message "Comment at %d, length=%d"
                                         (js2-node-pos comment)
                                         (js2-node-len comment))
                     ;; TODO: IF comments are in reverse document order, we should be able to
                     ;; bail out early and know we didn't find one
                     (when (and (>= (js2-node-pos comment) beg)
                                (<= (+ (js2-node-pos comment) (js2-node-len comment)) end))
                       (cl-return-from rjsx-check-for-empty-curlies
                         (make-js2-empty-expr-node :pos beg :len (- end beg))))))
        (if warning
            (progn (js2-report-warning "msg.empty.expr" nil beg len)
                   (make-js2-empty-expr-node :pos beg :len (- end beg)))
          (js2-report-error "msg.empty.expr" nil beg len)
          (make-js2-error-node :pos beg :len len))))))


(defun rjsx-parse-spread ()
  "Parse an {...props} attribute."
  (let ((pn (make-rjsx-spread :pos (js2-current-token-beg)))
        (beg (js2-current-token-beg))
        missing-dots expr)
    (setq missing-dots (not (js2-match-token js2-TRIPLEDOT)))
    ;; parse-assign-expr will go crazy if we're looking at `} /', so we
    ;; check for an empty spread first
    (if (js2-match-token js2-RC)
        (setq expr (make-js2-error-node :len 1))
      (setq expr (js2-parse-assign-expr))
      (when (js2-error-node-p expr)
        (pop js2-parsed-errors)))       ; We'll add our own error
    (unless (or (js2-match-token js2-RC) (js2-error-node-p expr))
      (js2-report-error "msg.no.rc.after.spread" nil
                        beg (- (js2-current-token-end) beg)))
    (setf (rjsx-spread-expr pn) expr)
    (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
    (js2-node-add-children pn expr)
    (if (js2-error-node-p expr)
        (js2-report-error "msg.syntax" nil beg (- (js2-current-token-end) beg))
      (when missing-dots
        (js2-report-error "msg.no.dots.in.prop.spread" nil beg (js2-node-len pn))))
    (if (= 0 (js2-node-len pn))  ; TODO: Is this ever possible?
        (make-js2-error-node :pos beg :len 0)
      pn)))

(defun rjsx-parse-single-attr ()
  "Parse an 'a=b' JSX attribute and return the corresponding XML node."
  (let ((pn (make-rjsx-attr)) name value beg)
    (setq name (rjsx-parse-identifier 'rjsx-attr)) ; Won't consume token on error
    (if (js2-error-node-p name)
        name
      (setf (rjsx-attr-name pn) name)
      (setq beg (js2-node-pos name))
      (setf (js2-node-pos pn) beg)
      (js2-node-add-children pn name)
      (rjsx-maybe-message "Got the name for the attr: `%s'" (rjsx-identifier-full-name name))
      (if (js2-match-token js2-ASSIGN)  ; Won't consume on error
          (progn
            (rjsx-maybe-message "Matched the equals sign")
            (if (js2-match-token js2-LC)
                (setq value (rjsx-parse-wrapped-expr nil t))
              (if (js2-match-token js2-STRING)
                  (setq value (rjsx-parse-string))
                (js2-report-error "msg.no.value.after.jsx.prop" (rjsx-identifier-full-name name)
                                  beg (- (js2-current-token-end) beg))
                (setq value (make-js2-error-node :pos beg :len (js2-current-token-len))))))
        (setq value (make-js2-empty-expr-node :pos (js2-current-token-end) :len 0)))
      (rjsx-maybe-message "value type: `%s'" (js2-node-type value))
      (setf (rjsx-attr-value pn) value)
      (setf (js2-node-len pn) (- (js2-node-end value) (js2-node-pos pn)))
      (js2-node-add-children pn value)
      (rjsx-maybe-message "Finished single attribute.")
      pn)))

(defun rjsx-parse-wrapped-expr (allow-empty skip-to-rc)
  "Parse a curly-brace-wrapped JS expression.
If ALLOW-EMPTY is non-nil, will warn for empty braces, otherwise
will signal a syntax error.  If it does not find a right curly
and SKIP-TO-RC is non-nil, after the expression, consumes tokens
until the end of the JSX node"
  (rjsx-maybe-message "parsing wrapped expression")
  (let (pn
        (beg (js2-current-token-beg))
        (child (rjsx-check-for-empty-curlies nil
                                             :check-for-comments allow-empty
                                             :warning allow-empty)))
    (if child
        (if allow-empty
            (make-rjsx-wrapped-expr :pos beg :len (js2-node-len child) :child child)
          child) ;; Will be an error node in this case
      (setq child (js2-parse-assign-expr))
      (rjsx-maybe-message "parsed expression, type: `%s'" (js2-node-type child))
      (setq pn (make-rjsx-wrapped-expr :pos beg :child child))
      (js2-node-add-children pn child)
      (when (js2-error-node-p child)
        (pop js2-parsed-errors)) ; We'll record our own message after checking for RC
      (if (js2-match-token js2-RC)
          (rjsx-maybe-message "matched } after expression")
        (rjsx-maybe-message "did not match } after expression")
        (when skip-to-rc
          (while (not (memql (js2-get-token) (list js2-RC js2-EOF js2-DIV js2-GT)))
            (rjsx-maybe-message "Skipped over `%s'" (js2-current-token-string)))
          (when (memq (js2-current-token-type) (list js2-DIV js2-GT))
            (js2-unget-token)))
        (unless (js2-error-node-p child)
          (js2-report-error "msg.no.rc.after.expr" nil beg
                            (- (js2-current-token-beg) beg))))
      (when (js2-error-node-p child)
        (js2-report-error "msg.syntax" nil beg (- (js2-current-token-end) beg)))
      (setf (js2-node-len pn) (- (js2-current-token-end) beg))
      pn)))

(defun rjsx-parse-string ()
  "Verify that current token is a valid JSX string.
Returns a `js2-error-node' if TOKEN-STRING is not a valid JSX
string, otherwise returns a `js2-string-node'.  (Strings are
invalid if they contain the delimiting quote character inside)"
  (rjsx-maybe-message "Parsing string")
  (let* ((token (js2-current-token))
         (beg (js2-token-beg token))
         (len (- (js2-token-end token) beg))
         (token-string (js2-token-string token)) ;; JS2 does not include the quote-chars
         (quote-char (char-before (js2-token-end token))))
    (if (cl-position quote-char token-string)
        (progn
          (js2-report-error "msg.invalid.jsx.string" nil beg len)
          (make-js2-error-node :pos beg :len len))
      (make-js2-string-node :pos beg :len len :value token-string))))

(cl-defun rjsx-parse-identifier (&optional face &key (allow-ns t))
  "Parse a possibly namespaced identifier and fontify with FACE if given.
Returns a `js2-error-node' if unable to parse.  If the &key
argument ALLOW-NS is nil, does not allow namespaced names."
  (if (js2-must-match-name "msg.bad.jsx.ident")
      (let ((pn (make-rjsx-identifier))
            (beg (js2-current-token-beg))
            (name-parts (list (js2-current-token-string)))
            (allow-colon allow-ns)
            (continue t)
            (prev-token-end (js2-current-token-end))
            (name-start (js2-current-token-beg))
            matched-colon)
        (while (and continue
                    (or (and (memq (js2-peek-token) (list js2-SUB js2-ASSIGN_SUB))
                             (prog2  ; Ensure no whitespace between previous name and this dash
                                 (js2-get-token)
                                 (eq prev-token-end (js2-current-token-beg))
                               (js2-unget-token)))
                        (and allow-colon (= (js2-peek-token) js2-COLON))))
          (if (setq matched-colon (js2-match-token js2-COLON))
              (setf (rjsx-identifier-namespace pn) (apply #'concat (nreverse name-parts))
                    allow-colon nil
                    name-parts (list)
                    name-start nil)
            (when (= (js2-get-token) js2-ASSIGN_SUB) ; Otherwise it's a js2-SUB
              (setf (js2-token-end (js2-current-token)) (1- (js2-current-token-end))
                    (js2-token-type (js2-current-token)) js2-SUB
                    (js2-token-string (js2-current-token)) "-"
                    js2-ts-cursor (1+ (js2-current-token-beg))
                    js2-ti-lookahead 0))
            (push "-" name-parts))
          (setq prev-token-end (js2-current-token-end))
          (if (js2-match-token js2-NAME)
              (if (eq prev-token-end (js2-current-token-beg))
                  (progn (push (js2-current-token-string) name-parts)
                         (setq prev-token-end (js2-current-token-end)
                               name-start (or name-start (js2-current-token-beg))))
                (js2-unget-token)
                (setq continue nil))
            (when (= js2-COLON (js2-current-token-type))
              (js2-report-error "msg.bad.jsx.ident" nil beg (- (js2-current-token-end) beg)))
            ;; We only keep going if this is an `ident-ending-with-dash-colon:'
            (setq continue (and (not matched-colon) (= (js2-peek-token) js2-COLON)))))
        (when face
          (js2-set-face beg (js2-current-token-end) face 'record))
        (let ((name-node (if name-start
                             (make-js2-name-node :pos name-start
                                                 :len (- (js2-current-token-end) name-start)
                                                 :name (apply #'concat (nreverse name-parts)))
                           (make-js2-name-node :pos (js2-current-token-end) :len 0 :name ""))))
          (setf (js2-node-len pn) (- (js2-current-token-end) beg)
                (rjsx-identifier-name pn) name-node)
          (js2-node-add-children pn name-node))
        pn)
    (make-js2-error-node :len (js2-current-token-len))))

(defun rjsx-parse-member-or-ns (&optional face)
  "Parse a dotted expression or a namespaced identifier and fontify with FACE if given."
  (let ((ident (rjsx-parse-identifier face)))
    (cond
     ((js2-error-node-p ident) ident)
     ((rjsx-identifier-namespace ident) ident)
     (t (rjsx-parse-member ident face)))))

(defun rjsx-parse-member (ident &optional face)
  "Parse a dotted member expression starting with IDENT and fontify with FACE.
IDENT is the `rjsx-identifier' node for the first item in the
member expression.  Returns a `js2-error-node' if unable to
parse."
  (let (idents dots-pos pn end)
    (setq pn (make-rjsx-member :pos (js2-node-pos ident)))
    (setq end (js2-current-token-end))
    (push ident idents)
    (while (and (js2-match-token js2-DOT) (not (js2-error-node-p ident)))
      (push (js2-current-token-beg) dots-pos)
      (setq end (js2-current-token-end))
      (setq ident (rjsx-parse-identifier nil :allow-ns nil))
      (push ident idents)
      (unless (js2-error-node-p ident)
        (setq end (js2-current-token-end)))
      (js2-node-add-children pn ident))
    (apply 'js2-node-add-children pn idents)
    (setf (rjsx-member-idents pn) (nreverse idents)
          (rjsx-member-dots-pos pn) (nreverse dots-pos)
          (js2-node-len pn) (- end (js2-node-pos pn)))
    (when face
      (js2-set-face (js2-node-pos pn) end face 'record))
    pn))


(defun rjsx-parse-child (expect-fragment)
  "Parse an XML child node.
Child nodes include plain (unquoted) text, other XML elements,
and {}-bracketed expressions.  Return the parsed child.

EXPECT-FRAGMENT if t, indicates that `</>' should be parsed
as a fragment closing node, and not as an empty tag."
  (let ((tt (rjsx-get-next-xml-token)))
    (rjsx-maybe-message "child type `%s'" tt)
    (cond
     ((= tt js2-LT)
      (rjsx-maybe-message "xml-or-close")
      (rjsx-parse-xml-or-closing-tag expect-fragment))

     ((= tt js2-LC)
      (rjsx-maybe-message "parsing expression { %s" (js2-peek-token))
      (rjsx-parse-wrapped-expr t nil))

     ((= tt rjsx-JSX-TEXT)
      (rjsx-maybe-message "text node: '%s'" (js2-current-token-string))
      (js2-set-face (js2-current-token-beg) (js2-current-token-end) 'rjsx-text 'record)
      (js2-record-text-property (js2-current-token-beg) (js2-current-token-end)
                                'syntax-table rjsx-text-syntax-table)
      (make-rjsx-text :value (js2-current-token-string)))

     ((= tt js2-ERROR)
      (make-js2-error-node :len (js2-current-token-len)))

     (t (error "Unexpected token type: %s" (js2-peek-token))))))

(defun rjsx-parse-xml-or-closing-tag (expect-fragment)
  "Parse a JSX tag, which could be a child or a closing tag.
Return the parsed child, which is a `rjsx-closing-tag' if a
closing tag was parsed.

EXPECT-FRAGMENT if t, indicates that `</>' should be parsed
as a fragment closing node, and not as an empty tag."
  (let ((beg (js2-current-token-beg)) pn)
    (rjsx-record-token-class '<)
    (rjsx-record-tag-bracket-face)
    (if (and (not expect-fragment) (setq pn (rjsx-parse-empty-tag)))
        pn
      (if (js2-match-token js2-DIV)
          (progn (rjsx-record-tag-bracket-face)
                 (if (and expect-fragment (eq (js2-peek-token) js2-GT))
                     (setq pn (make-rjsx-closing-tag :pos beg))
                   (setq pn (make-rjsx-closing-tag :pos beg
                                                   :name (rjsx-parse-member-or-ns 'rjsx-tag)))
                   (js2-node-add-children pn (rjsx-closing-tag-name pn)))
                 (when (js2-must-match js2-GT "msg.no.gt.in.closer" beg (- (js2-current-token-end) beg))
                   (rjsx-record-token-class '>)
                   (rjsx-record-tag-bracket-face))
                 (setf (js2-node-len pn) (- (js2-current-token-end) beg))
                 pn)
        (rjsx-maybe-message "parsing a child XML item")
        (rjsx-parse-xml)))))

(defun rjsx-get-next-xml-token ()
  "Scan through the XML text and push one token onto the stack."
  (setq js2-ts-string-buffer nil)  ; for recording the text
  (when (> js2-ti-lookahead 0)
    (setq js2-ts-cursor (js2-current-token-end))
    (setq js2-ti-lookahead 0))

  (let ((token (js2-new-token 0))
        c)
    (rjsx-maybe-message "Running the xml scanner")
    (catch 'return
      (while t
        (setq c (js2-get-char))
        (rjsx-maybe-message "'%s' (%s)" (if (= c js2-EOF_CHAR) "EOF" (char-to-string c)) c)
        (cond
         ((or (= c ?}) (= c ?>))
          (js2-set-string-from-buffer token)
          (setf (js2-token-type token) js2-ERROR)
          (js2-report-scan-error "msg.syntax" t)
          (throw 'return js2-ERROR))

         ((or (= c ?<) (= c ?{))
          (js2-unget-char)
          (if js2-ts-string-buffer
              (progn
                (js2-set-string-from-buffer token)
                (setf (js2-token-type token) rjsx-JSX-TEXT)
                (rjsx-maybe-message "created rjsx-JSX-TEXT token: `%s'" (js2-token-string token))
                (throw 'return rjsx-JSX-TEXT))
            (js2-get-char)
            (js2-set-string-from-buffer token)
            (setf (js2-token-type token) (if (= c ?<) js2-LT js2-LC))
            (setf (js2-token-string token) (string c))
            (throw 'return (js2-token-type token))))

         ((= c js2-EOF_CHAR)
          (js2-set-string-from-buffer token)
          (rjsx-maybe-message "Hit EOF. Current buffer: `%s'" (js2-token-string token))
          (setf (js2-token-type token) js2-ERROR)
          (rjsx-maybe-message "Scanner hit EOF. Panic!")
          (signal 'rjsx-eof-while-parsing nil))
         (t (js2-add-to-string c)))))))

(js2-deflocal rjsx-buffer-chars-modified-tick 0 "Variable holding the last per-buffer value of `buffer-chars-modified-tick'.")

(defun rjsx-maybe-reparse ()
  "Called before accessing the parse tree.
For small buffers, will do an immediate reparse to ensure the
parse tree is up to date."
  (when (and (<= (point-max) rjsx-max-size-for-frequent-reparse)
             (/= rjsx-buffer-chars-modified-tick (buffer-chars-modified-tick)))
    (js2-reparse)
    (setq rjsx-buffer-chars-modified-tick (buffer-chars-modified-tick))))

(defun rjsx--tag-at-point ()
  "Return the JSX tag at point, if any, or nil."
  (rjsx-maybe-reparse)
  (let ((node (js2-node-at-point (point) t)))
    (while (and node (not (rjsx-node-p node)))
      (setq node (js2-node-parent node)))
    node))


;;;; Interactive commands and keybindings
(defun rjsx-electric-lt (n)
  "Insert a context-sensitive less-than sign.
Optional prefix argument N indicates how many signs to insert.
If N is greater than one, no special handling takes place.
Otherwise, if the less-than sign would start a JSX block, it
inserts `</>' and places the cursor inside the new tag."
    (interactive "p")
    (if (/= n 1)
        (insert (make-string n ?<))
      (if (save-excursion
            (forward-comment most-negative-fixnum)
            (skip-chars-backward "\n\r")
            (or (= (point) (point-min))
                (memq (char-before) (append "=(?:>}&|{," nil))
                (let ((start (- (point) 6)))
                  (and (>= start (point-min))
                       (string= (buffer-substring start (point)) "return")))))
          (progn (insert "</>")
                 (backward-char 2))
        (insert "<"))))

(define-key rjsx-mode-map "<" 'rjsx-electric-lt)

(defun rjsx-expand-self-closing-tag (node)
  "Expand NODE into a balanced tag.
Assumes NODE is self-closing `rjsx-node', and that point is at
the self-closing slash."
  (delete-char 1)
  (search-forward ">")
  (save-excursion
    (insert "</" (rjsx-node-opening-tag-name node) ">")))

(defun rjsx-electric-gt (n)
  "Insert a context-sensitive greater-than sign.
Optional prefix argument N indicates how many signs to insert.
If N is greater than one, no special handling takes place.
Otherwise, if point is in a self-closing JSX tag just before the
slash, it creates a matching end-tag and places point just inside
the tags."
  (interactive "p")
  (if (or (/= n 1)
          (not (eq (get-char-property (point) 'rjsx-class) 'self-closing-slash)))
      (insert (make-string n ?>))
    (let ((node (rjsx--tag-at-point)))
      (if node
          (rjsx-expand-self-closing-tag node)
        (insert (make-string n ?>))))))

(define-key rjsx-mode-map ">" 'rjsx-electric-gt)

(defun rjsx-delete-creates-full-tag (n &optional killflag)
  "N and KILLFLAG are as in `delete-char'.
If N is 1 and KILLFLAG nil, checks to see if we're in a
self-closing tag about to delete the slash.  If so, deletes the
slash and inserts a matching end-tag."
  (interactive "p")
  (if (or killflag (/= 1 n) (not (eq (get-char-property (point) 'rjsx-class) 'self-closing-slash)))
      (if (called-interactively-p 'any)
	  (call-interactively 'delete-forward-char)
	(delete-char n killflag))
    (let ((node (rjsx--tag-at-point)))
      (if node
          (rjsx-expand-self-closing-tag node)
        (delete-char 1)))))

(define-key rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag)

(defun rjsx-rename-tag-at-point (new-name)
  "Prompt for a new name and modify the tag at point.
NEW-NAME is the name to give the tag."
  (interactive "sNew tag name: ")
  (let* ((tag (rjsx--tag-at-point))
         (closer (and tag (rjsx-node-closing-tag tag))))
    (cond
     ((null tag) (message "No JSX tag found at point"))
     ((null (rjsx-node-name tag))       ; fragment
      (cl-assert closer nil "Fragment has no closing-tag")
      (save-excursion
        (goto-char (+ 2 (js2-node-abs-pos closer)))
        (insert new-name)
        (goto-char (1+ (js2-node-abs-pos tag)))
        (insert new-name))
      (js2-reparse))
     (t
      (let* ((head (rjsx-node-name tag))
             (tail (when closer (rjsx-closing-tag-name closer)))
             beg end)
        (dolist (part (if tail (list tail head) (list head)))
          (setq beg (js2-node-abs-pos part)
                end (+ beg (js2-node-len part)))
          (delete-region beg end)
          (save-excursion (goto-char beg) (insert new-name)))
        (js2-reparse))))))

(define-key rjsx-mode-map (kbd "C-c C-r") 'rjsx-rename-tag-at-point)


(defun rjsx-jump-closing-tag ()
  "Go to closing tag of tag at point."
  (interactive)
  (let* ((tag (rjsx--tag-at-point))
         (closer (and tag (rjsx-node-closing-tag tag))))
    (cond
     ((null tag) (message "No JSX tag found at point"))
     ((null closer) (message "JSX tag is self closing"))
     (t
      (goto-char (+ 1 (js2-node-abs-pos closer)))))))


(defun rjsx-jump-opening-tag ()
  "Go to opening tag of tag at point."
  (interactive)
  (let ((tag (rjsx--tag-at-point)))
    (if (null tag) (message "No JSX tag found at point")
      (goto-char (+ 1 (js2-node-abs-pos tag))))))

(defun rjsx-jump-tag ()
  "Switch between opening and closing tag of tag at point."
  (interactive)
  (let* ((tag (rjsx--tag-at-point))
         (closer (and tag (rjsx-node-closing-tag tag))))
    (cond
     ((null tag) (message "No JSX tag found at point"))
     ((null closer) (message "No closing JSX tag found at point"))
     ((eq (line-number-at-pos (js2-node-abs-pos tag)) (line-number-at-pos)) (rjsx-jump-closing-tag))
     ((eq (line-number-at-pos (js2-node-abs-pos closer)) (line-number-at-pos)) (rjsx-jump-opening-tag))
     (t
      (rjsx-jump-opening-tag)))))



(define-key rjsx-mode-map (kbd "C-c C-j") 'rjsx-jump-tag)



;; Utilities

(defun rjsx-ancestor (node predicate)
  "Find an ancestor of NODE satisfying PREDICATE.

Search upwards from the parent of NODE for an ancestor where
PREDICATE returns t.  Returns nil if no ancestor satisfies
PREDICATE."
  (let ((ancestor (js2-node-parent node)))
    (while (and ancestor
                (not (funcall predicate ancestor)))
      (setq ancestor (js2-node-parent ancestor)))
    ancestor))

(cl-defun rjsx--prev-sibling (node)
  "Get the previous non-blank sibling of NODE."
  (let* ((parent (js2-node-parent node)) prev)
    (dolist (kid (rjsx-node-kids parent))
      (cond
       ((eq kid node) (cl-return-from rjsx--prev-sibling prev))
       ((and (rjsx-text-p kid) (string-blank-p (rjsx-text-value kid))))
       (t (setq prev kid))))))



;; Comment handling

(defun rjsx-comment-region-function (beg end &optional arg)
  (js2-mode-wait-for-parse
   (lambda ()
     (let* ((node (js2-node-at-point beg))
            (in-jsx (or (rjsx-node-p node)
                        (rjsx-ancestor node 'rjsx-node-p)))
            (use-jsx-comment (and (rjsx-node-p (js2-node-parent node))
                                  (or (rjsx-text-p node)
                                      (and (rjsx-node-p node)
                                           (= (js2-node-abs-pos node) beg))))))
       (cond (use-jsx-comment
              (let ((comment-start "{/*")
                    (comment-end "*/}"))
                (comment-normalize-vars)
                (comment-region-default beg end arg)))
             (in-jsx
              (let ((comment-start "/*")
                    (comment-end "*/"))
                (comment-normalize-vars)
                (if (rjsx-wrapped-expr-p node)
                    (if (js2-empty-expr-node-p (rjsx-wrapped-expr-child node))
                        (let ((comment-start "{/*")
                              (comment-end "*/}"))
                          (comment-normalize-vars)
                          (comment-region-default beg end arg))
                      (comment-region-default (1+ beg) (1- end) arg))
                  (comment-region-default beg end arg))))
             (t (comment-region-default beg end arg)))))))

(defun rjsx-maybe-unwrap-expr (beg end)
  (save-excursion
    (save-restriction
      (js2-reparse)
      (goto-char beg)
      (skip-chars-forward "[:space:]\n" end)
      (let* ((node (js2-node-at-point (point)))
             (parent (js2-node-parent node)))
        (when (and parent
                   (rjsx-wrapped-expr-p parent)
                   (rjsx-node-p (js2-node-parent parent))
                   (or (rjsx-text-p node) (rjsx-node-p node)))
          (let* ((expr-start (js2-node-abs-pos parent))
                 (expr-end (+ expr-start (js2-node-len parent)))
                 (body-start (1+ expr-start))
                 (body-end (1- expr-end))
                 (body-length (- body-end body-start)))
            (when (and (comment-only-p body-start beg)
                       (comment-only-p end body-end))
              (goto-char expr-start)
              (delete-char 1)
              (forward-char body-length)
              (delete-char 1))))))))

(defun rjsx-uncomment-region-function (beg end &optional _)
  (js2-mode-wait-for-parse
   (lambda ()
     (goto-char beg)
     (setq end (copy-marker end))
     (let (cs ts te ce matched-start)
       ;; find comment start
       (while (and (<= (point) end)
                   (setq matched-start
                         (and (re-search-forward comment-start-skip end t 1)
                              (match-string-no-properties 0))))
         ;; delete comment-start
         (setq cs (match-beginning 1))
         (setq ts (match-end 1))
         (goto-char cs)
         (delete-region cs ts)

         ;; delete comment-padding start
         (when (and comment-padding (looking-at (regexp-quote comment-padding)))
           (delete-region (point) (+ (point) (length comment-padding))))

         ;; find comment end
         (when (re-search-forward (if (string-match "//+" matched-start) "\n" "\\*/}?") end t 1)
           (setq te (or (match-beginning 1) (match-beginning 0)))
           (setq ce (or (match-end 1) (match-end 0)))
           (goto-char te)

           ;; delete commend-end if it's not a newline
           (unless (string= "\n" (match-string-no-properties 0))
             (delete-region te ce)

             ;; delete comment-padding end
             (when comment-padding
               (backward-char (length comment-padding))
               (when (looking-at (regexp-quote comment-padding))
                 (delete-region (point) (+ (point) (length comment-padding))))))

           ;; unescape inner comments if any
           (save-restriction
             (narrow-to-region cs (point))
             (comment-quote-nested "{/*" "*/}" t)))))

     (rjsx-maybe-unwrap-expr beg end)

     (set-marker end nil))))

(defun rjsx-comment-quote-nested-function (_ __ unp)
  (let ((re (concat "\\*\\(\\\\" (if unp "+" "*") "\\)/}?"
                    "\\|"
                    "{?/\\(\\\\" (if unp "+" "*") "\\)\\*")))
    (goto-char (point-min))
    (while (re-search-forward re (point-max) t 1)
      (let ((ceme (match-end 1))
            (csme (match-end 2)))
        (goto-char (or ceme csme))
        (if (and unp (>= (length (or (match-string-no-properties 1)
                                     (match-string-no-properties 2)))
                         1))
            (delete-char -1)
          (insert "\\"))))))

;;;###autoload
(defun rjsx-comment-dwim (arg)
  "RJSX implementation of `comment-dwim'. If called on a region,
this function delegates to `comment-or-uncomment-region'. If the
point is not in a JSX context, it delegates to the
`comment-dwim', otherwise it will comment the JSX AST node at
point using the apppriate comment delimiters.

For example: If point is on a JSX attribute or JSX expression, it
will comment the entire attribute using \"/* */\". , otherwise if
it's on a descendent JSX Element, it will use \"{/* */}\"
instead."
  (interactive "*P")
  (js2-mode-wait-for-parse
   (lambda ()
     (if (use-region-p)
         (comment-or-uncomment-region (region-beginning) (region-end) arg)
       (save-excursion
         (when (looking-at "[[:space:]]")
           (forward-whitespace 1))
         (let ((node (js2-node-at-point)))
           (cond
            ;; If inside a regular JS comment node, uncomment the node
            ((js2-comment-at-point)
             (uncomment-region (js2-node-abs-pos node) (js2-node-abs-end node)))
            ;; If looking at a commented JSXAttribute value, forward one char to
            ;; uncomment the body
            ((and (looking-at comment-start-skip)
                  (looking-at "{")
                  (rjsx-attr-p (js2-node-parent node)))
             (forward-char 1)
             (rjsx-comment-dwim arg))
            ;; If the entire line is a comment, uncomment it
            ((and (comment-only-p (point) (line-end-position))
                  (not (looking-at "[[:space:]]*$")))
             (uncomment-region (point) (line-end-position)))
            ;; If looking at JSXText, comment the current line
            ((rjsx-text-p node)
             (let ((comment-start "{/*")
                   (comment-end "*/}"))
               (comment-line 1)))
            ;; If looking at a JSXAttribute or a JSXSpreadAttribute, comment the
            ;; entire attribute with C-style comment
            ((or (rjsx-spread-p node)
                 (rjsx-ancestor node 'rjsx-spread-p)
                 (rjsx-attr-p node)
                 (and (js2-name-node-p node)
                      (rjsx-identifier-p (js2-node-parent node))
                      (rjsx-attr-p (js2-node-parent (js2-node-parent node))))
                 (and (rjsx-identifier-p node)
                      (rjsx-attr-p (js2-node-parent node)))
                 (and (js2-string-node-p node)
                      (rjsx-attr-p (js2-node-parent node))))
             (let ((node (or (and (rjsx-spread-p node) node)
                             (rjsx-ancestor node 'rjsx-spread-p)
                             (and (rjsx-attr-p node) node)
                             (rjsx-ancestor node 'rjsx-attr-p)))
                   (comment-start "/*")
                   (comment-end "*/"))
               (comment-region
                (js2-node-abs-pos node)
                (js2-node-abs-end node) arg)))
            ;; If looking at a JSXElement or JSXFragment, comment the entire
            ;; node with JSX comment if it's a child of one of the above,
            ;; otherwise just comment with C-style comment.
            ((or (rjsx-node-p node)
                 (rjsx-closing-tag-p node)
                 (rjsx-member-p node)
                 (rjsx-ancestor node 'rjsx-member-p))
             (let* ((node (or (and (rjsx-node-p node) node)
                              (rjsx-ancestor node 'rjsx-node-p)))
                    (parent-node-p (rjsx-node-p (js2-node-parent node)))
                    (closing (rjsx-node-closing-tag node))
                    (comment-start (if parent-node-p "{/*" "/*"))
                    (comment-end (if parent-node-p "*/}" "*/")))
               (comment-region
                (js2-node-abs-pos node)
                (js2-node-abs-end (or closing node)) arg)))
            ;; If looking at a JSX {expression} or is inside a JSX expression,
            ;; comment the body with regular C-style comment. If the body is
            ;; already commented, uncomment it. If on a multi line JSX
            ;; expression, just comment the line.
            ((or (rjsx-wrapped-expr-p node)
                 (rjsx-ancestor node 'rjsx-wrapped-expr-p))
             (let* ((expr (or (and (rjsx-wrapped-expr-p node) node)
                              (rjsx-ancestor node 'rjsx-wrapped-expr-p)))
                    ;; Can only happen as a child of an element or fragment, an
                    ;; empty JSX expression attribute value will result in an
                    ;; error node
                    (expr-empty (js2-empty-expr-node-p (rjsx-wrapped-expr-child expr)))
                    (body-start (1+ (js2-node-abs-pos expr)))
                    (body-end (1- (js2-node-abs-end expr)))
                    (expr-child-of-node (rjsx-node-p (js2-node-parent expr))))
               ;; If the body is all comment, uncomment it, otherwise if it's
               ;; empty, wrap it with a JSX comment
               (if (and expr-child-of-node expr-empty)
                   (if (or (= (1+ (js2-node-abs-pos expr)) (js2-node-abs-end expr)) ;; {}
                           (string-blank-p (buffer-substring-no-properties body-start body-end)))
                       (let ((comment-start "{/*")
                             (comment-end "*/}"))
                         (comment-region (js2-node-abs-pos expr) (js2-node-abs-end expr) arg))
                     (when (comment-only-p body-start body-end)
                       (uncomment-region body-start body-end arg)))
                 ;; If in a multi-line JSX expression, comment the line
                 (if (> (count-lines body-start body-end) 1)
                     (comment-dwim arg)
                   ;; Otherwise comment the entire body
                   (let ((comment-start "/*")
                         (comment-end "*/"))
                     (comment-region body-start body-end arg))))))
            ;; Otherwise just delegate to (comment-dwim)
            (t (comment-dwim arg)))))))))

(define-key rjsx-mode-map [remap comment-dwim] 'rjsx-comment-dwim)



;; Indentation

(defun rjsx--js--looking-at-operator-p-advice (orig-fun)
  "Advice for `js--looking-at-operator-p' (ORIG-FUN) to handle JSX properly."
  (if (memq (get-char-property (point) 'rjsx-class) '(< >))
      nil
    (apply orig-fun nil)))

(advice-add 'js--looking-at-operator-p :around #'rjsx--js--looking-at-operator-p-advice)

(defvar-local rjsx--indent-region-p nil
  "t when `indent-region' is running.")

(defvar-local rjsx--indent-running-offset 0
  "Running offset to add to the node POS at the beginning of
every line during `indent-region' so we don't have to reparse
after indenting every line to update the AST node positions.")

(defvar-local rjsx--node-abs-pos-cache nil
  "Cache for JSX nodes' indent levels.
Used during `indent-region' calls to avoid repeated
`js2-node-abs-pos' calls.")

(defun rjsx--node-abs-pos (node)
  "Caching version of `js2-node-abs-pos' for NODE."
  (if (null rjsx--node-abs-pos-cache)
      (js2-node-abs-pos node)
    (let ((cached (gethash node rjsx--node-abs-pos-cache)))
      (or cached
          (setf (gethash node rjsx--node-abs-pos-cache)
                (js2-node-abs-pos node))))))

(defsubst rjsx--node-indent-level (node)
  "Return the indent level for NODE."
  (save-excursion (goto-char (rjsx--node-abs-pos node)) (current-column)))

(defsubst rjsx--indent-line-to-offset (node offset)
  "Indent current line relative to NODE, which must be an `rjsx-node' instance.
OFFSET indicates the number of spaces to add."
  (indent-line-to
   (+ offset (rjsx--node-indent-level node))))

(defun rjsx-indent-line ()
  "Similar to `js-jsx-indent-line', but fixes indentation bug
with JSXElement after a JSX expression and arrow function. In
addition, the > of a multi-line open tag and the /> of a
multi-line self-closing tag are aligned to the beginning <. This
function also ensures everything inside a JSX context is indented
according to `js-indent-level' using spaces, this is due to the
limitation of `sgml-indent-line' not being able to indent with
tabs.

Fixes:
- https://github.com/mooz/js2-mode/issues/490
- https://github.com/mooz/js2-mode/issues/482
- https://github.com/mooz/js2-mode/issues/462
- https://github.com/mooz/js2-mode/issues/451
"
  (unless rjsx--indent-region-p
    (js2-reparse))
  (let ((delta (- (point) (progn (back-to-indentation) (point)))))
    (rjsx--indent-line-1)
    (back-to-indentation)
    (when (> delta 0)
      (forward-char delta))))

(defun rjsx--indent-line-1 ()
  "Helper for `rjsx-indent-line'."
  (let* ((indent-tabs-mode nil)
         (cur-pos (point))
         (cur-char (char-after cur-pos))
         (node (js2-node-at-point (- cur-pos rjsx--indent-running-offset)))
         (parent (js2-node-parent node)))
    (cond
     ((rjsx-node-p node)
      (cond
       ((eq cur-char ?<)
        (if (rjsx-node-p parent)
            (rjsx--indent-line-to-offset parent sgml-basic-offset)
          ;; Top-level node, indent as JS
          (js-indent-line))
        (when rjsx--node-abs-pos-cache
          (setf (gethash node rjsx--node-abs-pos-cache)
                (save-excursion (back-to-indentation) (point)))))
       ((memq cur-char '(?/ ?>))
        (rjsx--indent-line-to-offset node 0))
       ((eq cur-char ?\n)
        (rjsx--indent-line-to-offset node sgml-basic-offset))
       (t (error "Don't know how to indent %s for JSX node" (make-string 1 cur-char)))))
     ((and (rjsx-identifier-p parent)
           (rjsx-member-p (js2-node-parent parent))
           (rjsx-node-p (js2-node-parent (js2-node-parent parent))))
      (rjsx--indent-line-to-offset (js2-node-parent (js2-node-parent parent)) 0))

     ;; JSX children
     ((rjsx-closing-tag-p node)
      (rjsx--indent-line-to-offset parent 0))
     ((rjsx-text-p node)
      (rjsx--indent-line-to-offset parent sgml-basic-offset))
     ((rjsx-wrapped-expr-p node)
      (if (eq cur-char ?})
          (js-indent-line)
        (rjsx--indent-line-to-offset parent sgml-basic-offset)))

     ;; Attribute-like (spreads, attributes, etc.)
     ;; if first attr is on same line as tag, then align
     ;; otherwise indent to parent level + sgml-basic-offset
     ((or (rjsx-identifier-p node)
          (and (rjsx-identifier-p parent)
               (rjsx-attr-p (js2-node-parent parent)))
          (rjsx-spread-p node))
      (let* ((tag (or (rjsx-ancestor node #'rjsx-node-p)
                      (error "Did not find containing JSX tag for attributes")))
             (name (rjsx-node-name tag))
             column)
        (save-excursion
          (goto-char (rjsx--node-abs-pos tag))
          (setq column (current-column))
          (when name (forward-char (js2-node-end name)) (skip-chars-forward " \t"))
          (if (eolp)
              (setq column (+ column sgml-basic-offset sgml-attribute-offset))
            (setq column (current-column))))
        (indent-line-to column)))

     ;; Everything else indent as javascript
     (t (js-indent-line)))

    (when rjsx--indent-region-p
      (cl-incf rjsx--indent-running-offset
               (- (save-excursion (back-to-indentation) (point))
                  cur-pos)))))

(defun rjsx-indent-region (start end)
  "Indent the region from START to END as JS/JSX."
  (js2-reparse)
  (let ((rjsx--indent-region-p t)
        (rjsx--indent-running-offset 0)
        (rjsx--node-abs-pos-cache (make-hash-table)))
    (js2-indent-region start end)))


(provide 'rjsx-mode)
;;; rjsx-mode.el ends here

;; Local Variables:
;; outline-regexp: ";;;\\(;* [^
;; ]\\|###autoload\\)\\|(....."
;; End:
