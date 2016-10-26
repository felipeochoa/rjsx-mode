;;; rjsx-mode.el --- Real support for JSX in Emacs

;; Copyright (C) 2016 Felipe Ochoa

;; Author: Felipe Ochoa <felipe@fov.space>
;; URL: https://github.com/felipeochoa/rjsx-mode/
;; Version: 20161019
;; Package-Requires: ((js2-mode "20160623") (cl-lib "0.5"))

;;; Commentary:
;; Defines a minor mode `rjsx-mode' that swaps out js2-mode's
;; XML parsing (made for E4X) with a JSX parser

;;; Code:

(require 'cl-lib)
(require 'js2-mode)

(defgroup rjsx-mode nil
  "Real support for JSX in Emacs"
  :group 'js2-mode)

;;;###autoload
(define-minor-mode rjsx-mode
  "Enable highlighting and syntax checking of JSX snippets."
  :lighter ":RJSX"
  :group 'rjsx-mode)

(defun rjsx-parse-xml-initializer (orig-fun)
  "Dispatch the xml parser based on `rjsx-mode' being active or not.
This function is used to advise `js2-parse-xml-initializer' using
the `:around' combinator.  JS2-PARSER is the original XML parser."
  (if rjsx-mode
      (rjsx-parse-top-xml)
    (apply orig-fun nil)))

(advice-add 'js2-parse-xml-initializer :around #'rjsx-parse-xml-initializer)

(defun rjsx-unadvice-js2 ()
  "Remove the rjsx advice on the js2 parser. This will cause rjsx to stop working globally."
  (advice-remove 'js2-parse-xml-initializer #'rjsx-parse-xml-initializer))


;;Token types for XML nodes. Never returned by scanner
(defvar rjsx-JSX            (+ 1 js2-num-tokens))
(defvar rjsx-JSX-CLOSE      (+ 2 js2-num-tokens))
(defvar rjsx-JSX-IDENT      (+ 3 js2-num-tokens))
(defvar rjsx-JSX-MEMBER     (+ 4 js2-num-tokens))
(defvar rjsx-JSX-ATTR       (+ 5 js2-num-tokens))
(defvar rjsx-JSX-SPREAD     (+ 6 js2-num-tokens))
(defvar rjsx-JSX-TEXT       (+ 7 js2-num-tokens))
(defvar rjsx-JSX-EXPRESSION (+ 8 js2-num-tokens))

(js2-msg "msg.bad.jsx.ident" "invalid JSX identifier")
(js2-msg "msg.invalid.jsx.string" "invalid JSX string (cannot contain delimiter in string body)")
(js2-msg "msg.mismatched.close.tag" "mismatched closing JSX tag; expected `%s'")
(js2-msg "msg.no.gt.in.opener" "missing '>' in opening tag")
(js2-msg "msg.no.gt.in.closer" "missing '>' in closing tag")
(js2-msg "msg.no.gt.after.slash" "missing '>' after '/' in self-closing tag")
(js2-msg "msg.no.rc.after.spread" "missing '}' after spread-prop")
(js2-msg "msg.no.equals.after.jsx.prop" "missing '=' after prop `%s'")
(js2-msg "msg.no.value.after.jsx.prop" "missing value after prop `%s'")
(js2-msg "msg.no.rc.after.expr" "missing '}' after expression")
(js2-msg "msg.empty.expr" "empty '{}' expression")


(defface rjsx-tag
  '((t . (:inherit font-lock-function-name-face)))
  "`rjsx-mode' face used to highlight JSX tag names."
  :group 'rjsx-mode)

(defface rjsx-attr
  '((t . (:inherit font-lock-variable-name-face)))
  "`rjsx-mode' face used to highlight JSX attribute names."
  :group 'rjsx-mode)


;; TODO: define js2-printers for all of the structs
(cl-defstruct (rjsx-node
               (:include js2-node (type rjsx-JSX))
               (:constructor nil)
               (:constructor make-rjsx-node
                             (&key (pos (js2-current-token-beg))
                                   len
                                   name
                                   rjsx-props
                                   kids)))
  name         ; AST node containing the parsed xml name
  rjsx-props    ; linked list of AST nodes (both attributes and spreads)
  kids         ; linked list of child xml nodes
  closing-tag) ; AST node with the tag closer


(put 'cl-struct-rjsx-node 'js2-visitor 'rjsx-node-visit)
(defun rjsx-node-visit (ast callback)
  "Visit the `rjsx-node' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-node-name ast) callback)
  (dolist (prop (rjsx-node-rjsx-props ast))
    (js2-visit-ast prop callback))
  (dolist (prop (rjsx-node-kids ast))
    (js2-visit-ast prop callback))
  (when (rjsx-node-closing-tag ast)
    (js2-visit-ast (rjsx-node-closing-tag ast))))

(defun rjsx-node-push-prop (n rjsx-prop)
  "Push js2-node JSX-PROP onto the end of the rjsx-node N's rjsx-props.
Sets JSX-PROPS's parent to N."
  (let ((rjsx-props (rjsx-node-rjsx-props n)))
    (if rjsx-props
        (setcdr rjsx-props (nconc (cdr rjsx-props) (list rjsx-prop)))
      (setf (rjsx-node-rjsx-props n) (list rjsx-prop))))
  (js2-node-add-children n rjsx-prop))

(defun rjsx-node-push-child (n kid)
  "Push js2-node KID onto the end of the rjsx-node N's children.
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
  name) ; A rjsx-identifier or rjsx-member node

(put 'cl-struct-rjsx-closing-tag 'js2-visitor 'rjsx-closing-tag-visit)

(defun rjsx-closing-tag-visit (ast callback)
  "Visit the `rjsx-closing-tag' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-closing-tag-name ast) callback))

(defun rjsx-closing-tag-full-name (n)
  "Return the string with N's fully-namespaced name, or just name if it's not namespaced."
  (let ((child (rjsx-closing-tag-name n)))
    (if (rjsx-member-p child)
        (rjsx-member-full-name child)
      (rjsx-identifier-full-name child))))

(cl-defstruct (rjsx-identifier
               (:include js2-node (type rjsx-JSX-IDENT))
               (:constructor nil)
               (:constructor make-rjsx-identifier (&key (pos (js2-current-token-beg))
                                                           len namespace name)))
  (namespace nil)
  name)

(put 'cl-struct-rjsx-identifier 'js2-visitor 'js2-visit-none)

(defun rjsx-identifier-full-name (n)
  "Return the string with N's fully-namespaced name, or just name if it's not namespaced."
  (if (rjsx-identifier-namespace n)
      (format "%s:%s" (rjsx-identifier-namespace n) (rjsx-identifier-name n))
    (rjsx-identifier-name n)))

(cl-defstruct (rjsx-member
               (:include js2-node (type rjsx-JSX-MEMBER))
               (:constructor nil)
               (:constructor make-rjsx-member (&key (pos len dots-pos idents))))
  dots-pos  ; List of positions of each dot
  idents)   ; List of rjsx-identifier nodes

(put 'cl-struct-rjsx-member 'js2-visitor 'js2-visit-none)

(defun rjsx-member-full-name (n)
  "Return the string with N's combined names together."
  (mapconcat 'rjsx-identifier-full-name (rjsx-member-idents n) "."))

(cl-defstruct (rjsx-attr
               (:include js2-node (type rjsx-JSX-ATTR))
               (:constructor nil)
               (:constructor make-rjsx-attr (&key (pos (js2-current-token-beg))
                                                     len name value)))
  name    ; a rjsx-identifier
  value)  ; a js2-expression

(put 'cl-struct-rjsx-attr 'js2-visitor 'rjsx-attr-visit)
(defun rjsx-attr-visit (ast callback)
  "Visit the `rjsx-attr' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-attr-name ast) callback)
  (js2-visit-ast (rjsx-attr-value ast) callback))

(cl-defstruct (rjsx-spread
               (:include js2-node (type rjsx-JSX-SPREAD))
               (:constructor nil)
               (:constructor make-rjsx-spread (&key (pos (js2-current-token-beg))
                                                       len expr)))
  expr)  ; a js2-expression

(put 'cl-struct-rjsx-spread 'js2-visitor 'rjsx-spread-visit)
(defun rjsx-spread-visit (ast callback)
  "Visit the `rjsx-spread' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (rjsx-spread-expr ast) callback))

(cl-defstruct (rjsx-text
               (:include js2-node (type rjsx-JSX-TEXT))
               (:constructor nil)
               (:constructor make-rjsx-text (&key (pos (js2-current-token-beg))
                                                     (len (js2-current-token-len))
                                                     value)))
  value)  ; a string

(put 'cl-struct-rjsx-text 'js2-visitor 'js2-visit-none)

(defvar rjsx-print-debug-message nil "If t will print out debug messages.")
;(setq rjsx-print-debug-message t)
(defmacro rjsx-maybe-message (&rest args)
  "If debug is enabled, call `message' with ARGS."
  `(when rjsx-print-debug-message
     (message ,@args)))


(js2-deflocal rjsx-in-xml nil "Variable used to track which xml parsing function is the outermost one.")

(defun rjsx-parse-top-xml ()
  "Parse a top level XML fragment.
This is the entry point when js2-parse-unary-expr finds a '<' character"
  (rjsx-maybe-message "Parsing a new xml fragment%s" (if rjsx-in-xml ", recursively" ""))
  ;; If there are imbalanced tags, we just need to bail out to the
  ;; topmost JSX parser and let js2 handle the EOF. Our custom scanner
  ;; will throw `t' if it finds the EOF, which it ordinarily wouldn't
  (let (pn)
    (when (catch 'rjsx-eof-while-parsing
            (let ((rjsx-in-xml t)) ;; We use dynamic scope to handle xml > expr > xml nestings
              (setq pn (rjsx-parse-xml)))
            nil)
      (rjsx-maybe-message "Caught a signal. Rethrowing?: `%s'" rjsx-in-xml)
      (if rjsx-in-xml
          (throw 'rjsx-eof-while-parsing t)
        (setq pn (make-js2-error-node))))
    (rjsx-maybe-message "Returning from top xml function: %s" pn)
    pn))

(defun rjsx-parse-xml ()
  "Parse a complete xml node from start to end tag."
  (let ((pn (make-rjsx-node)) self-closing name-n name-str child child-name-str)
    ;; If there are parse errors here
    (rjsx-maybe-message "cleared <")
    (setf (rjsx-node-name pn) (setq name-n (rjsx-parse-member-or-ns 'rjsx-tag)))
    (if (js2-error-node-p name-n)
        (progn (rjsx-maybe-message "could not parse tag name")
               (make-js2-error-node :pos (js2-node-pos pn) :len (1+ (js2-node-len name-n))))
      (setq name-str (if (rjsx-member-p name-n) (rjsx-member-full-name name-n)
                       (rjsx-identifier-full-name name-n)))
      (rjsx-maybe-message "cleared tag name: '%s'" name-str)
      ;; Now parse the attributes
      (rjsx-parse-attributes pn)
      (rjsx-maybe-message "cleared attributes")
      (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
      ;; Now parse either a self closing tag or the end of the opening tag
      (rjsx-maybe-message "next type: %s" (js2-peek-token))
      (if (setq self-closing (js2-match-token js2-DIV))
          ;; TODO: make sure there's no whitespace between / and >
          (js2-must-match js2-GT "msg.no.gt.after.slash" (js2-node-pos pn) (js2-node-len pn))
        (js2-must-match js2-GT "msg.no.gt.in.opener" (js2-node-pos pn) (js2-node-len pn)))
      (rjsx-maybe-message "cleared opener closer, self-closing: %s" self-closing)
      (if self-closing
          (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
        (while (not (rjsx-closing-tag-p (setq child (rjsx-parse-child))))
          ;; rjsx-parse-child calls our scanner, which always moves
          ;; forward at least one character. If it hits EOF, it
          ;; signals to our caller, so we don't have to worry about infinite loops here
          (rjsx-node-push-child pn child)
          (if (= 0 (js2-node-len child)) ; TODO: use js2-recover-from-parse-errors
              (js2-get-token)))
        (setq child-name-str (rjsx-closing-tag-full-name child))
        (unless (string= name-str child-name-str)
          (js2-report-error "msg.mismatched.close.tag" name-str (js2-node-pos child) (js2-node-len child)))
        (rjsx-maybe-message "cleared children for `%s'" name-str)
        (js2-node-add-children pn child)
        (setf (rjsx-node-closing-tag pn) child))
      pn)))

(defun rjsx-parse-attributes (parent)
  "Parse all attributes, including key=value and {...spread}, and add them to PARENT."
  ;; Getting this function to not hang in the loop proved tricky. The
  ;; key is that `rjsx-parse-spread' and `rjsx-parse-single-attr' both
  ;; return `js2-error-node's if they fail to consume any tokens,
  ;; which signals to us that we just need to discard one token and
  ;; keep going.
  (let (attr
        (max-iter 100)
        ; MAX-ITER is an awful hack, and I think superfluous at this
        ; point, but C-g hasn't been working for me and it's a huge
        ; headache when trying to use this mode. Once we have tests
        ; this can go easily
        (loop-terminators (list js2-DIV js2-GT js2-EOF js2-ERROR)))
    (while (not (memql (js2-peek-token) loop-terminators))
      (rjsx-maybe-message "Starting loop. Next token type: %s\nToken pos: %s" (js2-peek-token) (js2-current-token-beg))
      (when (= 0 (cl-decf max-iter)) (error "Too many iterations"))
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
If DONT-CONSUME-RC is true, the matched right curly token won't
be consumed.  Returns a `js2-error-node' if the curlies are empty
or nil otherwise.  If CHECK-FOR-COMMENTS (a &KEY argument) is t,
this will check for comments inside the curlies and returns the
first one found, if any.  If WARNING (a &key argument) is t,
reports the empty curlies as a warning and not an error.  Assumes
the current token is a '{'."
  (let ((beg (js2-current-token-beg)) end len found-comment)
    (when (js2-match-token js2-RC)
      (setq end (js2-current-token-end))
      (setq len (- end beg))
      (when dont-consume-rc
        (js2-unget-token))
      (if check-for-comments (rjsx-maybe-message "Checking for comments between %d and %d" beg end))
      (unless (and check-for-comments
                   (loop for comment in js2-scanned-comments
                         ;; TODO: IF comments are in reverse document order, we should be able to
                         ;; bail out early
                         do (rjsx-maybe-message "Comment at %d, length=%d"
                                                (js2-node-pos comment)
                                                (js2-node-len comment))
                         if (and (>= (js2-node-pos comment) beg)
                                 (<= (+ (js2-node-pos comment) (js2-node-len comment)) end))
                         do (cl-return-from rjsx-check-for-empty-curlies comment)))
        (if warning
            (js2-report-warning "msg.empty.expr" nil beg len)
          (js2-report-error "msg.empty.expr" nil beg len))
        (rjsx-maybe-message "Parsed empty {}")
        (make-js2-error-node :pos beg :len len)))))


(defun rjsx-parse-spread ()
  "Parse an {...props} attribute."
  (let ((pn (make-rjsx-spread :pos (js2-current-token-beg)))
        expr)
    (js2-must-match js2-TRIPLEDOT "msg.syntax")  ; Does not consume on error
    (setq expr (js2-parse-assign-expr))  ; No tokens consumed when error
    (if (js2-error-node-p expr)
        expr
      (unless (js2-match-token js2-RC t)  ; Won't consume on error
        (js2-report-error "msg.no.rc.after.spread" nil
                          (js2-node-pos pn)
                          (- (js2-current-token-end) (js2-node-pos pn))))
      (setf (rjsx-spread-expr pn) expr)
      (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
      (js2-node-add-children pn expr)
      pn)))

(defun rjsx-parse-single-attr ()
  "Parse an 'a=b' JSX attribute and return the corresponding XML node."
  (let ((pn (make-rjsx-attr)) name value beg)
    (setq name (rjsx-parse-identifier 'rjsx-attr)) ; Won't consume token on error
    (if (js2-error-node-p name)
        name
      (setf (rjsx-attr-name pn) name)
      (setq beg (js2-node-pos name))
      (js2-node-add-children pn name)
      (rjsx-maybe-message "Got the name for the attr: %s" (rjsx-identifier-full-name name))
      (if (js2-match-token js2-ASSIGN)  ; Won't consume on error
          (if (js2-match-token js2-LC)
            (or (setq value (rjsx-check-for-empty-curlies))
                (prog1
                    (setq value (js2-parse-assign-expr))
                  (if (js2-match-token js2-RC)
                      (rjsx-maybe-message "matched RC")
                    (while (not (memql (js2-get-token) (list js2-RC js2-EOF js2-DIV js2-GT)))
                      (rjsx-maybe-message "Skipped over `%s'" (js2-current-token-string)))
                    (when (memq (js2-current-token-type) (list js2-DIV js2-GT))
                      (js2-unget-token))
                    ; TODO: these error positions can be wrong if there's whitespace around the curlies
                    (js2-report-error "msg.no.rc.after.spread" nil (1- (js2-node-pos value))
                                      (- (js2-current-token-end) (js2-node-pos value) -1)))))
          (if (js2-match-token js2-STRING)
              (setq value (rjsx-parse-string))
            (js2-report-error "msg.no.value.after.jsx.prop" (rjsx-identifier-full-name name)
                              beg (- (js2-current-token-end) beg))
            (setq value (make-js2-error-node :pos beg :len (js2-current-token-len)))))
        (js2-report-error "msg.no.equals.after.jsx.prop" (rjsx-identifier-full-name name)
                          beg (- (js2-current-token-end) beg))
        (setq value (make-js2-error-node :pos beg :len (js2-current-token-len))))
      (rjsx-maybe-message "value type: '%s'" (js2-node-type value))
      (setf (rjsx-attr-value pn) value)
      (setf (js2-node-len pn) (- (js2-node-end value) (js2-node-pos pn)))
      (js2-node-add-children pn value)
      (rjsx-maybe-message "Finished single attribute.")
      pn)))

(defun rjsx-parse-string ()
  "Verify that current token is a valid JSX string.
Returns a `js2-error-node' if TOKEN-STRING is not a valid JSX
string, otherwise returns a `js2-string-node'.  (Strings are
invalid if they contain the delimiting quote character inside)"
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
            matched-colon )
        (while (and continue
                    (or (= (js2-peek-token) js2-SUB)
                        (and allow-colon (= (js2-peek-token) js2-COLON))))

          (if (setq matched-colon (js2-match-token js2-COLON))
              (setf (rjsx-identifier-namespace pn) (apply #'concat (nreverse name-parts))
                    allow-colon nil
                    name-parts (list))
            (js2-get-token) ;; Must be a js2-SUB
            (push "-" name-parts))
          (if (js2-match-token js2-NAME)
              (push (js2-current-token-string) name-parts)
            (when matched-colon
              (js2-report-error "msg.bad.jsx.ident" nil beg (- (js2-current-token-end) beg)))
            (setq continue nil)))
        (when face
          (js2-set-face beg (js2-current-token-end) face 'record))
        (setf (js2-node-len pn) (- (js2-current-token-end) beg)
              (rjsx-identifier-name pn) (apply #'concat (nreverse name-parts)))
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
  "Parse a dotted member expression and fontify with FACE if given.
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
    (setf (rjsx-member-idents pn) (nreverse idents)
          (rjsx-member-dots-pos pn) (nreverse dots-pos)
          (js2-node-len pn) (- end (js2-node-pos pn)))
    (when face
      (js2-set-face (js2-node-pos pn) end face 'record))
    pn))


(defun rjsx-parse-child ()
  "Parse an XML child node.
Child nodes include plain (unquoted) text, other XML elements,
and {}-bracketed expressions.  Returns the parsed child, which is
a `rjsx-identifier' if a closing tag was parsed."
  (let ((tt (rjsx-get-next-xml-token)) child)
    (rjsx-maybe-message "child type `%s'" tt)
    (cond
     ((= tt js2-LT)
      (rjsx-maybe-message "xml-or-close")
      (rjsx-parse-xml-or-closing-tag))

     ((= tt js2-LC)
      ;; TODO: Wrap this up in a JSX-EXPR node
      (rjsx-maybe-message "parsing expression { %s" (js2-peek-token))
      (or (setq child (rjsx-check-for-empty-curlies nil :check-for-comments t :warning t))
          (progn
            (setq child (js2-parse-assign-expr))
            (if (js2-must-match js2-RC "msg.no.rc.after.expr")
                (rjsx-maybe-message "matched } after expression")
              (rjsx-maybe-message "did not match } after expression"))))
      (rjsx-maybe-message "parsed expression, type: `%s'"
                          (js2-node-type child))
      child)

     ((= tt rjsx-JSX-TEXT)
      (rjsx-maybe-message "text node: '%s'" (js2-current-token-string))
      (make-rjsx-text :value (js2-current-token-string)))

     ((= tt js2-ERROR)
      (make-js2-error-node :len (js2-current-token-len)))

     (t (error "Unexpected token type: %s" (js2-peek-token))))))

(defun rjsx-parse-xml-or-closing-tag ()
  "Parse a JSX tag, which could be a child or a closing tag.
Returns the parsed child, which is a `rjsx-closing-tag' if a
closing tag was parsed."
  (let ((beg (js2-current-token-beg)) pn)
    (if (js2-match-token js2-DIV)
        (progn (setq pn (make-rjsx-closing-tag :pos beg :name (rjsx-parse-member-or-ns 'rjsx-tag)))
               (if (js2-must-match js2-GT "msg.no.gt.in.closer" beg (- (js2-current-token-end) beg))
                   (rjsx-maybe-message "parsed closing tag")
                 (rjsx-maybe-message "missing closing `>'"))
               (setf (js2-node-len pn) (- (js2-current-token-end) beg))
               pn)
      (rjsx-maybe-message "parsing a child XML item")
      (rjsx-parse-xml))))

(defun rjsx-get-next-xml-token ()
  "Scan through the XML text and push one token onto the stack."
  (setq js2-ts-string-buffer nil)  ; for recording the text
  (when (> js2-ti-lookahead 0)
    (setq js2-ts-cursor (js2-current-token-end))
    (setq js2-ti-lookahead 0))

  (let ((beg js2-ts-cursor)
        (token (js2-new-token 0))
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
          (setq continue nil)
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
          (rjsx-maybe-message "Scanner hit EOF. Panic!")
          (throw 'rjsx-eof-while-parsing t))
         (t (js2-add-to-string c)))))))

(provide 'rjsx-mode)
;;; rjsx-mode.el ends here
