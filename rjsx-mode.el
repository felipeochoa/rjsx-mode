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


;;Token types for XML nodes. Never returned by scanner
(defvar js2-JSX            (+ 1 js2-num-tokens))
(defvar js2-JSX-CLOSE      (+ 2 js2-num-tokens))
(defvar js2-JSX-IDENT      (+ 3 js2-num-tokens))
(defvar js2-JSX-MEMBER     (+ 4 js2-num-tokens))
(defvar js2-JSX-ATTR       (+ 5 js2-num-tokens))
(defvar js2-JSX-SPREAD     (+ 6 js2-num-tokens))
(defvar js2-JSX-TEXT       (+ 7 js2-num-tokens))
(defvar js2-JSX-EXPRESSION (+ 8 js2-num-tokens))

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


(defface jsx-tag
  '((t . (:inherit font-lock-function-name-face)))
  "`rjsx-mode' face used to highlight JSX tag names."
  :group 'rjsx-mode)

(defface jsx-attr
  '((t . (:inherit font-lock-variable-name-face)))
  "`rjsx-mode' face used to highlight JSX attribute names."
  :group 'rjsx-mode)


;; TODO: define js2-printers for all of the structs
(cl-defstruct (jsx-node
               (:include js2-node (type js2-JSX))
               (:constructor nil)
               (:constructor make-jsx-node
                             (&key (pos (js2-current-token-beg))
                                   len
                                   name
                                   jsx-props
                                   kids)))
  name      ; AST node containing the parsed xml name
  jsx-props ; linked list of AST nodes (both attributes and spreads)
  kids)     ; linked list of child xml nodes


(put 'cl-struct-jsx-node 'js2-visitor 'jsx-node-visit)
(defun jsx-node-visit (ast callback)
  "Visit the `jsx-node' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (jsx-node-name ast) callback)
  (dolist (prop (jsx-node-jsx-props ast))
    (js2-visit-ast prop callback))
  (dolist (prop (jsx-node-jsx-kids ast))
    (js2-visit-kids prop callback)))

(defun jsx-node-push-prop (n jsx-prop)
  "Push js2-node JSX-PROP onto the end of the jsx-node N's jsx-props.
Sets JSX-PROPS's parent to N."
  (let ((jsx-props (jsx-node-jsx-props n)))
    (if jsx-props
        (setcdr jsx-props (nconc (cdr jsx-props) (list jsx-prop)))
      (setf (jsx-node-jsx-props n) (list jsx-prop))))
  (js2-node-add-children n jsx-prop))

(defun jsx-node-push-child (n kid)
  "Push js2-node KID onto the end of the jsx-node N's children.
Sets KID's parent to N."
  (let ((kids (jsx-node-kids n)))
    (if kids
        (setcdr kids (nconc (cdr kids) (list kid)))
      (setf (jsx-node-kids n) (list kid))))
  (js2-node-add-children n kid))


(cl-defstruct (jsx-closing-tag
               (:include js2-node (type js2-JSX-CLOSE))
               (:constructor nil)
               (:constructor make-jsx-closing-tag (&key pos len name)))
  name) ; A jsx-identifier or jsx-member node

(put 'cl-struct-jsx-closing-tag 'js2-visitor 'jsx-closing-tag-visit)

(defun jsx-closing-tag-visit (ast callback)
  "Visit the `jsx-closing-tag' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (jsx-closing-tag-name ast) callback))

(defun jsx-closing-tag-full-name (n)
  "Return the string with N's fully-namespaced name, or just name if it's not namespaced."
  (let ((child (jsx-closing-tag-name n)))
    (if (jsx-member-p child)
        (jsx-member-full-name child)
      (jsx-identifier-full-name child))))

(cl-defstruct (jsx-identifier
               (:include js2-node (type js2-JSX-IDENT))
               (:constructor nil)
               (:constructor make-jsx-identifier (&key (pos (js2-current-token-beg))
                                                           len namespace name)))
  (namespace nil)
  name)

(put 'cl-struct-jsx-identifier 'js2-visitor 'js2-visit-none)

(defun jsx-identifier-full-name (n)
  "Return the string with N's fully-namespaced name, or just name if it's not namespaced."
  (if (jsx-identifier-namespace n)
      (format "%s:%s" (jsx-identifier-namespace n) (jsx-identifier-name n))
    (jsx-identifier-name n)))

(cl-defstruct (jsx-member
               (:include js2-node (type js2-JSX-MEMBER))
               (:constructor nil)
               (:constructor make-jsx-member (&key (pos len dots-pos idents))))
  dots-pos  ; List of positions of each dot
  idents)   ; List of jsx-identifier nodes

(put 'cl-struct-jsx-member 'js2-visitor 'js2-visit-none)

(defun jsx-member-full-name (n)
  "Return the string with N's combined names together."
  (mapconcat 'jsx-identifier-full-name (jsx-member-idents n) "."))

(cl-defstruct (jsx-attr
               (:include js2-node (type js2-JSX-ATTR))
               (:constructor nil)
               (:constructor make-jsx-attr (&key (pos (js2-current-token-beg))
                                                     len name value)))
  name    ; a jsx-identifier
  value)  ; a js2-expression

(put 'cl-struct-jsx-attr 'js2-visitor 'jsx-attr-visit)
(defun jsx-attr-visit (ast callback)
  "Visit the `jsx-attr' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (jsx-attr-name ast) callback)
  (js2-visit-ast (jsx-attr-value ast) callback))

(cl-defstruct (jsx-spread
               (:include js2-node (type js2-JSX-SPREAD))
               (:constructor nil)
               (:constructor make-jsx-spread (&key (pos (js2-current-token-beg))
                                                       len expr)))
  expr)  ; a js2-expression

(put 'cl-struct-jsx-spread 'js2-visitor 'jsx-spread-visit)
(defun jsx-spread-visit (ast callback)
  "Visit the `jsx-spread' children of AST, invoking CALLBACK on them."
  (js2-visit-ast (jsx-spread-expr ast) callback))

(cl-defstruct (jsx-text
               (:include js2-node (type js2-JSX-TEXT))
               (:constructor nil)
               (:constructor make-jsx-text (&key (pos (js2-current-token-beg))
                                                     (len (js2-current-token-len))
                                                     value)))
  value)  ; a string

(put 'cl-struct-jsx-text 'js2-visitor 'js2-visit-none)

(defvar rjsx-print-debug-message nil "If t will print out debug messages.")
;(setq rjsx-print-debug-message t)
(defun rjsx-maybe-message (&rest args)
  "If debug is enabled, call `message' with ARGS."
  (when rjsx-print-debug-message
    (apply #'message args)))

(js2-deflocal rjsx-in-xml nil "Variable used to track which xml parsing function is the outermost one.")
(defsubst rjsx-parent-tag () "The current xml tag being processed." (car rjsx-tags))

(defun rjsx-parse-top-xml ()
  "Parse a top level XML fragment.
This is the entry point when js2-parse-unary-expr finds a '<' character"
  (rjsx-maybe-message "Parsing a new xml fragment%s" (if rjsx-in-xml ", recursively" ""))
  ;; If there are imbalanced tags, we just need to bail out to the
  ;; topmost JSX parser and let js2 handle the EOF. Our custom scanner
  ;; will throw `t' if it finds the EOF, which it ordinarily wouldn't
  (let (pn)
    (when (catch 'jsx-eof-while-parsing
            (let ((rjsx-in-xml t)) ;; We use dynamic scope to handle xml > expr > xml nestings
              (setq pn (rjsx-parse-xml)))
            nil)
      (rjsx-maybe-message "Caught a signal. Rethrowing?: `%s'" rjsx-in-xml)
      (if rjsx-in-xml
          (throw 'jsx-eof-while-parsing t)
        (setq pn (make-js2-error-node))))
    (rjsx-maybe-message "Returning from top xml function: %s" pn)
    pn))

(defun rjsx-parse-xml ()
  "Parse a complete xml node from start to end tag."
  (let ((pn (make-jsx-node)) self-closing name-n name-str child child-name-str)
    ;; If there are parse errors here
    (rjsx-maybe-message "cleared <")
    (setf (jsx-node-name pn) (setq name-n (rjsx-parse-member-or-ns 'jsx-tag)))
    (if (js2-error-node-p name-n)
        (progn (rjsx-maybe-message "could not parse tag name")
               (make-js2-error-node :pos (js2-node-pos pn) :len (1+ (js2-node-len name-n))))
      (setq name-str (if (jsx-member-p name-n) (jsx-member-full-name name-n)
                       (jsx-identifier-full-name name-n)))
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
        (while (not (jsx-closing-tag-p (setq child (rjsx-parse-child))))
          ;; rjsx-parse-child calls our scanner, which always moves
          ;; forward at least one character. If it hits EOF, it
          ;; signals to our caller, so we don't have to worry about infinite loops here
          (jsx-node-push-child pn child)
          (if (= 0 (js2-node-len child)) ; TODO: use js2-recover-from-parse-errors
              (js2-get-token)))
        (setq child-name-str (jsx-closing-tag-full-name child))
        (unless (string= name-str child-name-str)
          (js2-report-error "msg.mismatched.close.tag" name-str (js2-node-pos child) (js2-node-len child)))
        (rjsx-maybe-message "cleared children for `%s'" name-str)
        (js2-node-add-children pn child))
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
      (jsx-node-push-prop parent attr))))


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
        (when dont-consume-rc
          (js2-unget-token))
        (if warning
            (js2-report-warning "msg.empty.expr" nil beg len)
          (js2-report-error "msg.empty.expr" nil beg len))
        (rjsx-maybe-message "Parsed empty {}")
        (make-js2-error-node :pos beg :len len)))))


(defun rjsx-parse-spread ()
  "Parse an {...props} attribute."
  (let ((pn (make-jsx-spread :pos (1- (js2-current-token-beg))))
        expr)
    (js2-must-match js2-TRIPLEDOT "msg.syntax")  ; Does not consume on error
    (setq expr (js2-parse-assign-expr))  ; No tokens consumed when error
    (if (js2-error-node-p expr)
        expr
      (unless (js2-match-token js2-RC t)  ; Won't consume on error
        (js2-report-error "msg.no.rc.after.spread" nil
                          (js2-node-pos pn)
                          (- (js2-current-token-end) (js2-node-pos pn))))
      (setf (jsx-spread-expr pn) expr)
      (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
      (js2-node-add-children pn expr)
      pn)))

(defun rjsx-parse-single-attr ()
  "Parse an 'a=b' JSX attribute and return the corresponding XML node."
  (let ((pn (make-jsx-attr)) name value beg)
    (setq name (rjsx-parse-identifier 'jsx-attr)) ; Won't consume token on error
    (if (js2-error-node-p name)
        name
      (setf (jsx-attr-name pn) name)
      (setq beg (js2-node-pos name))
      (js2-node-add-children pn name)
      (rjsx-maybe-message "Got the name for the attr: %s" (jsx-identifier-full-name name))
      (if (js2-match-token js2-ASSIGN)  ; Won't consume on error
          (if (js2-match-token js2-LC)
            (or (setq value (rjsx-check-for-empty-curlies))
                (prog1
                    (setq value (js2-parse-assign-expr))
                  (rjsx-maybe-message "parsed expression of type `%s': `%s'"
                                      (js2-node-type value)
                                      (with-temp-buffer
                                        (js2-print-ast value)
                                        (buffer-string)))
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
            (js2-report-error "msg.no.value.after.jsx.prop" (jsx-identifier-full-name name)
                              beg (- (js2-current-token-end) beg))
            (setq value (make-js2-error-node :pos beg :len (js2-current-token-len)))))
        (js2-report-error "msg.no.equals.after.jsx.prop" (jsx-identifier-full-name name)
                          beg (- (js2-current-token-end) beg))
        (setq value (make-js2-error-node :pos beg :len (js2-current-token-len))))
      (rjsx-maybe-message "value type: '%s'" (js2-node-type value))
      (setf (jsx-attr-value pn) value)
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
      (let ((pn (make-jsx-identifier))
            (beg (js2-current-token-beg))
            (name-parts (list (js2-current-token-string)))
            (allow-colon allow-ns)
            (continue t)
            matched-colon )
        (while (and continue
                    (or (= (js2-peek-token) js2-SUB)
                        (and allow-colon (= (js2-peek-token) js2-COLON))))

          (if (setq matched-colon (js2-match-token js2-COLON))
              (setf (jsx-identifier-namespace pn) (apply #'concat (nreverse name-parts))
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
              (jsx-identifier-name pn) (apply #'concat (nreverse name-parts)))
        pn)
    (make-js2-error-node :len (js2-current-token-len))))

(defun rjsx-parse-member-or-ns (&optional face)
  "Parse a dotted expression or a namespaced identifier and fontify with FACE if given."
  (let ((ident (rjsx-parse-identifier face)))
    (cond
     ((js2-error-node-p ident) ident)
     ((jsx-identifier-namespace ident) ident)
     (t (rjsx-parse-member ident face)))))

(defun rjsx-parse-member (ident &optional face)
  "Parse a dotted member expression and fontify with FACE if given.
IDENT is the `jsx-identifier' node for the first item in the
member expression.  Returns a `js2-error-node' if unable to
parse."
  (let (idents dots-pos pn end)
    (setq pn (make-jsx-member :pos (js2-node-pos ident)))
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
    (setf (jsx-member-idents pn) (nreverse idents)
          (jsx-member-dots-pos pn) (nreverse dots-pos)
          (js2-node-len pn) (- end (js2-node-pos pn)))
    (when face
      (js2-set-face (js2-node-pos pn) end face 'record))
    pn))


(defun rjsx-parse-child ()
  "Parse an XML child node.
Child nodes include plain (unquoted) text, other XML elements,
and {}-bracketed expressions.  Returns the parsed child, which is
a `jsx-identifier' if a closing tag was parsed."
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

     ((= tt js2-JSX-TEXT)
      (rjsx-maybe-message "text node: '%s'" (js2-current-token-string))
      (make-jsx-text :value (js2-current-token-string)))

     ((= tt js2-ERROR)
      (make-js2-error-node :len (js2-current-token-len)))

     (t (error "Unexpected token type: %s" (js2-peek-token))))))

(defun rjsx-parse-xml-or-closing-tag ()
  "Parse a JSX tag, which could be a child or a closing tag.
Returns the parsed child, which is a `jsx-closing-tag' if a
closing tag was parsed."
  (let ((beg (js2-current-token-beg)) pn)
    (if (js2-match-token js2-DIV)
        (progn (setq pn (make-jsx-closing-tag :pos beg :name (rjsx-parse-member-or-ns 'jsx-tag)))
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
                (setf (js2-token-type token) js2-JSX-TEXT)
                (rjsx-maybe-message "created js2-JSX-TEXT token: `%s'" (js2-token-string token))
                (throw 'return js2-JSX-TEXT))
            (js2-get-char)
            (js2-set-string-from-buffer token)
            (setf (js2-token-type token) (if (= c ?<) js2-LT js2-LC))
            (setf (js2-token-string token) (string c))
            (throw 'return (js2-token-type token))))

         ((= c js2-EOF_CHAR)
          (rjsx-maybe-message "Scanner hit EOF. Panic!")
          (throw 'jsx-eof-while-parsing t))
         (t (js2-add-to-string c)))))))

(provide 'rjsx-mode)
;;; rjsx-mode.el ends here
