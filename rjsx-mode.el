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

;;;###autoload
(define-minor-mode rjsx-mode
  "Enable highlighting and syntax checking of JSX snippets."
  :lighter ":RJSX"
  (if rjsx-mode
      (advice-add 'js2-parse-xml-initializer :override #'rjsx-parse-top-xml)
    ;; TODO: should we set js2-compiler-xml-available?
    (advice-remove 'js2-parse-xml-initializer #'rjsx-parse-top-xml)))

;;Token types for XML nodes. Never returned by scanner
(defvar js2-JSX            (+ 1 js2-num-tokens))
(defvar js2-JSX-IDENT      (+ 2 js2-num-tokens))
(defvar js2-JSX-ATTR       (+ 3 js2-num-tokens))
(defvar js2-JSX-SPREAD     (+ 4 js2-num-tokens))
(defvar js2-JSX-TEXT       (+ 5 js2-num-tokens))
(defvar js2-JSX-EXPRESSION (+ 6 js2-num-tokens))

(js2-msg "msg.bad.jsx.ident" "invalid JSX identifier")
(js2-msg "msg.mismatched.close.tag" "mismatched closing JSX tag; expected '%s'")
(js2-msg "msg.no.gt.in.opener" "missing > in opening tag")
(js2-msg "msg.no.gt.in.closer" "missing > in closing tag")
(js2-msg "msg.no.gt.after.slash" "missing > after / in self-closing tag")
(js2-msg "msg.no.rc.after.spread" "missing } after spread-prop")
(js2-msg "msg.no.equals.after.jsx.prop" "missing = after prop name")
(js2-msg "msg.no.quotes.after.jsx.prop" "missing quoted value after prop name")
(js2-msg "msg.no.rc.after.expr" "missing } after expression")
(js2-msg "msg.empty.expr" "empty {} expression")

(defgroup rjsx-mode nil
  "Real support for JSX in Emacs"
  :group 'js2-mode)

(defface jsx-tag
  '((t . (:inherit font-lock-function-name-face)))
  "`rjsx-mode' face used to highlight JSX tag names."
  :group rjsx-mode)

(defface jsx-attr
  '((t . (:inherit font-lock-variable-name-face)))
  "`rjsx-mode' face used to highlight JSX attribute names."
  :group rjsx-mode)


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
(js2-deflocal rjsx-tags nil "Stack of tag names processed")
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
            (let ((rjsx-in-xml t)
                  (rjsx-tags (list))) ;; We use dynamic scope to handle xml > expr > xml nestings
              (setq pn (rjsx-parse-xml)))
            nil)
      (rjsx-maybe-message "Caught a signal. Rethrowing?: `%s'" rjsx-in-xml)
      (if rjsx-in-xml
          (throw 'jsx-eof-while-parsing t)
        (setq pn (make-js2-error-node))))
    (rjsx-maybe-message "Returning from top xml function")
    pn))

(defun rjsx-parse-xml ()
  "Parse a complete xml node from start to end tag."
  (let ((pn (make-jsx-node)) self-closing name-n curr-depth)
    ;; First parse the name and push it onto rjsx-tags
    ;; If there are parse errors here
    (rjsx-maybe-message "cleared <")
    (setf (jsx-node-name pn) (setq name-n (rjsx-parse-identifier 'jsx-tag)))
    (if (js2-error-node-p name-n)
        (rjsx-maybe-message "could not parse tag name")
      (rjsx-maybe-message "cleared tag name: '%s'" (jsx-identifier-full-name name-n)))
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
      (push (unless (= (js2-node-type name-n) js2-ERROR)
              (jsx-identifier-full-name name-n))
            rjsx-tags)
      (setq curr-depth (length rjsx-tags))
      (while (<= curr-depth (length rjsx-tags))
        ;; TODO: Why is this <= and not =??
        ;; rjsx-parse-child calls our scanner, which always moves
        ;; forward at least one character. If it hits EOF, it
        ;; signals to our caller, so we don't have to worry about infinite loops here
        (rjsx-parse-child pn))
      (if (js2-error-node-p name-n)
          (rjsx-maybe-message "cleared children for ERROR")
        (rjsx-maybe-message "cleared children for `%s'" (jsx-identifier-full-name name-n))))
    pn))

(defun rjsx-parse-attributes (parent)
  "Parse all attributes, including key=value and {...spread}, and add them to PARENT."
  ;; This function needs to take care not to be stuck in an infinite
  ;; loop during parser errors, so it tracks its position on every
  ;; iteration to make sure it's making progress
  (let (pos)
    (while (not (or (= (js2-peek-token) js2-DIV) (= (js2-peek-token) js2-GT)))
      (setq pos (js2-current-token-beg))
      (jsx-node-push-prop
       parent
       (if (js2-match-token js2-LC)
           (if (js2-match-token js2-RC)
               (js2-report-error "msg.empty.expr"
                                 (1- (js2-current-token-beg))
                                 (js2-current-token-end))
             (rjsx-parse-spread))
         (rjsx-parse-single-attr)))
      (when (= pos (js2-current-token-beg))
        (if js2-recover-from-parse-errors
            (js2-get-token)
          (error "Unable to parse JSX attributes"))))))

(defun rjsx-parse-spread ()
  "Parse an {...props} attribute."
  (let ((pn (make-jsx-spread :pos (1- (js2-current-token-beg))))
        expr)
    (js2-must-match js2-TRIPLEDOT "msg.syntax")  ; Does not consume on error
    (setq expr (js2-parse-assign-expr))  ; Assuming no tokens consumed when error
    (setf (jsx-spread-expr pn) expr)
    (js2-node-add-children pn expr)
    (unless (js2-match-token js2-RC t)  ; Won't consume on error
      (js2-report-error "msg.no.rc.after.spread"
                        (js2-node-pos pn)
                        (- (js2-current-token-end) (js2-node-pos pn))))
    (setf (js2-node-len pn) (- (js2-current-token-end) (js2-node-pos pn)))
    pn))

(defun rjsx-parse-single-attr ()
  "Parse an 'a=b' JSX attribute and return the corresponding XML node."
  (let ((pn (make-jsx-attr)) name value)
    (setq name (rjsx-parse-identifier 'jsx-attr)) ; Won't parse on error
    (setf (jsx-attr-name pn) name)
    (js2-node-add-children pn name)
    (setf (js2-node-pos pn) (js2-node-pos name))
    (rjsx-maybe-message "Got the name for the attr: %s" (if (= (js2-node-type name) js2-ERROR) "ERROR" (jsx-identifier-full-name name)))
    (if (js2-must-match js2-ASSIGN "msg.no.equals.after.jsx.prop")  ; Won't consume on error
        (setf (jsx-attr-value pn)
              (setq value
                    (if (js2-match-token js2-LC)
                        (prog1 (js2-parse-assign-expr)
                          ;; TODO: report the missing right curly starting at the left curly
                          (rjsx-maybe-message "parse expression")
                          (if (js2-must-match js2-RC "msg.syntax")
                              (rjsx-maybe-message "matched RC")
                            (rjsx-maybe-message "did not match RC")))
                      ;; TODO: JSX does not allow backslash escaped quotation marks inside strings
                      (when (js2-must-match js2-STRING "msg.no.quotes.after.jsx.prop")
                        (make-js2-string-node)))))
      (rjsx-maybe-message "Did not find an equals after the attribute"))
    (rjsx-maybe-message "value type: '%s'" (when value (js2-node-type value)))
    (setf (js2-node-len pn) (- js2-ts-cursor (js2-node-pos pn)))
    (rjsx-maybe-message "Finished single attribute.")
    (js2-node-add-children pn value)
    pn))

(defun rjsx-parse-identifier (&optional face)
  "Parse a possibly namespaced identifier and fontify with FACE if given.
Returns a JS2-ERROR-NODE if unable to parse."
  (if (js2-must-match-name "msg.bad.jsx.ident")
      (let ((pn (make-jsx-identifier))
            (beg (js2-current-token-beg))
            (name-parts (list (js2-current-token-string))))
        (when (js2-match-token js2-COLON)
          (setf (jsx-identifier-namespace pn) (car name-parts))
          (setq name-parts (list))
          (when (js2-must-match-name "msg.bad.jsx.ident")
            (push (js2-current-token-string) name-parts)))
        (while (js2-match-token js2-DOT)
          (push (js2-current-token-string) name-parts)
          (when (js2-must-match-name "msg.bad.jsx.ident")
            (push (js2-current-token-string) name-parts)))
        (when face
          (js2-set-face beg (js2-current-token-end) face 'record))
        (setf (js2-node-len pn) (- (js2-current-token-end) beg)
              (jsx-identifier-name pn) (apply #'concat (nreverse name-parts)))
        pn)
    (make-js2-error-node)))


(defun rjsx-parse-child (parent)
  "Parse an XML child node and add it to PARENT.
Child nodes include plain (unquoted) text, other XML elements,
and {}-bracketed expressions"
  (let ((tt (js2-get-next-xml-token)))
    (rjsx-maybe-message "child type `%s' for `%s'" tt (jsx-identifier-full-name (jsx-node-name parent)))
    (cond
      ((= tt js2-LT)       (rjsx-maybe-message "xml-or-close") (rjsx-parse-xml-or-closing-tag parent))
      ((= tt js2-LC)       (rjsx-maybe-message "parsing expression { %s" (js2-peek-token))
                           (if (js2-match-token js2-RC)
                               (js2-report-error "msg.empty.expr"
                                                 (1- (js2-current-token-beg))
                                                 (js2-current-token-end))
                             (jsx-node-push-child parent (js2-parse-assign-expr)))
                           (rjsx-maybe-message "parsed expression, type: `%s'" (js2-node-type (car (last (jsx-node-kids parent)))))
                           (js2-must-match js2-RC "msg.no.rc.after.expr")
                           (rjsx-maybe-message "maybe matched } after expression"))
      ((= tt js2-JSX-TEXT) (rjsx-maybe-message "text node: '%s'" (js2-current-token-string)) (jsx-node-push-child parent (make-jsx-text :value (js2-current-token-string))))
      ((= tt js2-ERROR))
      (t (error "Unexpected token type: %s" (js2-peek-token))))))

(defun rjsx-parse-xml-or-closing-tag (parent)
  "Parse a JSX tag, closing an open tag if necessary, adding the child or closing tag to PARENT."
  (let ((beg js2-ts-cursor) name)
    (if (js2-match-token js2-DIV)
        (progn (setq name (rjsx-parse-identifier 'jsx-tag))
               (rjsx-maybe-message "parsed closing name: `%s'"
                                  (if (js2-error-node-p name) "ERROR"
                                    (jsx-identifier-full-name name)))
               (if (or (null (rjsx-parent-tag)) (string= (rjsx-parent-tag)
                                                            (if (js2-error-node-p name) ""
                                                              (jsx-identifier-full-name name))))
                   (pop rjsx-tags)
                 (js2-report-error "msg.mismatched.close.tag" (pop rjsx-tags)))
               (if (js2-must-match js2-GT "msg.no.gt.in.closer" beg (- js2-ts-cursor beg))
                   (rjsx-maybe-message "parsed closing tag")
                 (rjsx-maybe-message "missing closing `>'"))
               (js2-node-add-children parent name)
               (setf (js2-node-len parent) (- js2-ts-cursor (js2-node-pos parent)))
               (rjsx-maybe-message "set closing tag"))
      (rjsx-maybe-message "parsing a child XML item")
      (jsx-node-push-child parent (rjsx-parse-xml)))))

(defun js2-get-next-xml-token ()
  "Scan through the XML text and push one token onto the stack."
  (setq js2-ts-string-buffer nil)  ; for recording the text
  (setq js2-ti-lookahead 0)
  (let ((beg js2-ts-cursor)
        (token (js2-new-token 0))
        c)
    (rjsx-maybe-message "Running the xml scanner")
    (catch 'return
      (while t
        (setq c (js2-get-char))
        (rjsx-maybe-message "'%s' (%s)" (if (= c js2-EOF_CHAR) "EOF" (char-to-string c)) c)
        (cond
         ((or (= c ?}) (= c ?>) (= c js2-EOF_CHAR))
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
            (throw 'return (js2-token-type token))))

         ((= c js2-EOF_CHAR) (throw 'jsx-eof-while-parsing t))
         (t (js2-add-to-string c)))))))

(provide 'rjsx-mode)
;;; rjsx-mode.el ends here
