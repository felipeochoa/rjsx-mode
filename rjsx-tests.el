;;; tjsx-tests.el --- Tests for rjsx-mode.    -*- lexical-binding: t -*-

;; Copyright (C) 2016 Felipe Ochoa

;;;; Commentary:

;;

;;;; Code:

(load-file "./js2-tests.el")
(require 'rjsx-mode)

(defun js2-mode--and-parse ()  ;; No point in advising, so we just overwrite this internal function
  (js2-jsx-mode)
  (rjsx-mode)
  (js2-reparse))

(js2-deftest-parse no-attr-no-children-self-closing
  "<div/>;")

(js2-deftest-parse no-attr-no-children-self-closing
  "<div></div>;")

(js2-deftest-parse no-children-self-closing
  "<div a=\"1\" b={123} {...props}/>;")

(js2-deftest-parse no-attr-xml-child
  "<div><span/></div>;")

(js2-deftest-parse no-attr-text-child
  "<div>Hello world</div>;")

(js2-deftest-parse no-attr-expr-child
  "<div>{coolVar ? 'abc' : 'xyz'}</div>;")

(js2-deftest-parse ultra-nested
  "<div a={<span>{fall ? <b>Fell</b> : <img src=\"abc\"/>}</span>}></div>;")

(js2-deftest-parse hidden-behind-and
  "<div>{cond && <span/>}</div>;")

(js2-deftest-parse ns-tag
  "<xml:a/>;")

(js2-deftest-parse ns-tag-with-dashes
  "<xml-lmx-m:a-b-c/>;")

(js2-deftest-parse ns-tag-with-dashes-at-end
  "<xml-lmx-:a-b-/>;")

(js2-deftest-parse ns-attr
  "<xml:a lmx:attr=\"1\"/>;")

(js2-deftest-parse-expected-failure ns-attr-with-dashes-at-end
  ;; The js2 tokenizer parses '-=' as a single token, which makes this tough
  "<xml:a lmx:attr-=\"1\"/>;")

(js2-deftest-parse ns-attr-with-dashes-at-end-of-ns
  "<xml:a lmx-:attr=\"1\"/>;")

(js2-deftest-parse member-tag
  "<Module.Component/>;")

(js2-deftest-parse member-tag-many
  "<Module.Component.Sub1.Sub2/>;")

(js2-deftest-parse complex
  "<form onSubmit={this.handleSubmit} className={className}>
  <input type=\"text\"
         onChange={this.getChangeHandler(\"name\")}
         placeholder=\"Project name\"
         className={errors.name ? \"invalid\" : \"\"}
         ref={c => this._topInput = c}/>
    {errors.name && <span className=\"error\">{errors.name}</span>}
    {   } Empty is OK as child, but warning is issued
    {/* Node with comment gets no warning */}
    hello <div { } /> This should be a spread, so error
    <div empty={}  /> Empty attributes are not allowed
    {React.Children.count(this.props.children) === 1
        ? <OnlyChild {...this.props}>{this.props.children}</OnlyChild>
        : React.Children.map(this.props.children, (child, index) => (
            <li key={index} undefinedProp={notDefined}>
              {index === 0 && <span className=\"first\"/>}
              {React.cloneElement(child, {toolTip: <Tooltip index={index} />})}
            </li>
        ))
    }
</form>"
  :errors-count 2
  :warnings-count 1
  :syntax-error "{ }")

(js2-deftest-parse empty-child
  "<div>{}</div>;"
  :warnings-count 1)

(js2-deftest-parse empty-child-with-comment
  "<div>{/* this is a comment */}</div>;"
  :warnings-count 0
  :reference "<div>{}</div>;")

;;; Now we test all of the malformed bits:

(js2-deftest-parse mismatched-tags
  "<div></vid>"
  :syntax-error "</vid>")

(js2-deftest-parse-expected-failure missing-attr-last
  ;; The js2 tokenizer parses '=>' as one token
  "<div attr=></div>"
  :syntax-error "attr=")

(js2-deftest-parse missing-attr-last-self-closing
  "<div attr=/>"
  :syntax-error "attr=")

(js2-deftest-parse missing-attr
  "<div attr= attr2=\"2\"></div>"
  :syntax-error "attr=")

(js2-deftest-parse-expected-failure missing-attr-last-self-closing-last-dash
  "<div attr-=/>"
  :syntax-error "attr-=")

(js2-deftest-parse-expected-failure missing-attr-last-dash
  "<div attr-= attr2=\"2\"></div>"
  :syntax-error "attr-=")

(js2-deftest-parse missing-eq-after-att-self-closing
  "<div attr/>"
  :syntax-error "attr")

(js2-deftest-parse missing-eq-after-att
  "<div attr></div>"
  :syntax-error "attr")

(js2-deftest-parse missing-eq-after-att-self-closing-dashes
  "<div attr-name/>"
  :syntax-error "attr-name")

(js2-deftest-parse missing-eq-after-att-dashes
  "<div attr-name></div>"
  :syntax-error "attr-name")

(js2-deftest-parse invalid-tag-member-and-ns-self-closing
  "<xml:Component.Child/>"
  :errors-count 2 ; tag parsed as xml:Component, then erratic ., then Child as attr missing value
  :syntax-error ".") ;; TODO: report the error over the entire tag

(js2-deftest-parse invalid-ns-tag-with-double-dashes
  "<xml-lmx--m:a-b-c/>;"
  :errors-count 2 ; tag parsed as xml-lmx, then erratic decrement, then missing attr value
  :syntax-error "--")

(js2-deftest-parse invalid-jsx-string-in-attr
  "<div a=\"He said, \\\"Don't you worry child\\\"\"/>"
  :syntax-error "\"He said, \\\"Don't you worry child\\\"\"")

(js2-deftest-parse invalid-jsx-string-in-attr-single-quotes
  "<div a='He said, \"Don\\'t you worry child\"'/>"
  :syntax-error "'He said, \"Don\\'t you worry child\"'")

(js2-deftest-parse invalid-attr-value
  "<div a=123/>"
  :errors-count 2 ; missing attr value; erratic number
  :syntax-error "a=")

(js2-deftest-parse attr-missing-rc-at-end
  "<div a={123/>"
  :errors-count 2
  :syntax-error "{123/")

(js2-deftest-parse attr-missing-rc
  "<div a={123 b=\"1\"/>"
  :errors-count 1
  :syntax-error "{123 b=\"1\"")

(js2-deftest-parse invalid-spread-value
  "<div {...&&}/>"
  :errors-count 1
  :syntax-error "&")

(js2-deftest-parse spread-value-missing-dots
  "<div {props}/>"
  :errors-count 1
  :syntax-error "{props}")

(js2-deftest-parse spread-value-missing-rc
  "<div {...props a=\"1\"/>"
  :errors-count 1
  :syntax-error "{...props")

(js2-deftest-parse spread-value-missing-at-end
  "<div {...props/>"
  :errors-count 2
  :syntax-error "{...props/")

(js2-deftest-parse missing-closing-lt-self-closing
  "<div/"
  :syntax-error "<div/")

(js2-deftest-parse invalid-tag-name
  "<123 />"
  :errors-count 3
  :syntax-error "123")

(js2-deftest-parse invalid-tag-name-only-ns
  "<abc: />"
  :syntax-error "abc:")

(js2-deftest-parse invalid-attr-name-only-ns
  "<xyz abc:={1} />"
  :syntax-error "abc:")

(js2-deftest-parse falls-off-a-cliff-but-doesnt-hang
  "const Component = ({prop}) => <span>;\n\nexport default Component;"
  :syntax-error ";\n\nexport default Component;")

(js2-deftest-parse falls-off-a-cliff-but-doesnt-hang-even-with-braces
  "const Component = ({prop}) => <span>;\n\nexport { Component };"
  :syntax-error ";")

(js2-deftest-parse falls-off-a-cliff-but-doesnt-hang-even-with-other-jsx
  "const Component = ({prop}) => <span>;\nconst C2 = () => <span></span>\n\n"
  :errors-count 2  ; 1 from the stray > in the arrow function and 1 from the missing closer
  :syntax-error ";\nconst C2 = () =>")

(js2-deftest-parse falls-off-a-cliff-in-recursive-parse
  "const Component = ({prop}) => <div>{pred && <span>};\n\nexport { Component }"
  :errors-count 2
  :syntax-error "}")

;; Other odds and ends


(ert-deftest rjsx-node-opening-tag ()
  (ert-with-test-buffer (:name 'origin)
    (dolist (test '(("<div/>" "div" "div")
                    ("<div></div>" "div" "div")
                    ("<div></vid>" "div" "div")
                    ("<C-d-e:f-g-h-></C-d-e:f-g-h->" "C-d-e:f-g-h" "C-d-e:f-g-h")
                    ("<C.D.E></C.D.E>" "C.D.E" "C.D.E")
                    ("<C-a.D-a.E-a/>" "C-a.D-a.E-a" nil)))
      (erase-buffer)
      (js2-visit-ast
       (js2-test-string-to-ast (car test))
       (lambda (node end-p)
         (when (not end-p)
           (cond
            ((rjsx-node-p node)
             (should (string= (cadr test) (rjsx-node-opening-tag-name node))))
            ((rjsx-closing-tag-p node)
             (should (string= (caddr test) (rjsx-closing-tag-full-name node))))))
         nil)))))

;;; rjsx-tests.el ends here
