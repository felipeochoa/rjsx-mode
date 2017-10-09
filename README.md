# rjsx-mode: A major mode for editing JSX files
[![MELPA](https://melpa.org/packages/rjsx-mode-badge.svg)](https://melpa.org/#/rjsx-mode)
[![Build Status](https://travis-ci.org/felipeochoa/rjsx-mode.svg?branch=master)](https://travis-ci.org/felipeochoa/rjsx-mode)

This mode derives from `js2-jsx-mode`, extending its parser to support JSX syntax
according to the [official spec](https://facebook.github.io/jsx/).  This
means you get all of the `js2` features plus proper syntax checking
and highlighting of JSX code blocks.

Here's a screenshot of it in action:

<img src="demo.png" width="600" alt="Actual syntax highlighting and no spurious errors!">


## Installing

`rjsx-mode` is available on [Melpa](https://melpa.org/), so if you have that
repository configured you can just `package-list-packages` and install it from there.
(If not, you can follow [their guide](https://melpa.org/#/getting-started) on
getting started). `rjsx-mode` automatically registers itself for `*.jsx` files,
but you can use `(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))`

Alternatively, you can download `rjsx-mode.el` from this repository and use
`load-file` or similar to add it to your current session.

## Features

`js2-mode` does not include a JSX parser, but rather an E4X parser, which
means it gets confused with certain JSX constructs. This mode extends the
`js2` parser to support all JSX constructs and proper syntax highlighting.

`rjsx-mode` adds some electrcity to `<` and `C-d` to make adding new
JSX elements less repetitive:

* `<` inserts `</>` whenever it would start a new JSX node (and simply
  inserts `<` otherwise)
* `>` or `C-d` right before the slash in a self-closing tag
  automatically inserts a closing tag and places point inside the
  element

The result is you can do the following:

<img src="key-demo.gif" width="200" alt="Quickly and easily add new components">

* `C-c C-r` lets you rename the enclosing tag

Some features that this mode adds to `js2`:

* Highlighting JSX tag names and attributes (using the `rjsx-tag` and
  `rjsx-attr` faces)
* Parsing the spread operator `{...otherProps}`
* `&&` and `||` in child expressions `<div>{cond && <BigComponent/>}</div>`
* Ternary expressions `<div>{toggle ? <ToggleOn /> : <ToggleOff
  />}</div>`

If you don't like this behavior, you can disable it by adding the following to
your init file:

```elisp
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))
```

Additionally, since `rjsx-mode` extends the `js2` AST, utilities using the
parse tree gain access to the JSX structure.

## Bugs, contributing

Please submit any bugs or feature requests on the GitHub tracker.

**Note**: This mode does not add any indentation improvements to the one built
into `js-mode`. Please report all identation bugs there


## License

This project is licensed under the MIT license.
