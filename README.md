# rjsx-mode
[![Build Status](https://travis-ci.org/felipeochoa/rjsx-mode.svg?branch=master)](https://travis-ci.org/felipeochoa/rjsx-mode)

A major mode for editing JSX files.

This mode implements a JSX parser according to the
spec [here](https://facebook.github.io/jsx/). This mode derives from
`js2-jsx-mode`, extending its parser to support JSX syntax. This
means you get all of the `js2` features plus proper syntax checking
and highlighting of JSX code blocks.

Here's a screenshot:

![Actual syntax highlighting and no spurious errors!](demo.png)


## Installing

Currently you can use `(load-file "rjsx-mode.el")` to load the mode and then
use `(add-hook 'js2-jsx-mode-hook 'rjsx-mode)` if you want to automatically
enable it in `js2-jsx-mode`. Otherwise you can use `(rjsx-mode)` to enable
it on a per-buffer basis.

## Features

`js2-mode` does not include a JSX parser, but rather an E4X parser, which
means it gets confused with certain JSX constructs. This mode extends the
`js2` parser to support all JSX constructs and proper syntax highlighting.
Some things that `js2` cannot do that this mode can:

* Highlighting JSX tag names and attributes (using the `rjsx-tag` and 
  `rjsx-attr` faces)
* Parsing the spread operator `{...otherProps}`
* `&&` and `||` in child expressions `<div>{cond && <BigComponent/>}</div>
* Ternary expressions `<div>{toggle ? <ToggleOn /> : <ToggleOff />}</div>`

Additionally, since `rjsx-mode` extends the `js2` AST, utilities using the
parse tree gain access to the JSX structure.

## Bugs, contributing

Please submit any bugs or feature requests on the GitHub tracker.

**Note**: This mode does not add any indentation improvements to the one built
into `js-mode`. Pleae report all identation bugs there


## License

This project is licensed under the MIT license.
