# rjsx-mode
A real JSX minor mode for use with js2-mode

This mode implements a JSX parser according to the
spec [here](https://facebook.github.io/jsx/). When enabled, it plugs
the JSX parser into `js2-mode`'s own parser to build the JSX
AST. Currently the mode handles syntax checking and highlighting, and
when you use `js2-jsx-mode` (bundled with `j2-mode`), also indentation.

Here's a screenshot:

![Actual syntax highlighting and no spurious errors!](demo.png)
