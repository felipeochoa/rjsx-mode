# rjsx-mode
A real JSX minor mode for use with js2-mode

This mode implements a JSX parser according to the spec [here](https://facebook.github.io/jsx/). When enabled, it plugs the JSX parser into `js2-mode`'s own parser to build the JSX AST. Currently the mode handles syntax checking and highlighting, but does not handle indentation. For that you should use the `js2-jsx-mode` derived mode that comes with `j2` (`rjsx` works with either one).
