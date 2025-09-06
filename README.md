# mason.el - Work In Progress

mason.el is installer for LSP servers, DAP servers, linters and formatters,
inspired by [mason.nvim](https://github.com/mason-org/mason.nvim).

Package registry at [mason-org/mason-registry](https://github.com/mason-org/mason-registry).

Run `M-x mason-install RET` to install packages.

## Installation
### `use-package` with [Elpaca](https://github.com/progfolio/elpaca)
``` emacs-lisp
(use-package mason
  :ensure (:host github :repo "deirn/mason.el")
  :config
  (mason-ensure))
```
