# mason.el

mason.el is installer for LSP servers, DAP servers, linters and formatters,
inspired by [mason.nvim](https://github.com/mason-org/mason.nvim).

Package registry at [mason-org/mason-registry](https://github.com/mason-org/mason-registry).

- Run `M-x mason-install RET` to install packages.
- Run `M-x mason-manager RET` to open package manager.

## Installation
### `use-package` with [Elpaca](https://github.com/progfolio/elpaca)
``` emacs-lisp
(use-package mason
  :ensure (:host github :repo "deirn/mason.el")
  :config
  (mason-ensure))
```

## Screenshots
|                                             |                                           |
|:-------------------------------------------:|:-----------------------------------------:|
| ![Mason manager](docs/Screenshot-1.png)     | ![Package info](docs/Screenshot-2.png)    |
| `mason-manager`                             | Package info                              |
| ![M-x mason-install](docs/Screenshot-3.png) | ![`M-x mason-log`](docs/Screenshot-4.png) |
| `mason-install`                             | `mason-log`                               |
|                                             |                                           |
