# README #

This mode for [code-it-later](https://github.com/ccqpein/code-it-later-rs) emacs embedding. You can run the `codeitlater` in your emacs and jump to the file location.

## Install ##

**install code-it-later-rs**

`cargo install code-it-later-rs`

**use straight.el**

```elisp
(use-package code-it-later-mode
  :straight (code-it-later-mode :type git :host github :repo "ccqpein/code-it-later-mode")
  :commands code-it-later
  )
```

## Usage ##

`M-x code-it-later` and pick the dir or file you want to run `code-it-later`
