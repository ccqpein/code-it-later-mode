# README #

This mode for [code-it-later](https://github.com/ccqpein/code-it-later-rs) emacs embedding. You can run the `codeitlater` in your emacs and jump to the file location.

## Install ##

**install code-it-later-rs**

`cargo install code-it-later-rs`

**use straight.el**

```elisp
(use-package helm-code-it-later
  :straight (helm-code-it-later :type git :host github :repo "ccqpein/helm-code-it-later")
  :commands helm-code-it-later
  )
```

## Usage ##

`M-x helm-code-it-later` and pick the dir or file you want to run `code-it-later`
