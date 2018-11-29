# Some useful shortcuts for DrRacket.

Inspired by paredit

## Implemented:

Movement:
* `("c:m:f")` paredit-forward-sexp
* `("c:m:b")` paredit-backward-sexp
* `("c:m:d")` down-sexp ;rebind to `"c:m:d"`
* `("m:right")` forward-atom ;this is not paredit shortcuts, but alternative for forward-word
* `("m:left")` backward-atom ;ditto



Depth-Changing:
* `("m:s")` paredit-splice-sexp
* `("m:(")` paredit-wrap-round
* `("m:up")` paredit-splice-sexp-killing-backward
* `("m:down")` paredit-splice-sexp-killing-forward
* `("m:r")` paredit-raise-sexp
* `("m:?")` paredit-convolute-sexp

Slurpage & barfage
* `("c:right" "c:)" "c:]")` paredit-slurp-forward
* `("c:m:left" "c:(" "c:[")` paredit-slurp-backward
* `("c:left" "c:}")` paredit-barf-forward
* `("c:m:right" "c:{")` paredit-barf-backward

## Note:

  All the key bindings involving meta key `"m:"` can also be accessed
  using the Escape key, by pressing and releasing it before proceeding
  with the remaining keys, just like in Emacs. This is equivalent to
  replacing `"m:"` with prepended `"esc;"`.

  Moreover, `"m:"` can be accessed through `"?:a:"`(the Option Key) on MacOS.

  You can see the up-to-date list of all the key bindings applied on
  your platform, by selecting from DrRacket's menu Edit, Keybindings,
  Show Active Keybindings, and filtering the list with "paredit."

## Install:
```shell
  raco pkg install drracket-paredit
```

or manually:
  1. Download the "paredit.rkt"
  2. Open the 'Edit|Keybindings|Add user-defined-keybindings' dialog, select "paredit.rkt".
