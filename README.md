[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

# inf-clojure

This package provides basic interaction with a Clojure subprocess (REPL).
It's based on ideas from the popular `inferior-lisp` package.

`inf-clojure` has two components - a nice Clojure REPL with
auto-completion and a minor mode (`inf-clojure-minor-mode`), which
extends `clojure-mode` with commands to evaluate forms directly in the
REPL.

For a more powerful/full-featured solution see [CIDER][].

## Installation

Available on all major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `inf-clojure` using the following command:

<kbd>M-x package-install [RET] inf-clojure [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```el
(unless (package-installed-p 'inf-clojure)
  (package-refresh-contents)
  (package-install 'inf-clojure))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

Add the following to your Emacs config to enable
`inf-clojure-minor-mode` for Clojure source buffers:

```el
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
```

**Don't enable `inf-clojure-minor-mode` and `cider-mode` at the same
time.  They have overlapping functionality and keybindings and the
result will be nothing short of havoc.**

## Usage

`M-x inf-clojure` or `C-c C-z` within a Clojure source file.

`inf-clojure` has several custom variables which control the command
used to start a REPL for particular project type - `inf-clojure-lein-cmd`,
`inf-clojure-boot-cmd` and `inf-clojure-generic-cmd`.

By default all those variables are set to strings (e.g. `lein repl`).
However, it is possible to use a cons pair like `("localhost" . 5555)`
to connect to a socket REPL like the one provided
with [planck](http://planck-repl.org/), which can be started from the
command line with `planck -n 5555`.

Use `C-u C-c C-z` to start a REPL with a different command/cons pair than
the default specified in `inf-clojure-program`.

You can set custom values to `inf-clojure` variables on a per-project basis using [directory
variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).

## REPL Type

An `inf-clojure` REPL can be of different types: Clojure, ClojureScript, Lumo and Planck are all potentially valid options.
At the moment, the default Clojure REPL and the Lumo REPL (though partially, see https://github.com/clojure-emacs/inf-clojure/pull/44) are supported.

To hook up a custom REPL type, just use the right launch command (or connect through socket).
For example, for Lumo just add the following in your `.dir-locals.el`:

    ((nil . ((inf-clojure-boot-cmd . "lumo")))) ;; inf-clojure-lein-cmd if you are using Leiningen

## ElDoc

`eldoc-mode` is supported in Clojure source buffers and `*inferior-clojure*`
buffers which are running a Clojure REPL.

When ElDoc is enabled and there is an active REPL, it will show the
argument list of the function call you are currently editing in the
echo area.

You can activate ElDoc with `M-x eldoc-mode` or by adding the
following to you Emacs config:

```el
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)
```

ElDoc currently doesn't work with ClojureScript buffers and REPL's.
You can leave it enabled, it just won't show anything in the echo area.

## Troubleshooting

### REPL not responsive in Windows OS

In Windows, the REPL is not returning anything. For example, type `(+
1 1)` and press `ENTER`, the cursor just drops to a new line and
nothing is shown.

The explanation of this problem and solution can be found [here](https://groups.google.com/forum/#!topic/leiningen/48M-xvcI2Ng).

The solution is to create a file named `.jline.rc` in your `$HOME`
directory and add this line to that file:

```
jline.terminal=unsupported
```

## License

Copyright © 2014-2017 Bozhidar Batsov and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/inf-clojure-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/inf-clojure-badge.svg
[melpa-package]: http://melpa.org/#/inf-clojure
[melpa-stable-package]: http://stable.melpa.org/#/inf-clojure
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[badge-travis]: https://travis-ci.org/clojure-emacs/inf-clojure.svg?branch=master
[CIDER]: https://github.com/clojure-emacs/cider
[Leiningen]: http://leiningen.org
[contributors]: https://github.com/clojure-emacs/inf-clojure/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
