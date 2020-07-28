[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![Circle CI][circleci-badge]][circleci]

# inf-clojure

This package provides basic interaction with a Clojure subprocess (REPL).
It's based on ideas from the popular `inferior-lisp` package.

`inf-clojure` has two components - a nice REPL buffer (`inf-clojure`) and a REPL
interaction minor mode (`inf-clojure-minor-mode`), which extends `clojure-mode`
with commands to evaluate forms directly in the REPL.

**This documentation tracks the `master` branch of `inf-clojure`. Some of
the features and settings discussed here might not be available in
older releases (including the current stable release). Please, consult
the relevant git tag (e.g. 2.2.0) if you need documentation for a
specific `inf-clojure` release.**
***

- [Overview](#overview)
- [Rationale](#rationale)
- [Installation](#installation)
- [Basic Usage](#basic-usage)
- [Configuration](#configuration)
- [Troubleshooting](#troubleshooting)
- [License](#license)

## Overview

`inf-clojure` aims to expose the extensive self-documenting features of Clojure
REPLs via an Emacs package. `inf-clojure` is extremely simple and does not require special tooling.
It supports the following REPLs:

- Clojure
- ClojureScript
- [Planck](http://planck-repl.org/)
- [Lumo](https://github.com/anmonteiro/lumo)
- [Joker](https://joker-lang.org/)
- [babashka](https://github.com/borkdude/babashka)

`inf-clojure` provides a set of essential features for interactive
Clojure(Script) development:

* REPL
* Interactive code evaluation
* Code completion
* Definition lookup
* Documentation lookup
* ElDoc
* Apropos
* Macroexpansion
* Require `:reload`/`:reload-all`
* Support connecting to socket REPLs

For a more powerful/full-featured solution see [CIDER](https://github.com/clojure-emacs/cider).

## Rationale

`inf-clojure`'s goal is to provide the simplest possible way to interact with a Clojure REPL.
In Emacs terminology "inferior" process is a subprocess started by Emacs (it being the "superior" process, of course).

`inf-clojure` doesn't require much of setup, as at its core it simply runs a terminal REPL process, pipes input to it, and
processes its output. As the Clojure socket REPL works in exactly the same manner `inf-clojure` can also interact with it.

Functionality like code completion and eldoc is powered by evaluation of predefined code snippets that provide the necessary results.
As different Clojure REPLs have different capabilities, `inf-clojure` tracks the type of a REPL and invokes
the right code for each REPL type.

## Installation

Available on all major `package.el` community maintained repos -
[MELPA Stable][] and [MELPA][] repos.

MELPA Stable is recommended as it has the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run from a git checkout.

You can install `inf-clojure` using the following command:

<kbd>M-x package-install [RET] inf-clojure [RET]</kbd>

or if you'd rather keep it in your dotfiles:

```emacs-lisp
(unless (package-installed-p 'inf-clojure)
  (package-refresh-contents)
  (package-install 'inf-clojure))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

Add the following to your Emacs config to enable
`inf-clojure-minor-mode` for Clojure source buffers:

```emacs-lisp
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
```

**Don't enable `inf-clojure-minor-mode` and `cider-mode` at the same time. They
have overlapping functionality and keybindings and the result will be nothing
short of havoc.**

## Basic Usage

Just invoke `M-x inf-clojure` or press `C-c C-z` within a Clojure
source file. You should get a prompt with the supported REPL types and
common startup forms. You can select one of these or type in your own
custom startup. This will start a REPL process for the current project
and you can start interacting with it.

If you've already started a socket REPL server, use `M-x inf-clojure-connect`
and enter its host and port numbers.

Inf-clojure aims to be very simple and offer tooling that the REPL
itself exposes. A few commands are:

- eval last sexp `C-x C-e`
- show arglists for function `C-c C-a`
- show var documentation `C-c C-v`
- show source `C-c C-s`
- insert top level form into REPL `C-c C-j d`

For a list of all available commands in `inf-clojure-mode` (a.k.a. the
REPL) and `inf-clojure-minor-mode` you can either invoke `C-h f RET
inf-clojure-mode` and `C-h f RET inf-clojure-minor-mode` or simply
browse their menus.

Many `inf-clojure-minor-mode` commands by default act on the symbol at
point. You can, however, change this behaviour by invoking such
commands with a prefix argument. For instance: `C-u C-c C-v` will ask
for the symbol you want to show the docstring for.

## Configuration

**Note:** The configuration options were changed massively in `inf-clojure` 3.0.

In the time-honoured Emacs tradition `inf-clojure`'s behaviour is extremely
configurable.

You can set custom values to `inf-clojure` variables on a
per-project basis using [directory
variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
or by setting them in in your init file.

You can see all the configuration options available using the command
`M-x customize-group RET inf-clojure`.

### Startup

While `inf-clojure` is capable of starting many common REPLs out of the box, it's
fairly likely you will want to set some custom REPL startup command
(e.g. because you need to include some `tools.deps` profile) and the REPL type
that goes with it. This is most easily achieved with the following `dir-locals.el`:

```emacs-lisp
((nil
  (inf-clojure-custom-startup . "clojure -A:compliment")
  (inf-clojure-custom-repl-type . clojure)))
```

**Note:** This file has to be in the directory in which you're invoking `inf-clojure` or a parent
directory.

There are two important configuration variables here:

1. `inf-clojure-custom-startup`: Which startup command to use so
   inf-clojure can run the inferior Clojure process (REPL).
2. `inf-clojure-custom-repl-type`: The type of the REPL started by the above command (e.g. `lumo`).

If these are set and you wish to prevent inf-clojure from using them,
use a prefix arg when invoking `inf-clojure` (`C-u M-x inf-clojure`).

### REPL Features

The supported REPL-features are in an alist called
`inf-clojure-repl-features` and it has the following shape:

```emacs-lisp
'((cljs . ((doc . "(cljs.repl/doc %s)")
           (source . "(cljs.repl/source %s)")
           (arglists . "(try (->> '%s cljs.core/resolve cljs.core/meta :arglists) (catch :default _ nil))")
           (apropos . "(cljs.repl/apropos \"%s\")")
           (ns-vars . "(cljs.repl/dir %s)")
           (set-ns . "(in-ns '%s)")
           (macroexpand . "(cljs.core/macroexpand '%s)")
           (macroexpand-1 . "(cljs.core/macroexpand-1 '%s)"))))
```

If you want to add a new REPL type, just do something like:

``` emacs-lisp
(add-to-list 'inf-clojure-repl-features
             (cons new-repl-type '((doc . "(myrepl/doc-command %s")
                                   (source . "...")
                                   ...)))
```

The `inf-clojure-repl-features` data structure is just an
alist of alists, so you can manipulate it in numerous ways.

If you want to update a specific form there is a function
`inf-clojure-update-repl-feature` which can be used like so:

```emacs-lisp
(inf-clojure-update-feature 'clojure 'completion "(complete.core/completions \"%s\")")
```

#### Caveats

It is highly recommended to use a cons pair like `("localhost" . 5555)` to
connect to a socket REPL, terminal REPLs are inherently hard to work with and
support will be deprecated in the foreseeable future. If you use the
same project often, make a dir-locals file with this information in `inf-clojure-custom-startup`.

Note that if you decide _NOT_ to use the socket REPL, it is highly recommended
you disable output coloring and/or readline facilities: `inf-clojure` does not
filter out ASCII escape characters at the moment and will not behave correctly.

For leiningen, there are no command line switches and you need to add
a custom [`project.clj`
option](https://github.com/technomancy/leiningen/blob/master/sample.project.clj):

```clojure
...
  :repl-options {:color false}
...
```

#### Clojure Command Line Socket REPL

If you have the new [Clojure CLI tools][] installed you can use the `clojure` command:

_do not use `clj` because it adds readline support_

``` shellsession
$ clojure -J-Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}"
```

Then either `C-c M-c RET localhost RET 5555` from within Emacs or add the following to your `.dir-locals.el`:

```emacs-lisp
((nil . ((inf-clojure-custom-startup . ("localhost" . 5555)))))
```
#### Leiningen Socket REPL

For Leiningen, add the following option to your `~/.lein/profiles.clj` or your `project.clj`:

```clojure
:jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]
```

Then run `lein repl` from within your project directory to start the
REPL.  To connect, you can either `m-x inf-clojure-connect [RET]
localhost [RET] 5555` or you can put in a dir local file the
information on how connect:

```emacs-lisp
((nil (inf-clojure-custom-startup "localhost" . 5555)))
```

The socket server REPL configuration options are described [here](https://clojure.org/reference/repl_and_main#_launching_a_socket_server).

#### Lumo Socket REPL

Lumo is decoupled from `inf-clojure-project-type` and therefore the command used depends on what you are using for dependency resolution.

For example if a `project.clj` is present in the project root folder, `inf-clojure-lein-cmd` will be used.

After you launch `lumo ... -n 5555`, as customary, either `C-c M-c RET localhost RET 5555` from within Emacs or add the following to your `.dir-locals.el`:

```emacs-lisp
((nil (inf-clojure-custom-startup "localhost" . 5555)))
```

#### Multiple Process Support

To run multiple Clojure processes, you start the first up
with `inf-clojure`.  It will be in a buffer named `*inf-clojure*`.
Rename this buffer with `rename-buffer`.  You may now start up a new
process with another `inf-clojure`.  It will be in a new buffer,
named `*inf-clojure*`.  You can switch between the different process
buffers with `switch-to-buffer`.

Commands that send text from source buffers to Clojure processes (like `inf-clojure-eval-defun`
or `inf-clojure-show-arglists`) have to choose a process to send to, when you have more than
one Clojure process around. This is determined by the global variable `inf-clojure-buffer`.

Suppose you have three inferior Clojures running:

```
Buffer              Process
------              -------
foo                 inf-clojure
bar                 inf-clojure<2>
*inf-clojure*       inf-clojure<3>
```

If you do a `inf-clojure-eval-defun` command on some Clojure source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or `*inf-clojure*`),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inf-clojure-buffer`.

This process selection is performed by function `inf-clojure-proc`.
Whenever `inf-clojure` fires up a new process, it resets
`inf-clojure-buffer` to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you might need to change `inf-clojure-buffer` to
whichever process buffer you want to use.

#### REPL Type

An `inf-clojure` REPL has an associated type. The available types can be
obtained from `inf-clojure-repl-features`:

```emacs-lisp
(mapcar 'car inf-clojure-repl-features)

;; => (cljs lumo planck joker clojure babashka)
```

What does it mean that a REPL type is supported? Well, it means that
`inf-clojure` would use the proper Clojure(Script) code internally to power
commands like definition lookup and friends.  Those differ from REPL to REPL and
can't be implemented in a REPL-independent way. The REPL type is inferred on
startup when using the `inf-clojure` command or is specified manually when using
`inf-clojure-connect`.

#### ElDoc

**Note:** You can skip this section if you're using Emacs 26.1+, as `eldoc-mode`
is enabled by default there.

`eldoc-mode` is supported in Clojure source buffers and `*inferior-clojure*`
buffers which are running a Clojure REPL.

When ElDoc is enabled and there is an active REPL, it will show the
argument list of the function call you are currently editing in the
echo area.

You can activate ElDoc with `M-x eldoc-mode` or by adding the
following to you Emacs config:

```emacs-lisp
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'inf-clojure-mode-hook #'eldoc-mode)
```

ElDoc currently doesn't work with ClojureScript buffers and REPL's.
You can leave it enabled, it just won't show anything in the echo area.

#### Code Completion

Code completion is a tricky aspect if you are trying to be as close to
a generic REPL as possible. Planck and lumo REPL implementations
explicitly provide completion functions in their REPL namespaces. For
clojure, you will need to have a library on your classpath. If you are
using lein, you already have
[clojure-complete](https://github.com/ninjudd/clojure-complete). You
could alternatively use `compliment {:mvn/version "0.3.10"}`.

```emacs-lisp
;; for clojure-complete
(inf-clojure-update-feature 'clojure 'completion "(complete.core/completions \"%s\")")

;; or
;; for compliment
(inf-clojure-update-feature 'clojure 'completion "(compliment.core/completions \"%s\")")

```

If you give a form for the completion form, it is your responsibility
to ensure that this namespace is on the classpath and required. If
using lein, this is done for you with clojure-complete. If adding
compliment, the following sample deps.edn can conveniently add the dep
to your program.

Sample deps.edn:

```clojure
{:aliases {:compliment {:extra-deps {compliment {:mvn/version "0.3.10"}}}}}
```


Use the startup command: `clojure -A:compliment`. Then require the ns
once so that the completion machinery will work: `(require
'compliment.core)`. Now tab completion should work.

For more advanced customization, code completion is particularly open
to customization. Not only you can `setq` the customary
`inf-clojure-completion-form`, `inf-clojure-completion-form-lumo`,
`inf-clojure-completion-form-planck` and
`inf-clojure-completion-form-joker` - the form to send to the REPL -
but you can also use `inf-clojure-completions-fn` for specifying a
function that given the REPL response should return elisp data
compatible with
[`completion-at-point-functions`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html).
For more info run `M-x describe-variable RET
inf-clojure-completions-fn`.  Another option is to have a look at [how
cider does
it](https://github.com/clojure-emacs/cider/blob/3e9ed12e8cfbad04d7618e649322765dc9bff5d6/cider-interaction.el#L595).

#### Lumo Setup

For an optimal Lumo experience the `-d` needs to be passed to Lumo
when launched from the command line. This disable `readline` support
in order to play nicely with emacs.

## Troubleshooting

### Things seem broken

Inf-clojure is intentionally quite simple and just sends commands to a
REPL on your behalf to provide features. In order to do this
inf-clojure largely needs to know the REPL type so it can format the
correct calls. Most end up in `(lumo.repl/doc [symbol])` or
`(cljs.repl/doc ...)` so its important that the REPL type is set
correctly. This REPL type exists in the process buffer (REPL) and the
source buffers as a cache. If you have problems, run `m-x
inf-clojure-set-repl-type` from the source buffer to set the REPL type
in both buffers. To see how simple inf-clojure is, look at
`inf-clojure-repl-features` to see largely how things are laid out.

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

### Log process activity

Standard Emacs debugging turns out to be difficult when an asynchronous process is involved. In this case try to enable logging:

```emacs-lisp
(setq inf-clojure-log-activity t)
```

This creates `.inf-clojure.log` in the project directory so that you can `tail -f` on it.

## License

Copyright © 2014-2020 Bozhidar Batsov and [contributors][].

Distributed under the GNU General Public License; type <kbd>C-h C-c</kbd> to view it.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/inf-clojure-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/inf-clojure-badge.svg
[melpa-package]: http://melpa.org/#/inf-clojure
[melpa-stable-package]: http://stable.melpa.org/#/inf-clojure
[COPYING]: http://www.gnu.org/copyleft/gpl.html
[circleci]: https://circleci.com/gh/clojure-emacs/inf-clojure
[circleci-badge]: https://circleci.com/gh/clojure-emacs/inf-clojure.svg?style=svg
[CIDER]: https://github.com/clojure-emacs/cider
[Leiningen]: http://leiningen.org
[contributors]: https://github.com/clojure-emacs/inf-clojure/contributors
[melpa]: http://melpa.org
[melpa stable]: http://stable.melpa.org
[Emacs init file]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
[Clojure cli tools]: https://clojure.org/guides/getting_started
[Boot]: http://boot-clj.com
