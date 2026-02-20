[![Circle CI][circleci-badge]][circleci]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/inf-clojure.svg)](https://elpa.nongnu.org/nongnu/inf-clojure.html)
[![License GPL 3][badge-license]][copying]

# inf-clojure

This package provides basic interaction with a Clojure subprocess (REPL).
It's based on ideas from the popular `inferior-lisp` package.

`inf-clojure` has two components - a nice REPL buffer (`inf-clojure-mode`) and a REPL
interaction minor mode (`inf-clojure-minor-mode`), which extends `clojure-mode`
with commands to evaluate forms directly in the REPL.

> [!IMPORTANT]
>
> This documentation tracks the `master` branch of `inf-clojure`. Some of
> the features and settings discussed here might not be available in
> older releases (including the current stable release). Please, consult
> the relevant git tag (e.g. 2.2.0) if you need documentation for a
> specific `inf-clojure` release.

## Overview

`inf-clojure` aims to expose the extensive self-documenting features of Clojure
REPLs via an Emacs package. `inf-clojure` is extremely simple and does not require special tooling.
It supports the following REPLs:

- Clojure
- ClojureScript
- ClojureCLR (via [clr.tools.deps](https://github.com/clojure/clr.tools.deps))
- [Planck](https://github.com/planck-repl/planck)
- [Joker](https://joker-lang.org/)
- [babashka](https://github.com/babashka/babashka)

`inf-clojure` provides a set of essential features for interactive
Clojure/ClojureScript/ClojureCLR development:

- Enhanced REPL
- Interactive code evaluation
- Code completion
- Definition lookup
- Documentation lookup
- ElDoc
- Apropos
- Macroexpansion
- Reloading a namespace (via `require :reload`/`require :reload-all`)
- Connecting to socket REPLs

For a more powerful/full-featured solution see [CIDER][].

### inf-clojure vs CIDER

Both `inf-clojure` and [CIDER][] are Emacs packages for interactive Clojure
development, but they differ significantly in scope and approach:

| | inf-clojure | CIDER |
|---|---|---|
| **Architecture** | Talks to a standard REPL (process or socket) | Requires an [nREPL](https://nrepl.org) server + [cider-nrepl](https://github.com/clojure-emacs/cider-nrepl) middleware |
| **Setup** | Minimal — works out of the box with any Clojure REPL | Requires adding middleware dependencies to your project |
| **Code evaluation** | Yes | Yes |
| **Code completion** | Basic (requires a completion library on the classpath) | Rich, context-aware (via `compliment` or `clj-suitable`) |
| **Documentation lookup** | Yes | Yes, with more formatting options |
| **Source lookup** | Yes | Yes |
| **Debugger** | No | Yes (step debugger) |
| **Inspector** | No | Yes (value inspector) |
| **Test runner** | No | Yes (integrated test runner) |
| **Refactoring** | No | Via [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) |
| **Multiple REPLs** | Basic support | Rich support (per-project, sibling REPLs) |

**Choose `inf-clojure`** if you want a lightweight, simple setup that works with
any Clojure REPL (including socket REPLs) without adding project dependencies.

**Choose CIDER** if you want a full-featured IDE experience with debugging,
inspection, test running, and rich completion.

## Rationale

`inf-clojure`'s goal is to provide the simplest possible way to interact with a
Clojure REPL.  In Emacs terminology "inferior" process is a subprocess started
by Emacs (it being the "superior" process, of course).

`inf-clojure` doesn't require much of setup, as at its core it simply runs a
terminal REPL process, pipes input to it, and processes its output. As the
Clojure socket REPL works in exactly the same manner `inf-clojure` can also
interact with it.

Functionality like code completion and eldoc is powered by evaluation of
predefined code snippets that provide the necessary results.  As different
Clojure REPLs have different capabilities, `inf-clojure` tracks the type of a
REPL and invokes the right code for each REPL type.

`inf-clojure` is built on top of Emacs's
[comint](https://github.com/emacs-mirror/emacs/blob/master/lisp/comint.el). Unfortunately
`comint` is pretty light on official documentation, but there is a good
overview/tutorial
[here](https://www.masteringemacs.org/article/comint-writing-command-interpreter).

## Installation

> [!IMPORTANT]
>
> `inf-clojure` requires Emacs 28 or newer.

`inf-clojure` is available on the official [NonGNU ELPA](https://elpa.nongnu.org/nongnu/inf-clojure.html) `package.el` repo and on the community-maintained
[MELPA Stable][] and [MELPA][] repos.

NonGNU ELPA and MELPA Stable are recommended as they have the latest stable version.
MELPA has a development snapshot for users who don't mind breakage but
don't want to run `inf-clojure` from a git checkout.

You can install `inf-clojure` using the following command:

<kbd>M-x package-install [RET] inf-clojure [RET]</kbd>

or if you'd rather keep it in your Emacs config:

```emacs-lisp
(unless (package-installed-p 'inf-clojure)
  (package-refresh-contents)
  (package-install 'inf-clojure))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents</kbd>

`inf-clojure-minor-mode` will be auto-enabled for Clojure source buffers after you do
`M-x inf-clojure`. You can disable this behavior by setting `inf-clojure-auto-mode` to
`nil`.

You can also add the following to your Emacs config to enable
`inf-clojure-minor-mode` for Clojure source buffers, regardless of whether
there's an `inf-clojure` REPL running:

```emacs-lisp
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; or if you're a `clojure-ts-mode' user:

(add-hook 'clojure-ts-mode-hook #'inf-clojure-minor-mode)
```

> [!WARNING]
>
> Don't enable `inf-clojure-minor-mode` and `cider-mode` at the same time. They
> have overlapping functionality and keybindings and the result will be nothing
> short of havoc.

## Basic Usage

Just invoke `M-x inf-clojure` or press `C-c C-z` within a Clojure
source file. You should get a prompt with the supported REPL types and
common startup forms. You can select one of these or type in your own
custom startup. This will start a REPL process for the current project
and you can start interacting with it.

If you want to use a socket REPL server, use `M-x inf-clojure-socket-repl`
which will start a socket server and connect to it for you.

If you've already started a socket REPL server, use `M-x inf-clojure-connect`
and enter its host and port numbers.

Inf-clojure aims to be very simple and offer tooling that the REPL
itself exposes. A few commands are:

- eval last sexp (`C-x C-e`)
- load file (`C-c C-l`)
- reload namespace (`C-c M-r`, with `M--` prefix for `:reload-all`)
- set REPL namespace (`C-c M-n`)
- show arglists for function (`C-c C-a`)
- show var documentation (`C-c C-v`)
- show source (`C-c C-s`)
- show var metadata (`C-c C-S-m`)
- insert top level form into REPL (`C-c C-j d`)

For a list of all available commands in `inf-clojure-mode` (a.k.a. the
REPL) and `inf-clojure-minor-mode` you can either invoke `C-h f RET
inf-clojure-mode` and `C-h f RET inf-clojure-minor-mode` or simply
browse their menus.

Many `inf-clojure-minor-mode` commands by default act on the symbol at
point. You can, however, change this behaviour by invoking such
commands with a prefix argument. For instance: `C-u C-c C-v` will ask
for the symbol you want to show the docstring for.

## Configuration

In the time-honoured Emacs tradition `inf-clojure`'s behaviour is extremely
configurable.

You can set custom values to `inf-clojure` variables on a
per-project basis using [directory
variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
or by setting them in in your [init file][Emacs init file].

You can see all the configuration options available using the command
`M-x customize-group RET inf-clojure`.

### Startup

While `inf-clojure` is capable of starting many common REPLs out of the box, it's
fairly likely you will want to set some custom REPL startup command
(e.g. because you need to include some `tools.deps` profile) and the REPL type
that goes with it. This is most easily achieved with the following `.dir-locals.el`:

```emacs-lisp
((nil
  (inf-clojure-custom-startup . "clojure -A:compliment")
  (inf-clojure-custom-repl-type . clojure)))
```

> [!IMPORTANT]
>
> This file has to be in the directory in which you're invoking
> `inf-clojure` or a parent directory.

There are two important configuration variables here:

1. `inf-clojure-custom-startup`: Which startup command to use so
   inf-clojure can run the inferior Clojure process (REPL).
2. `inf-clojure-custom-repl-type`: The type of the REPL started by the above command (e.g. `planck`).

If these are set and you wish to prevent inf-clojure from using them,
use a prefix arg when invoking `inf-clojure` (`C-u M-x inf-clojure`).

### Namespace-aware Evaluation

By default, eval commands (`C-x C-e`, `C-M-x`, `C-c C-r`, etc.) send code
to the REPL as-is, so it runs in whatever namespace the REPL is currently in.
If you're evaluating code from a file with a different `ns` declaration, you
may get "Unable to resolve symbol" errors.

Setting `inf-clojure-eval-ns-aware` to non-nil makes eval commands
automatically wrap code so it executes in the buffer's namespace:

```emacs-lisp
(setopt inf-clojure-eval-ns-aware t)
```

Alternatively, you can switch the REPL to the buffer's namespace
explicitly with `C-c M-n` (`inf-clojure-set-ns`).

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
           (macroexpand-1 . "(cljs.core/macroexpand-1 '%s)")
           (reload . "(require '%s :reload)")
           (reload-all . "(require '%s :reload-all)"))))
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
`inf-clojure-update-feature` which can be used like so:

```emacs-lisp
(inf-clojure-update-feature 'clojure 'completion "(incomplete.core/completions \"%s\")")
```

### `clojure-ts-mode` support

`inf-clojure` will try to use `clojure-ts-mode` by default if it's
available with fallback to `clojure-mode`.

If you want to use `inf-clojure` with `clojure-mode` exclusively, you
can set it to:

```emacs-lisp
(setopt inf-clojure-source-modes '(clojure-mode))
```

#### Caveats

As `inf-clojure` is built on top of `comint` it has all the usual comint limitations -
namely it can't handle well some fancy terminal features (e.g. ANSI colours).
In general the "dumber" your terminal REPL is, the better (e.g. `clojure` vs `clj`).
Connecting to a socket REPL is one simple way to avoid dealing with this type of
problems.

If you decide _not_ to use the socket REPL, it is highly recommended
you disable output coloring and/or `readline` facilities: `inf-clojure` does not
filter out ASCII escape characters at the moment and will not behave correctly.

For [Leiningen][], there are no command-line switches and you need to add
a custom [`project.clj`
option](https://github.com/technomancy/leiningen/blob/master/sample.project.clj):

```clojure
...
:repl-options {:color false}
...
```

#### Clojure Command Line Socket REPL

If you have the new [Clojure CLI tools][] installed you can use the `clojure` command:

> [!IMPORTANT]
>
> Do not use `clj` because it adds readline support.

``` shell
clojure -J-Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}"
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

The socket server REPL configuration options are described
[here](https://clojure.org/reference/repl_and_main#_launching_a_socket_server).

#### nREPL TTY Connection

`inf-clojure` can connect to an [nREPL](https://nrepl.org) server via its
[TTY transport](https://nrepl.org/nrepl/usage/misc.html#using-the-tty-transport),
which exposes a plain-text REPL interface that works well with comint-based
tools.

Start an nREPL server with the TTY transport enabled:

``` shell
clojure -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.3.1"}}}' \
  -M -m nrepl.cmdline --transport nrepl.transport/tty --port 7777
```

Or with Leiningen, add to your `project.clj`:

```clojure
:repl-options {:transport nrepl.transport/tty}
```

Then connect with `M-x inf-clojure-connect RET localhost RET 7777`.

> [!NOTE]
>
> The TTY transport provides a standard text REPL — you get the same features as
> a socket REPL (evaluation, completion, eldoc, etc.) but the nREPL server can
> simultaneously serve regular nREPL clients like CIDER on its default transport.

#### Multiple Process Support

To run multiple Clojure processes, you start the first up
with `inf-clojure`.  It will be in a buffer named `*inf-clojure*`.
Rename this buffer with `rename-buffer`.  You may now start up a new
process with another `inf-clojure`.  It will be in a new buffer,
named `*inf-clojure*`.  You can switch between the different process
buffers with `switch-to-buffer`.

> [!NOTE]
>
> If you're starting `inf-clojure` within a Clojure project directory the name
> of the project will be incorporated into the name of the REPL buffer -
> e.g. `*inf-clojure my-project*`.

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

You can use the helpful function `inf-clojure-set-repl`. If called in
an `inf-clojure` REPL buffer, it will assign that buffer as the current
REPL (`(setq inf-clojure-buffer (current-buffer)`). If you are
not in an `inf-clojure` REPL buffer, it will offer a choice of
acceptable buffers to set as the REPL buffer. If called with a prefix,
it will always give the list even if you are currently in an
acceptable REPL buffer.

> [!TIP]
>
> Renaming buffers will greatly improve the
> functionality of this list; the list "project-1: clojure repl",
> "project-2: cljs repl" is far more understandable than "inf-clojure",
> "inf-clojure<2>".

#### REPL Type

An `inf-clojure` REPL has an associated type. The available types can be
obtained from `inf-clojure-repl-features`:

```emacs-lisp
(mapcar 'car inf-clojure-repl-features)

;; => (cljs planck joker babashka node-babashka clojure clojure-clr)
```

What does it mean that a REPL type is supported? Well, it means that
`inf-clojure` would use the proper Clojure(Script) code internally to power
commands like definition lookup and friends.  Those differ from REPL to REPL and
can't be implemented in a REPL-independent way. The REPL type is inferred on
startup when using the `inf-clojure` command or is specified manually when using
`inf-clojure-connect`.

#### ElDoc

`eldoc-mode` is supported in Clojure source buffers and `*inf-clojure*`
buffers which are running a Clojure REPL.

When ElDoc is enabled and there is an active REPL, it will show the argument
list of the function call you are currently editing in the echo area. It
accomplishes this by evaluating forms to get the metadata for the vars under
your cursor. One side effect of this is that it can mess with repl vars like
`*1` and `*2`. You can disable inf-clojure's Eldoc functionality with `(setq
inf-clojure-enable-eldoc nil)`.

ElDoc currently doesn't work with ClojureScript buffers and REPL's.
You can leave it enabled, it just won't show anything in the echo area.

#### Code Completion

Code completion requires a completion library on the classpath. `inf-clojure`
does not ship with a default completion form — you need to configure one.

The two main options are [incomplete](https://github.com/nrepl/incomplete) (simple, lightweight) and
[compliment](https://github.com/alexander-yakushev/compliment) (richer, context-aware). Some runtimes
(e.g. Planck) provide built-in completion functions.

**Setup with `deps.edn` (including socket REPLs):**

Add a completion library to your dependencies:

```clojure
{:aliases {:completions {:extra-deps {io.github.nrepl/incomplete {:mvn/version "0.1.0"}}}}}
```

Start your REPL with the alias:

``` shell
clojure -A:completions -J-Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}"
```

Then configure `inf-clojure` to use it:

```emacs-lisp
;; for incomplete
(inf-clojure-update-feature 'clojure 'completion "(incomplete.core/completions \"%s\")")

;; or for compliment
(inf-clojure-update-feature 'clojure 'completion "(compliment.core/completions \"%s\")")
```

> [!IMPORTANT]
>
> You must ensure the completion namespace is required in the REPL before
> completions will work. For `incomplete`, run `(require 'incomplete.core)`.
> For `compliment`, run `(require 'compliment.core)`.

**Setup with Leiningen:**

Recent versions of Leiningen include `incomplete` automatically via
[REPLy](https://github.com/trptcolin/reply), so completions may work
out of the box. For socket REPLs started via Leiningen, you still need
to add the dependency explicitly and require the namespace.

You can also customize `inf-clojure-completions-fn` to specify a
function that parses the REPL's completion response and returns Elisp
data compatible with
[`completion-at-point-functions`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html).

For more info run `M-x describe-variable RET
inf-clojure-completions-fn`.

## Troubleshooting

### Things seem broken

Inf-clojure is intentionally quite simple and just sends commands to a
REPL on your behalf to provide features. In order to do this
inf-clojure needs to know the REPL type so it can format the correct
calls (e.g. `(clojure.repl/doc ...)` vs `(cljs.repl/doc ...)`), so
it's important that the REPL type is set correctly. This REPL type
exists in the process buffer (REPL) and the source buffers as a cache.
If you have problems, run `M-x inf-clojure-set-repl-type` from the
source buffer to set the REPL type in both buffers. To see how simple
inf-clojure is, look at `inf-clojure-repl-features` to see how things
are laid out.

### REPL not responsive in Windows OS

In Windows, the REPL is not returning anything. For example, type `(+
1 1)` and press `ENTER`, the cursor just drops to a new line and
nothing is shown.

The explanation of this problem and solution can be found [here](https://groups.google.com/forum/#!topic/leiningen/48M-xvcI2Ng).

The solution is to create a file named `.jline.rc` in your `$HOME`
directory and add this line to that file:

```ini
jline.terminal=unsupported
```

### Log process activity

Standard Emacs debugging turns out to be difficult when an asynchronous process is involved. In this case try to enable logging:

```emacs-lisp
(setq inf-clojure-log-activity t)
```

This creates `.inf-clojure.log` in the project directory so that you can `tail -f` on it.

## License

Copyright © 2014-2026 Bozhidar Batsov and [contributors][].

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
