[![License GPL 3][badge-license]][copying]
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![Circle CI][circleci-badge]][circleci]

# inf-clojure

This package provides basic interaction with a Clojure subprocess (REPL).
It's based on ideas from the popular `inferior-lisp` package.

`inf-clojure` has two components - a nice Clojure REPL with
auto-completion and a minor mode (`inf-clojure-minor-mode`), which
extends `clojure-mode` with commands to evaluate forms directly in the
REPL.

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
* Support for [Lumo](https://github.com/anmonteiro/lumo)
* Support for [Planck](http://planck-repl.org/)
* Support for [Joker](https://joker-lang.org/)

For a more powerful/full-featured solution see [CIDER](https://github.com/clojure-emacs/cider).

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

## Usage

Just invoke `M-x inf-clojure` or press `C-c C-z` within a Clojure
source file. You should get a prompt with the supported repl types and
common startup forms. You can select one of these or type in your own
custom startup. This will start a REPL process for the current project
and you can start interacting with it.

If you've already started a socket repl, use `M-x inf-clojure-connect`
and enter the host and port numbers.

It is highly recommended to use a cons pair like `("localhost" . 5555)` to
connect to a socket REPL, terminal REPLs are inherently hard to work with and
support will be deprecated in the foreseeable future.

Inf-clojure aims to be very simple and offer tooling that the repl
itself exposes. A few commands are:

- eval last sexp `C-x C-e`
- show arglists for function `C-c C-a`
- show var documentation `C-c C-v`
- show source `C-c C-s`
- insert top level form into repl `C-c C-j d`

Many `inf-clojure-minor-mode` commands by default act on the symbol at
point. You can, however, change this behaviour by invoking such
commands with a prefix argument. For instance: `C-u C-c C-v` will ask
for the symbol you want to show the docstring for.

#### Clojure Command Line Socket REPL

If you have the new [Clojure CLI tools][] installed you can use the `clojure` command:

_do not use `clj` because it adds readline support_

```shell
clojure -J-Dclojure.server.repl="{:port 5555 :accept clojure.core.server/repl}"
```

Then either `C-c M-c RET localhost RET 5555` from within Emacs or add the following to your `.dir-locals.el`:

```emacs-lisp
((nil . ((inf-clojure-tools-deps-cmd . ("localhost" . 5555)))))
```

or the following to your [Emacs init file][]:

```emacs-lisp
(setf inf-clojure-tools-deps-cmd '("localhost" . 5555)):
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
((nil . ((inf-clojure-lein-cmd . ("localhost" . 5555)))))
```

or the following to your [Emacs init file][]:

```emacs-lisp
(setf inf-clojure-lein-cmd '("localhost" . 5555))
```

Project detection can be completely skipped and the `generic` project type can be used instead:

```emacs-lisp
(setf inf-clojure-project-type . "generic")
(setf inf-clojure-generic-cmd '("localhost" 5555))
```

#### Caveats

Note that if you decide _NOT_ to use the socket repl, it is highly recommended
you disable output coloring and/or readline facilities: `inf-clojure` does not
filter out ASCII escape characters at the moment and will not behave correctly.

You can disable coloring the following way for `boot`:

```emacs-lisp
((nil . ((inf-clojure-boot-cmd . "boot repl -C"))))
```

For leiningen, there are no command line switches and you need to add a custom [`project.clj` option](https://github.com/technomancy/leiningen/blob/master/sample.project.clj):

```clojure
...
  :repl-options {:color false}
...
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

## Configuration options

In the time-honoured Emacs tradition `inf-clojure`'s behaviour is extremely
configurable.

You can see all the configuration options available using the command
`M-x customize-group RET inf-clojure`.

#### REPL Type

An `inf-clojure` REPL can be of different types: Clojure, ClojureScript, Lumo
and Planck are all potentially valid options.

At the moment, the default Clojure REPL, the Lumo REPL, the Planck REPL and the
Joker REPL are supported (standard ClojureScript is lacking mostly because some
features require to access the compiler state,
[cljs-tooling](https://github.com/clojure-emacs/cljs-tooling) is a good
candidate for enabling support).

What does it mean that a REPL type is supported - well it means that `inf-clojure`
would use the proper code internally to power commands like definition lookup and friends.
Those differ from REPL to REPL and can't be implemented in a REPL-independent way. At
boot type `inf-clojure` tries to detect the type of the REPL that was started and uses
this type to dispatch the proper code for the respective REPL type.

By default `inf-clojure` would start a standard Clojure REPL using
`lein` or `boot` but you can easily change this.  To boot some other REPL just use the
right launch command (or connect to the REPL via a socket).  For example, for
Lumo just add the following in your `.dir-locals.el`:

```emacs-lisp
((nil . ((inf-clojure-boot-cmd . "lumo -d")))) ;; inf-clojure-lein-cmd if you are using Leiningen
```

#### ElDoc

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

Code completion is particularly open to customization. Not only you can `setq`
the customary `inf-clojure-completion-form`,
`inf-clojure-completion-form-lumo`, `inf-clojure-completion-form-planck` and
`inf-clojure-completion-form-joker` - the form to send to the REPL - but you
can also use `inf-clojure-completions-fn` for specifying a function that given
the REPL response should return elisp data compatible with
[`completion-at-point-functions`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html).
For more info run `M-x describe-variable RET inf-clojure-completions-fn`.
Another option is to have a look at
[how cider does it](https://github.com/clojure-emacs/cider/blob/3e9ed12e8cfbad04d7618e649322765dc9bff5d6/cider-interaction.el#L595).

#### Lumo Setup

For an optimal Lumo experience the `-d` needs to be passed to Lumo
when launched from the command line. This disable `readline` support
in order to play nicely with emacs.

For example, you can use the following command (assuming `cp` contains
the classpath) in your `.dir-locals.el`:

```emacs-lisp
((nil . (eval . (setq inf-clojure-generic-cmd (concat "lumo -d -c "
                                                      (f-read (concat (inf-clojure-project-root) "cp")))))))
```

## Troubleshooting

### Things seem broken

Inf-clojure is intentionally quite simple and just sends commands to a
repl on your behalf to provide features. In order to do this
inf-clojure largely needs to know the repl type so it can format the
correct calls. Most end up in `(lumo.repl/doc [symbol])` or
`(cljs.repl/doc ...)` so its important that the repl type is set
correctly. This repl type exists in the process buffer (repl) and the
source buffers as a cache. If you have problems, run `m-x
inf-clojure-set-repl-type` from the source buffer to set the repl type
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
