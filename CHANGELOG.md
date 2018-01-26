# Changelog

## master (unreleased)

## 2.1.0 (2018-01-02)

### New Features

* [#114](https://github.com/clojure-emacs/inf-clojure/pull/114): Introduce `inf-clojure-project-type` defcustom.
* [#117](https://github.com/clojure-emacs/inf-clojure/pull/117): Introduce `tools.deps` project type and `inf-clojure-tools-deps-cmd`.
* [#122](https://github.com/clojure-emacs/inf-clojure/pull/122): Introduce `inf-clojure-completions-fn` defcustom.
* [#128](https://github.com/clojure-emacs/inf-clojure/pull/128): Expose `inf-clojure-apropos` as `C-c C-S-a` in `inf-clojure-mode` (the REPL).
* [#125](https://github.com/clojure-emacs/inf-clojure/pull/125): Avoid throwing an error for frequent operations like completion.

### Bugs Fixed

* [#79](https://github.com/clojure-emacs/inf-clojure/pull/82): Eldoc error when running boot repl.
* [#83](https://github.com/clojure-emacs/inf-clojure/pull/85): No such namespace: complete.core in lumo REPL.
* [#93](https://github.com/clojure-emacs/inf-clojure/pull/93): Slow response from inf-clojure (completions, arglists, ...).
* [#101](https://github.com/clojure-emacs/inf-clojure/pull/101): `inf-clojure-set-ns` hangs Emacs.
* [#119](https://github.com/clojure-emacs/inf-clojure/pull/119): Set inf-clojure-buffer REPL type on detect.
* [#120](https://github.com/clojure-emacs/inf-clojure/pull/120): Send REPL string always, even if empty.
* [#128](https://github.com/clojure-emacs/inf-clojure/pull/128): Fix inf-clojure-apropos.
* [#131](https://github.com/clojure-emacs/inf-clojure/pull/131): Add macroexpand forms for Lumo.

## 2.0.1 (2017-05-18)

### Bugs Fixed

* [#77](https://github.com/clojure-emacs/inf-clojure/pull/77): Fix request "Eval expression:" if arglists return is `nil`.

## 2.0.0 (2017-05-01)

### New Features

* [#63](https://github.com/clojure-emacs/inf-clojure/pull/69): Fix spurious process output on init.
* [#57](https://github.com/clojure-emacs/inf-clojure/pull/68): Add `inf-clojure-connect`.
* [#66](https://github.com/clojure-emacs/inf-clojure/pull/56): Add Planck support.
* [#51](https://github.com/clojure-emacs/inf-clojure/pull/51): Commands do not prompt by default anymore, unless they receive a non-nil prefix argument.
* [#44](https://github.com/clojure-emacs/inf-clojure/pull/44): Add REPL types and Lumo support.
* [#50](https://github.com/clojure-emacs/inf-clojure/pull/50): Rename defcustoms to `inf-clojure-*-form` where appropriate.
* [#34](https://github.com/clojure-emacs/inf-clojure/pull/34): Add support for socket REPL connections.
* New interactive command `inf-clojure-display-version`.
* [#42](https://github.com/clojure-emacs/inf-clojure/issues/42): Add a defcustom controlling the window in which the REPL buffer is displayed (`inf-clojure-repl-use-same-window`).
* Font-lock the code in the REPL.
* Handle properly ANSI color escape sequences in the REPL.
* [#41](https://github.com/clojure-emacs/inf-clojure/issues/41): Add a command to quit the REPL (it's bound to `C-c C-q`).
* [#29](https://github.com/clojure-emacs/inf-clojure/issues/29): Add a command to restart the REPL.
* [#31](https://github.com/clojure-emacs/inf-clojure/issues/31): Invoke different init command based on the project type (boot, lein or generic).

### Changes

* Display the REPL in a different window by default (it used to be displayed in the current window).
* [#26](https://github.com/clojure-emacs/inf-clojure/issues/26): Make switching to the REPL optional on `inf-clojure-load-file` (it's now controlled via a prefix argument).
* Removed the `inf-clojure` alias `run-clojure`.

### Bugs Fixed

* [#35](https://github.com/clojure-emacs/inf-clojure/issues/35): Fix prompt being included in input history.

## 1.4.0 (2016-01-17)

### New Features

* [#22](https://github.com/clojure-emacs/inf-clojure/pull/22): Add ElDoc support.
