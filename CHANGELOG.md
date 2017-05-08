# Changelog

## master (unreleased)

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
