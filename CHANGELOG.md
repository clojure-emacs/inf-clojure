# Changelog

## master (unreleased)

### New Features

* [#34](https://github.com/clojure-emacs/inf-clojure/pull/34): Add support for socket REPL connections.
* [#46](https://github.com/clojure-emacs/inf-clojure/pull/46): Make it possible to disable prompt on `inf-clojure-set-ns`.
* New interactive command `inf-clojure-display-version`.
* [#42](https://github.com/clojure-emacs/inf-clojure/issues/42): Add a defcustom controlling the window in which the REPL buffer is displayed (`inf-clojure-repl-use-same-window`).
* Font-lock the code in the REPL.
* Handle properly ANSI color escape sequences in the REPL.

### Changes

* Display the REPL in a different window by default (it used to be displayed in the current window).

### Bugs Fixed

* [#35](https://github.com/clojure-emacs/inf-clojure/issues/35): Fix prompt being included in input history.

## 1.4.0 (2016-01-17)

### New Features

* [#22](https://github.com/clojure-emacs/inf-clojure/pull/22): Add ElDoc support.
