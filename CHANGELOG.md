# Changelog

## master (unreleased)

### New features

- [#57](https://github.com/clojure-emacs/inf-clojure/issues/57): Add `inf-clojure-show-var-meta` command (`C-c C-S-m`) to display a var's metadata.
- [#205](https://github.com/clojure-emacs/inf-clojure/issues/205): Add `inf-clojure-eval-ns-aware` option for namespace-aware evaluation.

### Changes

- Unify `reload` and `reload-all` forms with `inf-clojure-repl-features`, enabling per-REPL-type dispatch. The standalone `inf-clojure-reload-form` and `inf-clojure-reload-all-form` defcustoms have been removed.
- Deduplicate Clojure-family REPL feature definitions via a shared base alist.
- [#143](https://github.com/clojure-emacs/inf-clojure/issues/143): `inf-clojure-show-arglists` now displays results in the REPL buffer instead of the minibuffer.

### Bugs fixed

- Fix `inf-clojure-connected-p` to check for a live process.
- Fix truncated autoload cookie on `inf-clojure-connect`.
- Fix `prefix-arg` vs `current-prefix-arg` in REPL startup.
- [#219](https://github.com/clojure-emacs/inf-clojure/issues/219): Fix `project-root` error when no project is detected (e.g. Squint REPL outside a project).

## 3.3.0 (2025-05-25)

### New features

- [#202](https://github.com/clojure-emacs/inf-clojure/issues/202): Add ClojureCLR support.
- [#210](https://github.com/clojure-emacs/inf-clojure/pull/210) Include `inf-clojure-socket-repl` to create a socket REPL and connect to it from inside Emacs.
- [#217](https://github.com/clojure-emacs/inf-clojure/pull/217): Add `clojure-ts-mode` support.

### Changes

- Improve support for multiple forms in the same line by replacing `beginning-of-defun` fn.
- [#204](https://github.com/clojure-emacs/inf-clojure/issues/204): Scroll repl buffer on insert commands
- [#208](https://github.com/clojure-emacs/inf-clojure/pull/208) Display message after setting REPL.
- Require Emacs 28.
- Drop support for Lumo.

## 3.2.1 (2022-07-22)

### Bugs fixed

- Address some small issues with NonGNU ELPA (e.g. missing maintainer metadata).

## 3.2.0 (2022-07-15)

### New features

- [#168](https://github.com/clojure-emacs/inf-clojure/pull/197): New helper function `inf-clojure-switch-to-recent-buffer` to select the last buffer an inf-clojure process buffer was swapped to from.
- [#187](https://github.com/clojure-emacs/inf-clojure/pull/197): New defcustom `inf-clojure-enable-eldoc` to disable eldoc interaction.

### Bugs fixed

- [#185](https://github.com/clojure-emacs/inf-clojure/issues/185): Improve cmd string splitting.
- [#193](https://github.com/clojure-emacs/inf-clojure/pull/193): Set syntax table in REPL buffer.
- Fix `inf-clojure-display-version` (it wasn't extracting properly the package version).

## 3.1.0 (2021-07-23)

### New features

- [#190](https://github.com/clojure-emacs/inf-clojure/pull/190): Helper function `inf-clojure-set-repl` to select inf-clojure process buffer.
- Auto-enable `inf-clojure-minor-mode` after invoking `inf-clojure`. This behaviour is controlled via `inf-clojure-auto-mode`.
- Include the project name automatically in the REPL buffer name.

### Bugs fixed

- [#152](https://github.com/clojure-emacs/inf-clojure/issues/152): Sanitize should only remove whitespace at the end of a command.
- [#188](https://github.com/clojure-emacs/inf-clojure/pull/188): Handle newlines between forms for `inf-clojure-eval-buffer`.
- [#189](https://github.com/clojure-emacs/inf-clojure/pull/189): Font-lock code inserted in the REPL from a source buffer.

## 3.0.0 (2020-08-01)

### New features

- [#174](https://github.com/clojure-emacs/inf-clojure/pull/174): Invoke `inf-clojure` with a prefix argument to prevent using `inf-clojure-custom-startup` and `inf-clojure-custom-repl-type`.
- Made it possible to add user-defined REPL types (by modifying `inf-clojure-repl-features`).

### Changes

- **(Breaking)*- Restructure massively the configuration. See `inf-clojure-repl-features` for details.
- [#174](https://github.com/clojure-emacs/inf-clojure/pull/174): Set REPL type from startup form or prompt at startup, introduce `inf-clojure-custom-repl-type` defcustom.
- [#173](https://github.com/clojure-emacs/inf-clojure/issues/173): Use clojure-mode's project detection instead of duplicate version in inf-clojure.

### Bugs fixed

- [#178](https://github.com/clojure-emacs/inf-clojure/issues/178): Ensure a valid directory is used when starting process.

## 2.2.0 (2020-04-15)

### New features

- [#170](https://github.com/clojure-emacs/inf-clojure/pull/170): Add insert defun and last sexp commands.
- [#160](https://github.com/clojure-emacs/inf-clojure/pull/160): Support [Joker](https://joker-lang.org/).

### Bugs fixed

- [#164](https://github.com/clojure-emacs/inf-clojure/pull/164): Fix for eldoc-mode on ClojureCLR.
- [#135](https://github.com/clojure-emacs/inf-clojure/pull/135): Improve command sanitation code.
- Fix `info-clojure-apropos`.

## 2.1.0 (2018-01-02)

### New Features

- [#114](https://github.com/clojure-emacs/inf-clojure/pull/114): Introduce `inf-clojure-project-type` defcustom.
- [#117](https://github.com/clojure-emacs/inf-clojure/pull/117): Introduce `tools.deps` project type and `inf-clojure-tools-deps-cmd`.
- [#122](https://github.com/clojure-emacs/inf-clojure/pull/122): Introduce `inf-clojure-completions-fn` defcustom.
- [#128](https://github.com/clojure-emacs/inf-clojure/pull/128): Expose `inf-clojure-apropos` as `C-c C-S-a` in `inf-clojure-mode` (the REPL).
- [#125](https://github.com/clojure-emacs/inf-clojure/pull/125): Avoid throwing an error for frequent operations like completion.
- [#130](https://github.com/clojure-emacs/inf-clojure/pull/130): Support loading directory locals in our buffers.
- [#129](https://github.com/clojure-emacs/inf-clojure/pull/129): Improve the completion bounds detection (now with keywords).
- [#132](https://github.com/clojure-emacs/inf-clojure/pull/132): Introduce inf-clojure-reload.

### Bugs Fixed

- [#79](https://github.com/clojure-emacs/inf-clojure/pull/82): Eldoc error when running boot repl.
- [#83](https://github.com/clojure-emacs/inf-clojure/pull/85): No such namespace: complete.core in lumo REPL.
- [#93](https://github.com/clojure-emacs/inf-clojure/pull/93): Slow response from inf-clojure (completions, arglists, ...).
- [#101](https://github.com/clojure-emacs/inf-clojure/pull/101): `inf-clojure-set-ns` hangs Emacs.
- [#119](https://github.com/clojure-emacs/inf-clojure/pull/119): Set inf-clojure-buffer REPL type on detect.
- [#120](https://github.com/clojure-emacs/inf-clojure/pull/120): Send REPL string always, even if empty.
- [#128](https://github.com/clojure-emacs/inf-clojure/pull/128): Fix inf-clojure-apropos.
- [#131](https://github.com/clojure-emacs/inf-clojure/pull/131): Add macroexpand forms for Lumo.

## 2.0.1 (2017-05-18)

### Bugs Fixed

- [#77](https://github.com/clojure-emacs/inf-clojure/pull/77): Fix request "Eval expression:" if arglists return is `nil`.

## 2.0.0 (2017-05-01)

### New Features

- [#63](https://github.com/clojure-emacs/inf-clojure/pull/69): Fix spurious process output on init.
- [#57](https://github.com/clojure-emacs/inf-clojure/pull/68): Add `inf-clojure-connect`.
- [#66](https://github.com/clojure-emacs/inf-clojure/pull/56): Add Planck support.
- [#51](https://github.com/clojure-emacs/inf-clojure/pull/51): Commands do not prompt by default anymore, unless they receive a non-nil prefix argument.
- [#44](https://github.com/clojure-emacs/inf-clojure/pull/44): Add REPL types and Lumo support.
- [#50](https://github.com/clojure-emacs/inf-clojure/pull/50): Rename defcustoms to `inf-clojure-*-form` where appropriate.
- [#34](https://github.com/clojure-emacs/inf-clojure/pull/34): Add support for socket REPL connections.
- New interactive command `inf-clojure-display-version`.
- [#42](https://github.com/clojure-emacs/inf-clojure/issues/42): Add a defcustom controlling the window in which the REPL buffer is displayed (`inf-clojure-repl-use-same-window`).
- Font-lock the code in the REPL.
- Handle properly ANSI color escape sequences in the REPL.
- [#41](https://github.com/clojure-emacs/inf-clojure/issues/41): Add a command to quit the REPL (it's bound to `C-c C-q`).
- [#29](https://github.com/clojure-emacs/inf-clojure/issues/29): Add a command to restart the REPL.
- [#31](https://github.com/clojure-emacs/inf-clojure/issues/31): Invoke different init command based on the project type (boot, lein or generic).

### Changes

- Display the REPL in a different window by default (it used to be displayed in the current window).
- [#26](https://github.com/clojure-emacs/inf-clojure/issues/26): Make switching to the REPL optional on `inf-clojure-load-file` (it's now controlled via a prefix argument).
- Removed the `inf-clojure` alias `run-clojure`.

### Bugs Fixed

- [#35](https://github.com/clojure-emacs/inf-clojure/issues/35): Fix prompt being included in input history.

## 1.4.0 (2016-01-17)

### New Features

- [#22](https://github.com/clojure-emacs/inf-clojure/pull/22): Add ElDoc support.
