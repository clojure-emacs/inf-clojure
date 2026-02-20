;;; inf-clojure-tests.el --- Tests for Inf-Clojure -*- lexical-binding: t; -*-
;;
;; Copyright © 2014-2026 Bozhidar Batsov

;; Authors: Bozhidar Batsov <bozhidar@batsov.dev>
;;          Andrea Richiardi <a.richiardi.work@gmail.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tests for inf-clojure.

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

(require 'buttercup)
(require 'assess) ;; requires Emacs 26, due to a dependency on seq.el
(require 'inf-clojure)

(cl-defmacro ict-with-assess-buffers ((&rest varlist) &body body)
  `(assess-with-temp-buffers (,@varlist)
                             (clojure-mode)
                             (inf-clojure-minor-mode)
                             ,@body))

(defun ict-bounds-string (bounds)
  (buffer-substring (car bounds) (cdr bounds)))

(describe "inf-clojure--kw-to-symbol"
  (it "returns symbol form of the given keyword"
    (expect (inf-clojure--kw-to-symbol "symbol") :to-equal "symbol")
    (expect (inf-clojure--kw-to-symbol ":clj.core/str") :to-equal "clj.core/str")
    (expect (inf-clojure--kw-to-symbol "::keyword") :to-equal "keyword")
    (expect (inf-clojure--kw-to-symbol nil) :to-equal nil)))

(describe "completion bounds at point"
  (it "computes bounds for plain-text"
    (ict-with-assess-buffers
     ((a (insert "plain-text")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal "plain-text"))))

  (it "computes bounds for @deref"
    (ict-with-assess-buffers
     ((a (insert "@deref")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal "deref"))))

  (it "computes bounds for ^:keyword"
    (ict-with-assess-buffers
     ((a (insert "^:keyword")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal ":keyword"))))

  (it "computes bounds for ::keyword"
    (ict-with-assess-buffers
     ((a (insert "::keyword")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal "::keyword"))))

  (it "computes bounds for [^:keyword (combined break chars and keyword)"
    (ict-with-assess-buffers
     ((a (insert "[^:keyword")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal ":keyword"))))

  (it "computes no bounds for point directly after a break expression"
    (ict-with-assess-buffers
     ((a (insert "@")))
     (with-current-buffer a
       (expect (inf-clojure-completion-bounds-of-expr-at-point)
               :to-be nil))))

  (it "computes bounds for [symbol"
    (ict-with-assess-buffers
     ((a (insert "[symbol")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal "symbol"))))

  (it "computes bounds for (@deref (multiple break chars)"
    (ict-with-assess-buffers
     ((a (insert "(@deref")))
     (with-current-buffer a
       (expect (ict-bounds-string (inf-clojure-completion-bounds-of-expr-at-point))
               :to-equal "deref")))))

(describe "inf-clojure--sanitize-command"
  (it "sanitizes the command correctly"
    (expect (inf-clojure--sanitize-command "(doc println)") :to-equal "(doc println)\n"))

  (it "trims newline at the right of a command"
    (expect (inf-clojure--sanitize-command "(doc println)\n\n\n\n") :to-equal "(doc println)\n"))

  (it "returns empty string when the command is empty"
    (expect (inf-clojure--sanitize-command "   ") :to-equal ""))

  (it "only removes whitespace at the end of the command - fix 152"
    (expect (inf-clojure--sanitize-command "1   5") :to-equal "1   5\n")))

(describe "inf-clojure--forms-without-newlines"
  (it "removes newlines between toplevel forms"
    (expect (inf-clojure--forms-without-newlines
             "(def foo 3)\n\n\n(def bar 4)")
            :to-equal "(def foo 3)\n(def bar 4)"))
  (it "doesn't remove newlines inside forms or strings"
    (expect (inf-clojure--forms-without-newlines
             "

(defn foo []

  :foo)


(def thing \"this

is a string\")

(defn bar [])")
            ;; note no leading newline, newlines inside defn remain,
            ;; newlines inside string remain
            :to-equal "(defn foo []

  :foo)
(def thing \"this

is a string\")
(defn bar [])")))


(describe "inf-clojure--update-feature"
  (it "updates new forms correctly"
    (let ((inf-clojure-repl-features (inf-clojure--update-feature 'cljs 'doc "new doc")))
      (expect (inf-clojure--get-feature 'cljs 'doc nil)
              :to-equal "new doc")))
  (describe "if the repl type is unknown"
    (it "signals an error"
      (expect (inf-clojure--update-feature 'not-found 'doc "new doc")
              :to-throw))))

(describe "inf-clojure--merge-repl-features"
  (it "merges overrides onto a base alist"
    (let ((base '((a . "base-a") (b . "base-b") (c . "base-c")))
          (overrides '((b . "override-b"))))
      (expect (inf-clojure--merge-repl-features base overrides)
              :to-equal '((b . "override-b") (a . "base-a") (c . "base-c")))))
  (it "preserves the base when overrides are empty"
    (let ((base '((a . "base-a") (b . "base-b"))))
      (expect (inf-clojure--merge-repl-features base nil)
              :to-equal base))))

(describe "inf-clojure-repl-features"
  (it "provides all base features for clojure-family REPL types"
    (let ((base-features '(load doc source apropos ns-vars set-ns
                                macroexpand macroexpand-1 arglists
                                reload reload-all var-meta)))
      (dolist (repl-type '(clojure clojure-clr babashka node-babashka lein-clr))
        (dolist (feature base-features)
          (expect (inf-clojure--get-feature repl-type feature nil)
                  :not :to-be nil)))))
  (it "gives node-babashka the same features as babashka"
    (let ((bb-features (alist-get 'babashka inf-clojure-repl-features))
          (nbb-features (alist-get 'node-babashka inf-clojure-repl-features)))
      (expect bb-features :to-equal nbb-features)))
  (it "shares arglists across JVM REPL types"
    (let ((clj-arglists (inf-clojure--get-feature 'clojure 'arglists nil))
          (bb-arglists (inf-clojure--get-feature 'babashka 'arglists nil)))
      (expect clj-arglists :to-equal bb-arglists)))
  (it "provides reload, reload-all and var-meta for all REPL types"
    (dolist (repl-type '(clojure clojure-clr babashka node-babashka lein-clr cljs planck joker))
      (expect (inf-clojure--get-feature repl-type 'reload nil)
              :not :to-be nil)
      (expect (inf-clojure--get-feature repl-type 'reload-all nil)
              :not :to-be nil)
      (expect (inf-clojure--get-feature repl-type 'var-meta nil)
              :not :to-be nil)))
  (it "uses a different arglists catch clause for CLR REPL types"
    (let ((clj-arglists (inf-clojure--get-feature 'clojure 'arglists nil))
          (clr-arglists (inf-clojure--get-feature 'clojure-clr 'arglists nil))
          (lein-clr-arglists (inf-clojure--get-feature 'lein-clr 'arglists nil)))
      (expect clj-arglists :not :to-equal clr-arglists)
      (expect clr-arglists :to-equal lein-clr-arglists)
      (expect clr-arglists :to-match "Exception"))))

(describe "inf-clojure--get-feature"
  (it "returns the feature form for a known repl type and feature"
    (expect (inf-clojure--get-feature 'clojure 'doc nil)
            :to-equal "(clojure.repl/doc %s)"))
  (it "returns nil for a missing feature when no-error is truthy"
    (expect (inf-clojure--get-feature 'joker 'source t)
            :to-be nil))
  (it "signals an error for a missing feature when no-error is nil"
    (expect (inf-clojure--get-feature 'joker 'source nil)
            :to-throw)))

(describe "inf-clojure--endpoint-p"
  (it "returns non-nil for a valid host/port cons"
    (expect (inf-clojure--endpoint-p '("localhost" . 5555)) :to-be-truthy))
  (it "returns nil for non-endpoints"
    (expect (inf-clojure--endpoint-p nil) :to-be nil)
    (expect (inf-clojure--endpoint-p "localhost:5555") :to-be nil)
    (expect (inf-clojure--endpoint-p '(5555 . "localhost")) :to-be nil)
    (expect (inf-clojure--endpoint-p '("localhost" . "5555")) :to-be nil)))

(describe "inf-clojure--project-name"
  (it "extracts the final path segment"
    (expect (inf-clojure--project-name "/home/user/projects/my-app")
            :to-equal "my-app"))
  (it "handles trailing slashes"
    (expect (inf-clojure--project-name "/home/user/projects/my-app/")
            :to-equal "my-app")))

(describe "inf-clojure-chomp"
  (it "removes a trailing newline"
    (expect (inf-clojure-chomp "hello\n") :to-equal "hello"))
  (it "removes only the final trailing newline"
    (expect (inf-clojure-chomp "hello\n\n\n") :to-equal "hello\n\n"))
  (it "returns unchanged string when no trailing newline"
    (expect (inf-clojure-chomp "hello") :to-equal "hello"))
  (it "preserves internal newlines"
    (expect (inf-clojure-chomp "hello\nworld\n") :to-equal "hello\nworld")))

(describe "inf-clojure-preoutput-filter"
  (it "passes through output when not filtering"
    (let ((inf-clojure--filtering-output nil)
          (last-command 'self-insert-command))
      (expect (inf-clojure-preoutput-filter "hello") :to-equal "hello")))
  (it "prepends a newline only to the first chunk of inf-clojure command output"
    (let ((inf-clojure--filtering-output nil)
          (inf-clojure--output-pending-newline nil)
          (this-command 'inf-clojure-eval-last-sexp)
          (inf-clojure-prompt "^[^=> \n]+=> *"))
      ;; First chunk: should get newline prefix
      (expect (inf-clojure-preoutput-filter "result")
              :to-equal "\nresult")
      ;; Second chunk: no newline prefix
      (expect (inf-clojure-preoutput-filter " more")
              :to-equal " more")))
  (it "stops filtering when a prompt is detected"
    (let ((inf-clojure--filtering-output t)
          (inf-clojure--output-pending-newline nil)
          (inf-clojure-prompt "^[^=> \n]+=> *"))
      (inf-clojure-preoutput-filter "\nuser=> ")
      (expect inf-clojure--filtering-output :to-be nil)))
  (it "removes subprompts from filtered output"
    (let ((inf-clojure--filtering-output t)
          (inf-clojure--output-pending-newline nil)
          (inf-clojure-subprompt " *#_=> *"))
      (expect (inf-clojure-preoutput-filter "foo #_=> bar")
              :to-equal "foobar"))))

(describe "inf-clojure-remove-subprompts"
  (it "removes subprompts from a string"
    (expect (inf-clojure-remove-subprompts "foo #_=> bar")
            :to-equal "foobar"))
  (it "returns unchanged string when no subprompts"
    (expect (inf-clojure-remove-subprompts "hello world")
            :to-equal "hello world")))

(describe "inf-clojure--nil-string-match-p"
  (it "matches the string nil"
    (expect (inf-clojure--nil-string-match-p "nil") :to-be-truthy))
  (it "matches nil with surrounding whitespace"
    (expect (inf-clojure--nil-string-match-p "  nil  ") :to-be-truthy)
    (expect (inf-clojure--nil-string-match-p "nil\n") :to-be-truthy))
  (it "does not match nil as part of a larger word"
    (expect (inf-clojure--nil-string-match-p "not-nil") :not :to-be-truthy)
    (expect (inf-clojure--nil-string-match-p "nilable") :not :to-be-truthy)))

(describe "inf-clojure--some"
  (it "returns nil for nil"
    (expect (inf-clojure--some nil) :to-be nil))
  (it "returns nil for the string nil"
    (expect (inf-clojure--some "nil") :to-be nil)
    (expect (inf-clojure--some " nil ") :to-be nil))
  (it "returns data for non-nil values"
    (expect (inf-clojure--some "([x] [x y])") :to-equal "([x] [x y])")
    (expect (inf-clojure--some 42) :to-equal 42)
    (expect (inf-clojure--some "") :to-equal "")))

(describe "inf-clojure--list-or-nil"
  (it "returns a list unchanged"
    (expect (inf-clojure--list-or-nil '("a" "b")) :to-equal '("a" "b")))
  (it "returns nil for the empty list"
    ;; Note: nil IS a list in elisp, so (listp nil) => t
    (expect (inf-clojure--list-or-nil nil) :to-be nil))
  (it "returns nil for non-list data"
    (expect (inf-clojure--list-or-nil "string") :to-be nil)
    (expect (inf-clojure--list-or-nil 42) :to-be nil)))

(describe "inf-clojure--read-or-nil"
  (it "reads a lisp form from a string"
    (expect (inf-clojure--read-or-nil "([x] [x y])") :to-equal '([x] [x y])))
  (it "returns nil for the string nil"
    (expect (inf-clojure--read-or-nil "nil") :to-be nil))
  (it "returns nil for nil input"
    (expect (inf-clojure--read-or-nil nil) :to-be nil))
  (it "returns nil for unparseable input"
    (expect (inf-clojure--read-or-nil ")") :to-be nil))
  (it "reads only the first sexp"
    (expect (inf-clojure--read-or-nil "(a b) (c d)") :to-equal '(a b))))

(describe "inf-clojure-list-completions"
  (it "parses a list of completion strings"
    (expect (inf-clojure-list-completions "(\"defn\" \"def\" \"defmacro\")")
            :to-equal '("defn" "def" "defmacro")))
  (it "returns nil for nil response"
    (expect (inf-clojure-list-completions "nil") :to-be nil))
  (it "returns nil for non-list response"
    (expect (inf-clojure-list-completions "42") :to-be nil)))

(describe "inf-clojure--wrap-for-ns"
  (it "returns code unchanged when inf-clojure-eval-ns-aware is nil"
    (let ((inf-clojure-eval-ns-aware nil))
      (ict-with-assess-buffers
       ((a (insert "(ns my.app)\n(+ 1 2)")))
       (with-current-buffer a
         (expect (inf-clojure--wrap-for-ns "(+ 1 2)") :to-equal "(+ 1 2)")))))
  (it "wraps code with binding when inf-clojure-eval-ns-aware is non-nil"
    (let ((inf-clojure-eval-ns-aware t))
      (ict-with-assess-buffers
       ((a (insert "(ns myapp)\n(+ 1 2)")))
       (with-current-buffer a
         (expect (inf-clojure--wrap-for-ns "(+ 1 2)")
                 :to-equal "(binding [*ns* (find-ns 'myapp)] (eval '(do (+ 1 2))))")))))
  (it "returns code unchanged when no namespace is detected"
    (let ((inf-clojure-eval-ns-aware t))
      (ict-with-assess-buffers
       ((a (insert "(+ 1 2)")))
       (with-current-buffer a
         (expect (inf-clojure--wrap-for-ns "(+ 1 2)") :to-equal "(+ 1 2)"))))))

(describe "inf-clojure--string-boundaries"
  (it "returns full string bounds when no regexps given"
    (expect (inf-clojure--string-boundaries "hello" "=>")
            :to-equal '(0 5 5)))
  (it "finds prompt position"
    (expect (inf-clojure--string-boundaries "result\nuser=>" "=>")
            :to-equal '(0 13 11)))
  (it "respects beg-regexp and end-regexp"
    (expect (inf-clojure--string-boundaries "foo(bar)baz" "=>" "(" ")")
            :to-equal '(3 8 11))))

;;; inf-clojure-tests.el ends here
