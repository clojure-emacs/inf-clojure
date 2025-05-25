;;; inf-clojure-tests.el --- Tests for Inf-Clojure -*- lexical-binding: t; -*-
;;
;; Copyright © 2014-2021 Bozhidar Batsov

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

;;; inf-clojure-tests.el ends here
