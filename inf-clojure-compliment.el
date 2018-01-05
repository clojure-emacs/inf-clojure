;;; inf-clojure-compliment.el --- Compliment completion file for Inf-Clojure -*- lexical-binding: t; -*-
;;
;; Copyright © 2014-2018 Bozhidar Batsov

;; Authors: Bozhidar Batsov <bozhidar@batsov.com>
;;       Andrea Richiardi <a.richiardi.work@gmail.com>
;; URL: http://github.com/clojure-emacs/inf-clojure
;; Keywords: processes, clojure
;; Version: 2.1.0
;; Package-Requires: ((emacs "24.4") (edn "1.1.2"))

;; This file is part of GNU Emacs.

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
;; Code completion using alexander-yakushev/compliment.

;;; Code:

(require 'edn)

(defconst inf-clojure-compliment--annotations-alist
  '((:class "c")
    (:field "fi")
    (:function "f")
    (:import "i")
    (:keyword "k")
    (:local "l")
    (:macro "m")
    (:method "me")
    (:namespace "n")
    (:protocol "p")
    (:protocol-function "pf")
    (:record "r")
    (:special-form "s")
    (:static-field "sf")
    (:static-method "sm")
    (:type "t")
    (:var "v"))
  "Controls the abbreviations used when annotating completion candidates.

Must be a list of elements with the form (TYPE . ABBREVIATION), where TYPE
is a possible value of the candidate's type returned from the completion
backend, and ABBREVIATION is a short form of that type.")

(defun inf-clojure-compliment--get-candidate-type (symbol)
  "Get candidate type for SYMBOL."
  (let ((type (get-text-property 0 'type symbol)))
    (or (cadr (assoc type inf-clojure-compliment--annotations-alist))
        type)))

(defun inf-clojure-compliment--get-candidate-ns (symbol)
  "Get candidate ns for SYMBOL."
  (get-text-property 0 'ns symbol))

(defun inf-clojure-compliment--annotate-symbol (symbol)
  "Return a string suitable for annotating SYMBOL.
If SYMBOL has a text property `type` whose value is recognised, its
abbreviation according to `cider-completion-annotations-alist' will be
used.  If `type` is present but not recognised, its value will be used
unaltered."
  (let* ((type (inf-clojure-compliment--get-candidate-type symbol))
         (ns (inf-clojure-compliment--get-candidate-ns symbol)))
    (concat (when ns (format " (%s)" ns))
            (when type (format " <%s>" type)))))

(defun inf-clojure-compliment--parse-candidate (candidate)
  "Get \"candidate\" from CANDIDATE.
Put type and ns properties on the candidate"
  (let ((entry (gethash :candidate candidate))
        (type (gethash :type candidate))
        (ns (gethash :ns candidate)))
    (put-text-property 0 1 'type type entry)
    (put-text-property 0 1 'ns ns entry)
    entry))

(defun inf-clojure-compliment--completions (response-str)
  "Parse completions from RESPONSE-STR.

Its only ability is to parse a Lisp list of candidate strings,
every other EXPR will be discarded and nil will be returned."
  (mapcar #'inf-clojure-compliment--parse-candidate
          (thread-first
              response-str
            (inf-clojure--some)
            (edn-read))))

(setq inf-clojure-completions-fn #'inf-clojure-compliment--completions)
(setq inf-clojure-completions-annotation-fn #'inf-clojure-compliment--annotate-symbol)
(setq inf-clojure-completion-form "(do (require '[compliment.core :as comp]) (compliment.core/completions \"%s\"))")

(provide 'inf-clojure-compliment)

;; Local variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; inf-clojure-compliment.el ends here
