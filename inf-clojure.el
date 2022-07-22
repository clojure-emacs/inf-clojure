;;; inf-clojure.el --- Run an external Clojure process in an Emacs buffer -*- lexical-binding: t; -*-

;; Copyright © 2014-2022 Bozhidar Batsov

;; Authors: Bozhidar Batsov <bozhidar@batsov.dev>
;;       Olin Shivers <shivers@cs.cmu.edu>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.dev>
;; URL: http://github.com/clojure-emacs/inf-clojure
;; Keywords: processes, comint, clojure
;; Version: 3.2.1
;; Package-Requires: ((emacs "25.1") (clojure-mode "5.11"))

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
;; This package provides basic interaction with a Clojure subprocess (REPL).
;; It's based on ideas from the popular `inferior-lisp` package.
;;
;; `inf-clojure` has two components - a nice Clojure REPL with
;; auto-completion and a minor mode (`inf-clojure-minor-mode`), which
;; extends `clojure-mode` with commands to evaluate forms directly in the
;; REPL.
;;
;; `inf-clojure` provides a set of essential features for interactive
;; Clojure(Script) development:
;;
;; * REPL
;; * Interactive code evaluation
;; * Code completion
;; * Definition lookup
;; * Documentation lookup
;; * ElDoc
;; * Apropos
;; * Macroexpansion
;; * Support connecting to socket REPLs
;; * Support for Lumo
;; * Support for Planck
;; * Support for Joker
;;
;; For a more powerful/full-featured solution see https://github.com/clojure-emacs/cider.
;;
;; If you're installing manually, you'll need to:
;;
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;
;;    (autoload 'inf-clojure "inf-clojure" "Run an inferior Clojure process" t)
;;    (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;;; Code:

(require 'comint)
(require 'clojure-mode)
(require 'eldoc)
(require 'thingatpt)
(require 'ansi-color)
(require 'cl-lib)
(require 'subr-x)

(defvar inf-clojure-startup-forms '((lein . "lein repl")
                                    (boot . "boot repl")
                                    (clojure . "clojure")
                                    (cljs . "clojure -m cljs.main -r")
                                    (planck . "planck -d")
                                    (babashka . "bb")
                                    (lumo . "lumo -d")
                                    (joker . "joker")))

(defvar inf-clojure-repl-features
  '((cljs . ((doc . "(cljs.repl/doc %s)")
             (source . "(cljs.repl/source %s)")
             (arglists . "(try (->> '%s cljs.core/resolve cljs.core/meta :arglists) (catch :default _ nil))")
             (apropos . "(cljs.repl/apropos \"%s\")")
             (ns-vars . "(cljs.repl/dir %s)")
             (set-ns . "(in-ns '%s)")
             (macroexpand . "(cljs.core/macroexpand '%s)")
             (macroexpand-1 . "(cljs.core/macroexpand-1 '%s)")))
    (lumo . ((load . "(clojure.core/load-file \"%s\")")
             (doc . "(lumo.repl/doc %s)")
             (source . "(lumo.repl/source %s)")
             (arglists .
                       "(let [old-value lumo.repl/*pprint-results*]
                          (set! lumo.repl/*pprint-results* false)
                          (js/setTimeout #(set! lumo.repl/*pprint-results* old-value) 0)
                          (lumo.repl/get-arglists \"%s\"))")
             (apropos . "(lumo.repl/apropos \"%s\")")
             (ns-vars . "(lumo.repl/dir %s)")
             (set-ns . "(in-ns '%s)")
             (macroexpand . "(macroexpand-1 '%s)")
             (macroexpand-1 . "(macroexpand-1 '%s)")
             (completion .
                         "(let [ret (atom nil)]
                            (lumo.repl/get-completions \"%s\" (fn [res] (reset! ret (map str res))))
                             @ret)")))
    (planck . ((load . "(load-file \"%s\")")
               (doc . "(planck.repl/doc %s)")
               (source . "(planck.repl/source %s)")
               (arglists . "(planck.repl/get-arglists \"%s\")")
               (apropos . "(doseq [var (sort (planck.repl/apropos \"%s\"))] (println (str var)))")
               (ns-vars . "(planck.repl/dir %s)")
               (set-ns . "(in-ns '%s)")
               (macroexpand . "(macroexpand '%s)")
               (macroexpand-1 . "(macroexpand-1 '%s)")
               (completion . "(seq (js->clj (#'planck.repl/get-completions \"%s\")))")))
    (joker . ((load . "(load-file \"%s\")")
              (doc . "(joker.repl/doc %s)")
              (arglists .
                        "(try
                            (:arglists
                             (joker.core/meta
                              (joker.core/resolve
                               (joker.core/read-string \"%s\"))))
                            (catch Error _ nil))")
              (set-ns . "(in-ns '%s)")
              (macroexpand . "(macroexpand '%s)")
              (macroexpand-1 . "(macroexpand-1 '%s)")))
    (babashka . ((load . "(clojure.core/load-file \"%s\")")
                 (doc . "(clojure.repl/doc %s)")
                 (source . "(clojure.repl/source %s)")
                 (arglists .
                           "(try (-> '%s clojure.core/resolve clojure.core/meta :arglists)
                              (catch Throwable e nil))")
                 (apropos . "(doseq [var (sort (clojure.repl/apropos \"%s\"))] (println (str var)))")
                 (ns-vars . "(clojure.repl/dir %s)")
                 (set-ns . "(clojure.core/in-ns '%s)")
                 (macroexpand . "(clojure.core/macroexpand '%s)")
                 (macroexpand-1 . "(clojure.core/macroexpand-1 '%s)")))
    (clojure . ((load . "(clojure.core/load-file \"%s\")")
                (doc . "(clojure.repl/doc %s)")
                (source . "(clojure.repl/source %s)")
                (arglists .
                          "(try
                             (:arglists
                              (clojure.core/meta
                               (clojure.core/resolve
                                (clojure.core/read-string \"%s\"))))
                            (catch #?(:clj Throwable :cljr Exception) e nil))")
                (apropos . "(doseq [var (sort (clojure.repl/apropos \"%s\"))] (println (str var)))")
                (ns-vars . "(clojure.repl/dir %s)")
                (set-ns . "(clojure.core/in-ns '%s)")
                (macroexpand . "(clojure.core/macroexpand '%s)")
                (macroexpand-1 . "(clojure.core/macroexpand-1 '%s)")))))

(defvar-local inf-clojure-repl-type nil
  "Symbol to define your REPL type.
Its root binding is nil and it can be further customized using
either `setq-local` or an entry in `.dir-locals.el`." )

(defvar inf-clojure-buffer nil
  "The current `inf-clojure' process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Clojure processes, you start the first up
with \\[inf-clojure].  It will be in a buffer named `*inf-clojure*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inf-clojure].  It will be in a new buffer,
named `*inf-clojure*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Clojure processes --
like `inf-clojure-eval-defun' or `inf-clojure-show-arglists' -- have to choose a
process to send to, when you have more than one Clojure process around.  This
is determined by the global variable `inf-clojure-buffer'.  Suppose you
have three inferior Clojures running:
    Buffer              Process
    foo                 inf-clojure
    bar                 inf-clojure<2>
    *inf-clojure*     inf-clojure<3>
If you do a \\[inf-clojure-eval-defun] command on some Clojure source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *inf-clojure*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `inf-clojure-buffer'.
This process selection is performed by function `inf-clojure-proc'.

Whenever \\[inf-clojure] fires up a new process, it resets
`inf-clojure-buffer' to be the new process's buffer.  If you only run
one process, this does the right thing.  If you run multiple
processes, you might need to change `inf-clojure-buffer' to
whichever process buffer you want to use.")

(defun inf-clojure--get-feature (repl-type feature no-error)
  "Get FEATURE for REPL-TYPE from repl-features.
If no-error is truthy don't error if feature is not present."
  (let ((feature-form (alist-get feature (alist-get repl-type inf-clojure-repl-features))))
    (cond (feature-form feature-form)
          (no-error nil)
          (t (error "%s not configured for %s" feature repl-type)))))

(defun inf-clojure-get-feature (proc feature &optional no-error)
  "Get FEATURE based on repl type for PROC."
  (let* ((repl-type (or (with-current-buffer (process-buffer proc)
                          inf-clojure-repl-type)
                        (error "REPL type is not known"))))
    (inf-clojure--get-feature repl-type feature no-error)))

(defun inf-clojure--update-feature (repl-type feature form)
  "Return a copy of the datastructure containing the repl features.
Given a REPL-TYPE (`clojure', `lumo', ...) and a FEATURE (`doc',
`apropos', ...) and a FORM this will return a new datastructure
that can be set as `inf-clojure-repl-features'."
  (let ((original (alist-get repl-type inf-clojure-repl-features)))
    (if original
        (cons (cons repl-type (cons (cons feature form) (assoc-delete-all feature original)))
              (assoc-delete-all repl-type inf-clojure-repl-features))
      (error "Attempted to update %s form of unknown REPL type %s"
             (symbol-name feature)
             (symbol-name repl-type)))))

(defun inf-clojure-update-feature (repl-type feature form)
  "Mutate the repl features to the new FORM.
Given a REPL-TYPE (`clojure', `lumo', ...) and a FEATURE (`doc',
`apropos', ...) and a FORM this will set
`inf-clojure-repl-features' with these new values."
  (setq inf-clojure-repl-features (inf-clojure--update-feature repl-type feature form)))

(defun inf-clojure-proc (&optional no-error)
  "Return the current inferior Clojure process.
When NO-ERROR is non-nil, don't throw an error when no process
has been found.  See also variable `inf-clojure-buffer'."
  (or (get-buffer-process (if (derived-mode-p 'inf-clojure-mode)
                              (current-buffer)
                            inf-clojure-buffer))
      (unless no-error
        (error "No Clojure subprocess; see variable `inf-clojure-buffer'"))))

(defun inf-clojure-repl-p (&optional buf)
  "Indicates if BUF is an inf-clojure REPL.
If BUF is nil then defaults to the current buffer.
Checks the mode and that there is a live process."
  (let ((buf (or buf (current-buffer))))
    (and (with-current-buffer buf (derived-mode-p 'inf-clojure-mode))
         (get-buffer-process buf)
         (process-live-p (get-buffer-process buf)))))

(defun inf-clojure-repls ()
  "Return a list of all inf-clojure REPL buffers."
  (let (repl-buffers)
    (dolist (b (buffer-list))
      (when (inf-clojure-repl-p b)
        (push (buffer-name b) repl-buffers)))
    repl-buffers))

(defun inf-clojure-set-repl (always-ask)
  "Set an inf-clojure buffer as the active (default) REPL.
If in a REPL buffer already, use that unless a prefix is used (or
ALWAYS-ASK).  Otherwise get a list of all active inf-clojure
REPLS and offer a choice.  It's recommended to rename REPL
buffers after they are created with `rename-buffer'."
  (interactive "P")
  (if (and (not always-ask)
           (inf-clojure-repl-p))
      (setq inf-clojure-buffer (current-buffer))
    (let ((repl-buffers (inf-clojure-repls)))
     (if (> (length repl-buffers) 0)
         (when-let ((repl-buffer (completing-read "Select default REPL: " repl-buffers nil t)))
           (setq inf-clojure-buffer (get-buffer repl-buffer)))
       (user-error "No buffers have an inf-clojure process")))))

(defvar inf-clojure--repl-type-lock nil
  "Global lock for protecting against proc filter race conditions.
See http://blog.jorgenschaefer.de/2014/05/race-conditions-in-emacs-process-filter.html")

(defun inf-clojure--prompt-repl-type ()
  "Set the REPL type to one of the available implementations."
  (interactive)
  (let ((types (mapcar #'car inf-clojure-repl-features)))
    (intern
     (completing-read "Set REPL type: "
                      (sort (mapcar #'symbol-name types) #'string-lessp)))))

(defgroup inf-clojure nil
  "Run an external Clojure process (REPL) in an Emacs buffer."
  :prefix "inf-clojure-"
  :group 'clojure
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/inf-clojure")
  :link '(emacs-commentary-link :tag "Commentary" "inf-clojure"))

(defconst inf-clojure-version
  (or (if (fboundp 'package-get-version)
          (package-get-version))
      "3.2.1")
  "The current version of `inf-clojure'.")

(defcustom inf-clojure-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean)

(defcustom inf-clojure-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior Clojure's input history.
Input matching this regexp is not saved on the input history in Inferior Clojure
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp)

(defun inf-clojure--modeline-info ()
  "Return modeline info for `inf-clojure-minor-mode'.
Either \"no process\" or \"buffer-name(repl-type)\""
  (if (and (bufferp inf-clojure-buffer)
           (buffer-live-p inf-clojure-buffer))
      (with-current-buffer inf-clojure-buffer
        (format "%s(%s)" (buffer-name (current-buffer)) inf-clojure-repl-type))
    "no process"))

(defvar inf-clojure-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-a") #'inf-clojure-show-arglists)
    (define-key map (kbd "C-c C-v") #'inf-clojure-show-var-documentation)
    (define-key map (kbd "C-c C-s") #'inf-clojure-show-var-source)
    (define-key map (kbd "C-c C-S-a") #'inf-clojure-apropos)
    (define-key map (kbd "C-c M-o") #'inf-clojure-clear-repl-buffer)
    (define-key map (kbd "C-c C-q") #'inf-clojure-quit)
    (define-key map (kbd "C-c C-z") #'inf-clojure-switch-to-recent-buffer)
    (easy-menu-define inf-clojure-mode-menu map
      "Inferior Clojure REPL Menu"
      '("Inf-Clojure REPL"
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        "--"
        ["Load file" inf-clojure-load-file t]
        "--"
        ["Show arglists" inf-clojure-show-arglists t]
        ["Show documentation for var" inf-clojure-show-var-documentation t]
        ["Show source for var" inf-clojure-show-var-source t]
        ["Apropos" inf-clojure-apropos t]
        "--"
        ["Clear REPL" inf-clojure-clear-repl-buffer]
        ["Restart" inf-clojure-restart]
        ["Quit" inf-clojure-quit]
        "--"
        ["Version" inf-clojure-display-version]))
    map))

(defvar inf-clojure-insert-commands-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'inf-clojure-insert-defun)
    (define-key map (kbd "C-d") #'inf-clojure-insert-defun)
    (define-key map (kbd "e") #'inf-clojure-insert-last-sexp)
    (define-key map (kbd "C-e") #'inf-clojure-insert-last-sexp)
    map))

(defvar inf-clojure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x")  #'inf-clojure-eval-defun)     ; Gnu convention
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp) ; Gnu convention
    (define-key map (kbd "C-c C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'inf-clojure-eval-defun)     ; SLIME/CIDER style
    (define-key map (kbd "C-c C-b") #'inf-clojure-eval-buffer)
    (define-key map (kbd "C-c C-r") #'inf-clojure-eval-region)
    (define-key map (kbd "C-c M-r") #'inf-clojure-reload)
    (define-key map (kbd "C-c C-n") #'inf-clojure-eval-form-and-next)
    (define-key map (kbd "C-c C-j") inf-clojure-insert-commands-map)
    (define-key map (kbd "C-c C-z") #'inf-clojure-switch-to-repl)
    (define-key map (kbd "C-c C-i") #'inf-clojure-show-ns-vars)
    (define-key map (kbd "C-c C-S-a") #'inf-clojure-apropos)
    (define-key map (kbd "C-c C-m") #'inf-clojure-macroexpand)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-a") #'inf-clojure-show-arglists)
    (define-key map (kbd "C-c C-v") #'inf-clojure-show-var-documentation)
    (define-key map (kbd "C-c C-s") #'inf-clojure-show-var-source)
    (define-key map (kbd "C-c M-n") #'inf-clojure-set-ns)
    (define-key map (kbd "C-c C-q") #'inf-clojure-quit)
    (define-key map (kbd "C-c M-c") #'inf-clojure-connect)
    (easy-menu-define inf-clojure-minor-mode-menu map
      "Inferior Clojure Minor Mode Menu"
      '("Inf-Clojure"
        ["Eval top-level sexp at point" inf-clojure-eval-defun t]
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        ["Eval region" inf-clojure-eval-region t]
        ["Eval buffer" inf-clojure-eval-buffer t]
        "--"
        ["Load file..." inf-clojure-load-file t]
        ["Reload file... " inf-clojure-reload t]
        "--"
        ["Switch to REPL" inf-clojure-switch-to-repl t]
        ["Set REPL ns" inf-clojure-set-ns t]
        "--"
        ["Show arglists" inf-clojure-show-arglists t]
        ["Show documentation for var" inf-clojure-show-var-documentation t]
        ["Show source for var" inf-clojure-show-var-source t]
        ["Show vars in ns" inf-clojure-show-ns-vars t]
        ["Apropos" inf-clojure-apropos t]
        ["Macroexpand" inf-clojure-macroexpand t]
        "--"
        ["Restart REPL" inf-clojure-restart]
        ["Quit REPL" inf-clojure-quit]))
    map))

;;;###autoload
(defcustom inf-clojure-mode-line
  '(:eval (format " inf-clojure[%s]" (inf-clojure--modeline-info)))
  "Mode line lighter for cider mode.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how inf-clojure-minor-mode
displays its status in the mode line.  The default value displays
the current REPL.  Set this variable to nil to disable the
mode line entirely."
  :type 'sexp
  :risky t)

(defcustom inf-clojure-enable-eldoc t
  "Var that allows disabling `eldoc-mode' in `inf-clojure'.

Set to nil to disable eldoc.  Eldoc can be quite useful by
displaying function signatures in the modeline, but can also
cause multiple prompts to appear in the REPL and mess with *1,
*2, etc."
  :type 'boolean
  :safe #'booleanp
  :package-version '(inf-clojure . "3.2.0"))

;;;###autoload
(define-minor-mode inf-clojure-minor-mode
  "Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}"
  :lighter inf-clojure-mode-line
  :keymap inf-clojure-minor-mode-map
  (setq-local comint-input-sender 'inf-clojure--send-string)
  (when inf-clojure-enable-eldoc
    (inf-clojure-eldoc-setup))
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'inf-clojure-completion-at-point))

(defun inf-clojure--endpoint-p (x)
  "Return non-nil if and only if X is a valid endpoint.

A valid endpoint consists of a host and port
number (e.g. (\"localhost\" . 5555))."
  (and
   (listp x)
   (stringp (car x))
   (numberp (cdr x))))

(defcustom inf-clojure-custom-startup
   nil
   "Form to be used to start `inf-clojure'.
Can be a cons pair of (host . port) where host is a string and
port is an integer, or a string to startup an interpreter like
\"planck\"."
   :type '(choice (cons string integer) (const nil)))

(defcustom inf-clojure-custom-repl-type
  nil
  "REPL type to use for `inf-clojure' process buffer.
Should be a symbol that is a key in `inf-clojure-repl-features'."
  :package-version '(inf-clojure . "3.0.0")
  :type '(choice (const :tag "clojure" clojure)
                 (const :tag "cljs" cljs)
                 (const :tag "lumo" lumo)
                 (const :tag "planck" planck)
                 (const :tag "joker" joker)
                 (const :tag "babashka" babashka)
                 (const :tag "determine at startup" nil)))

(defun inf-clojure--whole-comment-line-p (string)
  "Return non-nil iff STRING is a whole line semicolon comment."
  (string-match-p "^\s*;" string))

(defun inf-clojure--sanitize-command (command)
  "Sanitize COMMAND for sending it to a process.
An example of things that this function does is to add a final
newline at the end of the form.  Return an empty string if the
sanitized command is empty."
  (let ((sanitized (string-trim-right command)))
    (if (string-blank-p sanitized)
        ""
      (concat sanitized "\n"))))

(defun inf-clojure--send-string (proc string)
  "A custom `comint-input-sender` / `comint-send-string`.
It performs the required side effects on every send for PROC and
STRING (for example set the buffer local REPL type).  It should
always be preferred over `comint-send-string`.  It delegates to
`comint-simple-send` so it always appends a newline at the end of
the string for evaluation.  Refer to `comint-simple-send` for
customizations."
  (let ((sanitized (inf-clojure--sanitize-command string)))
    (inf-clojure--log-string sanitized "----CMD->")
    (comint-send-string proc sanitized)))

(defcustom inf-clojure-reload-form "(require '%s :reload)"
  "Format-string for building a Clojure expression to reload a file.
Reload forces loading of all the identified libs even if they are
already loaded.
This format string should use `%s' to substitute a namespace and
should result in a Clojure form that will be sent to the inferior
Clojure to load that file."
  :type 'string
  :safe #'stringp
  :package-version '(inf-clojure . "2.2.0"))

;; :reload forces loading of all the identified libs even if they are
  ;; already loaded
;; :reload-all implies :reload and also forces loading of all libs that the
;; identified libs directly or indirectly load via require or use

(defun inf-clojure-reload-form (_proc)
  "Return the form to query the Inf-Clojure PROC for reloading a namespace.
If you are using REPL types, it will pickup the most appropriate
`inf-clojure-reload-form` variant."
  inf-clojure-reload-form)

(defcustom inf-clojure-reload-all-form "(require '%s :reload-all)"
  "Format-string for building a Clojure expression to :reload-all a file.
Reload-all implies :reload and also forces loading of all libs
that the identified libs directly or indirectly load via require
or use.
This format string should use `%s' to substitute a namespace and
should result in a Clojure form that will be sent to the inferior
Clojure to load that file."
  :type 'string
  :safe #'stringp
  :package-version '(inf-clojure . "2.2.0"))

(defun inf-clojure-reload-all-form (_proc)
  "Return the form to query the Inf-Clojure PROC for :reload-all of a namespace.
If you are using REPL types, it will pickup the most appropriate
`inf-clojure-reload-all-form` variant."
  inf-clojure-reload-all-form)

(defcustom inf-clojure-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the Inferior Clojure mode."
  :type 'regexp)

(defcustom inf-clojure-subprompt " *#_=> *"
  "Regexp to recognize subprompts in the Inferior Clojure mode."
  :type 'regexp)

(defcustom inf-clojure-comint-prompt-regexp "^\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize both main prompt and subprompt for comint.
This should usually be a combination of `inf-clojure-prompt' and
`inf-clojure-subprompt'."
  :type 'regexp)

(defcustom inf-clojure-repl-use-same-window nil
  "Controls whether to display the REPL buffer in the current window or not."
  :type '(choice (const :tag "same" t)
                 (const :tag "different" nil))
  :safe #'booleanp
  :package-version '(inf-clojure . "2.0.0"))

(defcustom inf-clojure-auto-mode t
  "When non-nil, automatically enable inf-clojure-minor-mode for all Clojure buffers."
  :type 'boolean
  :safe #'booleanp
  :package-version '(inf-clojure . "3.1.0"))

(defun inf-clojure--clojure-buffers ()
  "Return a list of all existing `clojure-mode' buffers."
  (cl-remove-if-not
   (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'clojure-mode)))
   (buffer-list)))

(defun inf-clojure-enable-on-existing-clojure-buffers ()
  "Enable inf-clojure's minor mode on existing Clojure buffers.
See command `inf-clojure-minor-mode'."
  (interactive)
  (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
  (dolist (buffer (inf-clojure--clojure-buffers))
    (with-current-buffer buffer
      (inf-clojure-minor-mode +1))))

(defun inf-clojure-disable-on-existing-clojure-buffers ()
  "Disable command `inf-clojure-minor-mode' on existing Clojure buffers."
  (interactive)
  (dolist (buffer (inf-clojure--clojure-buffers))
    (with-current-buffer buffer
      (inf-clojure-minor-mode -1))))

(define-derived-mode inf-clojure-mode comint-mode "Inferior Clojure"
  "Major mode for interacting with an inferior Clojure process.
Runs a Clojure interpreter as a subprocess of Emacs, with Clojure
I/O through an Emacs buffer.  Variables of the type
`inf-clojure-*-cmd' combined with the project type controls how
a Clojure REPL is started.  Variables `inf-clojure-prompt',
`inf-clojure-filter-regexp' and `inf-clojure-load-form' can
customize this mode for different Clojure REPLs.

For information on running multiple processes in multiple buffers, see
documentation for variable `inf-clojure-buffer'.

\\{inf-clojure-mode-map}

Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`inf-clojure-mode-hook' (in that order).

You can send text to the inferior Clojure process from other buffers containing
Clojure source.
    `inf-clojure-switch-to-repl' switches the current buffer to the Clojure process buffer.
    `inf-clojure-eval-defun' sends the current defun to the Clojure process.
    `inf-clojure-eval-region' sends the current region to the Clojure process.

    Prefixing the inf-clojure-eval/defun/region commands with
    a \\[universal-argument] causes a switch to the Clojure process buffer after sending
    the text.

Commands:\\<inf-clojure-mode-map>
\\[comint-send-input] after the end of the process' output sends the text from the
    end of process to point.
\\[comint-send-input] before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
\\[comint-copy-old-input] copies the sexp ending at point to the end of the process' output,
    allowing you to edit it before sending it.
If `comint-use-prompt-regexp' is nil (the default), \\[comint-insert-input] on old input
   copies the entire old input to the end of the process' output, allowing
   you to edit it before sending it.  When not used on old input, or if
   `comint-use-prompt-regexp' is non-nil, \\[comint-insert-input] behaves according to
   its global binding.
\\[backward-delete-char-untabify] converts tabs to spaces as it moves back.
\\[clojure-indent-line] indents for Clojure; with argument, shifts rest
    of expression rigidly with the current line.
\\[indent-sexp] does \\[clojure-indent-line] on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (setq comint-input-sender 'inf-clojure--send-string)
  (setq comint-prompt-regexp inf-clojure-comint-prompt-regexp)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (when inf-clojure-enable-eldoc
    (inf-clojure-eldoc-setup))
  (setq comint-get-old-input #'inf-clojure-get-old-input)
  (setq comint-input-filter #'inf-clojure-input-filter)
  (setq-local comint-prompt-read-only inf-clojure-prompt-read-only)
  (add-hook 'comint-preoutput-filter-functions #'inf-clojure-preoutput-filter nil t)
  (add-hook 'completion-at-point-functions #'inf-clojure-completion-at-point nil t)
  (ansi-color-for-comint-mode-on)
  (when inf-clojure-auto-mode
    (inf-clojure-enable-on-existing-clojure-buffers)))

(defun inf-clojure-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (max (point) (comint-line-beginning-position)) end))))

(defun inf-clojure-input-filter (str)
  "Return t if STR does not match `inf-clojure-filter-regexp'."
  (not (string-match inf-clojure-filter-regexp str)))

(defun inf-clojure-chomp (string)
  "Remove final newline from STRING."
  (if (string-match "[\n]\\'" string)
      (replace-match "" t t string)
    string))

(defun inf-clojure-remove-subprompts (string)
  "Remove subprompts from STRING."
  (replace-regexp-in-string inf-clojure-subprompt "" string))

(defun inf-clojure-preoutput-filter (str)
  "Preprocess the output STR from interactive commands."
  (inf-clojure--log-string str "<-RES----")
  (cond
   ((string-prefix-p "inf-clojure-" (symbol-name (or this-command last-command)))
    ;; Remove subprompts and prepend a newline to the output string
    (inf-clojure-chomp (concat "\n" (inf-clojure-remove-subprompts str))))
   (t str)))

(defun inf-clojure-clear-repl-buffer ()
  "Clear the REPL buffer."
  (interactive)
  (with-current-buffer (if (derived-mode-p 'inf-clojure-mode)
                           (current-buffer)
                         inf-clojure-buffer)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer))))

(defun inf-clojure--swap-to-buffer-window (to-buffer)
  "Switch to `TO-BUFFER''s window."
  (let ((pop-up-frames
         ;; Be willing to use another frame
         ;; that already has the window in it.
         (or pop-up-frames
             (get-buffer-window to-buffer t))))
    (pop-to-buffer to-buffer '(display-buffer-reuse-window . ()))))

(defun inf-clojure-switch-to-repl (eob-p)
  "Switch to the inferior Clojure process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-clojure-buffer)
      (inf-clojure--swap-to-buffer-window inf-clojure-buffer)
    (call-interactively #'inf-clojure))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inf-clojure-switch-to-recent-buffer ()
  "Switch to the most recently used `inf-clojure-minor-mode' buffer."
  (interactive)
  (let ((recent-inf-clojure-minor-mode-buffer (seq-find (lambda (buf)
                                                          (with-current-buffer buf (bound-and-true-p inf-clojure-minor-mode)))
                                                        (buffer-list))))
    (if recent-inf-clojure-minor-mode-buffer
        (inf-clojure--swap-to-buffer-window recent-inf-clojure-minor-mode-buffer)
      (message "inf-clojure: No recent buffer known."))))

(defun inf-clojure-quit (&optional buffer)
  "Kill the REPL buffer and its underlying process.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic."
  (interactive)
  (let ((target-buffer (or buffer (inf-clojure-select-target-repl))))
    (when (get-buffer-process target-buffer)
      (delete-process target-buffer))
    (kill-buffer target-buffer)))

(defun inf-clojure-restart (&optional buffer)
  "Restart the REPL buffer and its underlying process.

You can pass the target BUFFER as an optional parameter
to suppress the usage of the target buffer discovery logic."
  (interactive)
  (let* ((target-buffer (or buffer (inf-clojure-select-target-repl)))
         (target-buffer-name (buffer-name target-buffer)))
    ;; TODO: Try to recycle the old buffer instead of killing and recreating it
    (inf-clojure-quit target-buffer)
    (call-interactively #'inf-clojure)
    (rename-buffer target-buffer-name)))

(defun inf-clojure--project-name (dir)
  "Extract a project name from a project DIR.
The name is simply the final segment of the path."
  (file-name-nondirectory (directory-file-name dir)))

;;;###autoload
(defun inf-clojure (cmd)
  "Run an inferior Clojure process, input and output via buffer `*inf-clojure*'.
If there is a process already running in `*inf-clojure*', just
switch to that buffer.

CMD is a string which serves as the startup command or a cons of
host and port.

 Prompts user for repl startup command and repl type if not
inferrable from startup command.  Uses `inf-clojure-custom-repl-type'
and `inf-clojure-custom-startup' if those are set.
Use a prefix to prevent using these when they
are set.

 Runs the hooks from `inf-clojure-mode-hook' (after the
`comint-mode-hook' is run).  \(Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive (list (or (unless current-prefix-arg
                           inf-clojure-custom-startup)
                         (completing-read "Select Clojure REPL startup command: "
                                          (mapcar #'cdr inf-clojure-startup-forms)
                                          nil
                                          'confirm-after-completion))))
  (let* ((project-dir (clojure-project-dir))
         (process-buffer-name (if project-dir
                                  (format "inf-clojure %s" (inf-clojure--project-name project-dir))
                                "inf-clojure"))
         ;; comint adds the asterisks to both sides
         (repl-buffer-name (format "*%s*" process-buffer-name)))
    ;; Create a new comint buffer if needed
    (unless (comint-check-proc repl-buffer-name)
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory (or project-dir default-directory))
            (cmdlist (if (consp cmd)
                         (list cmd)
                       (split-string-and-unquote cmd)))
            (repl-type (or (unless prefix-arg
                             inf-clojure-custom-repl-type)
                           (car (rassoc cmd inf-clojure-startup-forms))
                           (inf-clojure--prompt-repl-type))))
        (message "Starting Clojure REPL via `%s'..." cmd)
        (with-current-buffer (apply #'make-comint
                                    process-buffer-name (car cmdlist) nil (cdr cmdlist))
          (inf-clojure-mode)
          (set-syntax-table clojure-mode-syntax-table)
          (setq-local inf-clojure-repl-type repl-type)
          (hack-dir-local-variables-non-file-buffer))))
    ;; update the default comint buffer and switch to it
    (setq inf-clojure-buffer (get-buffer repl-buffer-name))
    (if inf-clojure-repl-use-same-window
        (pop-to-buffer-same-window repl-buffer-name)
      (pop-to-buffer repl-buffer-name))))

;;;###autoload
(defun inf-clojure-connect (host port)
  "Connect to a running socket REPL server via `inf-clojure'.
HOST is the host the process is running on, PORT is where it's listening."
  (interactive "shost: \nnport: ")
  (inf-clojure (cons host port)))

(defun inf-clojure--forms-without-newlines (str)
  "Remove newlines between toplevel forms.
STR is a string of contents to be evaluated.  When sending
multiple forms to a REPL, each newline triggers a prompt.
So we replace all newlines between top level forms but not inside
of forms."
  (condition-case nil
      (with-temp-buffer
        (progn
          (clojurec-mode)
          (insert str)
          (whitespace-cleanup)
          (goto-char (point-min))
          (while (not (eobp))
            (while (looking-at "\n")
              (delete-char 1))
            (unless (eobp)
              (clojure-forward-logical-sexp))
            (unless (eobp)
              (forward-char)))
          (buffer-substring-no-properties (point-min) (point-max))))
    (scan-error str)))

(defun inf-clojure-eval-region (start end &optional and-go)
  "Send the current region to the inferior Clojure process.
Sends substring between START and END.  Prefix argument AND-GO
means switch to the Clojure buffer afterwards."
  (interactive "r\nP")
  (let* ((str (buffer-substring-no-properties start end))
         ;; newlines over a socket repl between top level forms cause
         ;; a prompt to be returned. so here we dump the region into a
         ;; temp buffer, and delete all newlines between the forms
         (formatted (inf-clojure--forms-without-newlines str)))
    (inf-clojure--send-string (inf-clojure-proc) formatted))
  (when and-go (inf-clojure-switch-to-repl t)))

(defun inf-clojure-eval-string (code)
  "Send the string CODE to the inferior Clojure process to be executed."
  (inf-clojure--send-string (inf-clojure-proc) code))

(defun inf-clojure--defun-at-point (&optional bounds)
  "Return text or range of defun at point.
If BOUNDS is truthy return a dotted pair of beginning and end of
current defun else return the string.."
  (save-excursion
    (end-of-defun)
    (let ((end (point))
          (case-fold-search t)
          (func (if bounds #'cons #'buffer-substring-no-properties)))
      (beginning-of-defun)
      (funcall func (point) end))))

(defun inf-clojure-eval-defun (&optional and-go)
  "Send the current defun to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (let ((bounds (inf-clojure--defun-at-point t)))
     (inf-clojure-eval-region (car bounds) (cdr bounds) and-go))))

(defun inf-clojure-eval-buffer (&optional and-go)
  "Send the current buffer to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (widen)
    (let ((case-fold-search t))
      (inf-clojure-eval-region (point-min) (point-max) and-go))))

(defun inf-clojure-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (inf-clojure-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun inf-clojure-eval-form-and-next ()
  "Send the previous sexp to the inferior Clojure process and move to the next one."
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (inf-clojure-eval-last-sexp)
  (forward-sexp))

(defun inf-clojure-insert-and-eval (form)
  "Insert FORM into process and evaluate.
Indent FORM.  FORM is expected to have been trimmed."
  (let ((clojure-process (inf-clojure-proc)))
    (with-current-buffer (process-buffer clojure-process)
      (comint-goto-process-mark)
      (let ((beginning (point)))
        (insert (format "%s" form))
        (let ((end (point)))
          (goto-char beginning)
          (indent-sexp end)
          ;; font-lock the inserted code
          (font-lock-ensure beginning end)))
      (comint-send-input t t))))

(defun inf-clojure-insert-defun ()
  "Send current defun to process."
  (interactive)
  (inf-clojure-insert-and-eval (string-trim (inf-clojure--defun-at-point))))

(defun inf-clojure-insert-last-sexp ()
  "Send last sexp to process."
  (interactive)
  (inf-clojure-insert-and-eval
   (buffer-substring-no-properties (save-excursion (backward-sexp) (point))
                                   (point))))

;; Now that inf-clojure-eval-/defun/region takes an optional prefix arg,
;; these commands are redundant. But they are kept around for the user
;; to bind if he wishes, for backwards functionality, and because it's
;; easier to type C-c e than C-u C-c C-e.

(defun inf-clojure-eval-region-and-go (start end)
  "Send the current region to the inferior Clojure, and switch to its buffer.
START and END are the beginning and end positions in the buffer to send."
  (interactive "r")
  (inf-clojure-eval-region start end t))

(defun inf-clojure-eval-defun-and-go ()
  "Send the current defun to the inferior Clojure, and switch to its buffer."
  (interactive)
  (inf-clojure-eval-defun t))

(defvar inf-clojure-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `inf-clojure-load-file' command.")

(defcustom inf-clojure-source-modes '(clojure-mode)
  "Used to determine if a buffer contains Clojure source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Clojure source file by `inf-clojure-load-file'.
Used by this command to determine defaults."
  :type '(repeat symbol))

(defun inf-clojure-load-file (&optional switch-to-repl file-name)
  "Load a Clojure file into the inferior Clojure process.

The prefix argument SWITCH-TO-REPL controls whether to switch to
REPL after the file is loaded or not.  If the argument FILE-NAME
is present it will be used instead of the current file."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (file-name (or file-name
                        (car (comint-get-source "Load Clojure file: " inf-clojure-prev-l/c-dir/file
                                                ;; nil because doesn't need an exact name
                                                inf-clojure-source-modes nil))))
         (load-form (inf-clojure-get-feature proc 'load)))
    (comint-check-source file-name) ; Check to see if buffer needs saved.
    (setq inf-clojure-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                              (file-name-nondirectory file-name)))
    (inf-clojure--send-string proc (format load-form file-name))
    (when switch-to-repl
      (inf-clojure-switch-to-repl t))))

(defun inf-clojure-reload (arg)
  "Send a query to the inferior Clojure for reloading the namespace.
See variable `inf-clojure-reload-form' and
`inf-clojure-reload-all-form'.

The prefix argument ARG can change the behavior of the command:

  - C-u M-x `inf-clojure-reload': prompts for a namespace name.
  - M-- M-x `inf-clojure-reload': executes (require ... :reload-all).
  - M-- C-u M-x `inf-clojure-reload': reloads all AND prompts."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (reload-all-p (or (equal arg '-) (equal arg '(-4))))
         (prompt-p (or (equal arg '(4)) (equal arg '(-4))))
         (ns (if prompt-p
                (car (inf-clojure-symprompt "Namespace" (clojure-find-ns)))
              (clojure-find-ns)))
         (form (if (not reload-all-p)
                   (inf-clojure-reload-form proc)
                 (inf-clojure-reload-all-form proc))))
    (inf-clojure--send-string proc (format form ns))))

(defun inf-clojure-connected-p ()
  "Return t if inferior Clojure is currently connected, nil otherwise."
  (not (null inf-clojure-buffer)))



;;; Ancillary functions
;;; ===================

(defun inf-clojure-symprompt (prompt default)
  "Read a string from the user.

It allows to specify a PROMPT string and a DEFAULT string to
display."
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))


;; Adapted from function-called-at-point in help.el.
(defun inf-clojure-fn-called-at-pt ()
  "Return the name of the function called in the current call.
The value is nil if it can't find one."
  (condition-case nil
      (save-excursion
        (save-restriction
          (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
          (backward-up-list 1)
          (forward-char 1)
          (let ((obj (read (current-buffer))))
            (and (symbolp obj) obj))))
    (error nil)))

(defun inf-clojure-symbol-at-point ()
  "Return the name of the symbol at point, otherwise nil."
  (or (thing-at-point 'symbol) ""))

;;; Documentation functions: var doc and arglists.
;;; ======================================================================

(defun inf-clojure-show-var-documentation (prompt-for-symbol)
  "Send a form to the inferior Clojure to give documentation for VAR.
See function `inf-clojure-var-doc-form'.  When invoked with a
prefix argument PROMPT-FOR-SYMBOL, it prompts for a symbol name."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (var (if prompt-for-symbol
                  (car (inf-clojure-symprompt "Var doc" (inf-clojure-symbol-at-point)))
                (inf-clojure-symbol-at-point)))
         (doc-form (inf-clojure-get-feature proc 'doc)))
    (inf-clojure--send-string proc (format doc-form var))))

(defun inf-clojure-show-var-source (prompt-for-symbol)
  "Send a command to the inferior Clojure to give source for VAR.
See variable `inf-clojure-var-source-form'.  When invoked with a
prefix argument PROMPT-FOR-SYMBOL, it prompts for a symbol name."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (var (if prompt-for-symbol
                  (car (inf-clojure-symprompt "Var source" (inf-clojure-symbol-at-point)))
                (inf-clojure-symbol-at-point)))
         (source-form (inf-clojure-get-feature proc 'source)))
    (inf-clojure--send-string proc (format source-form var))))

;;;; Response parsing
;;;; ================

(defvar inf-clojure--redirect-buffer-name " *Inf-Clojure Redirect Buffer*"
  "The name of the buffer used for process output redirection.")

(defvar inf-clojure--log-file-name ".inf-clojure.log"
  "The name of the file used to log process activity.")

(defvar inf-clojure-log-activity nil
  "Log process activity?.
Inf-Clojure will create a log file in the project folder named
`inf-clojure--log-file-name' and dump the process activity in it
in case this is not nil." )

(defun inf-clojure--log-string (string &optional tag)
  "Log STRING to file, according to `inf-clojure-log-response'.
The optional TAG will be converted to string and printed before
STRING if present."
  (when inf-clojure-log-activity
    (write-region (concat "\n"
                          (when tag
                            (if (stringp tag)
                              (concat tag "\n")
                              (concat (prin1-to-string tag) "\n")))
                          (let ((print-escape-newlines t))
                            (prin1-to-string (substring-no-properties string))))
                  nil
                  (expand-file-name inf-clojure--log-file-name
                                    (clojure-project-dir))
                  'append
                  'no-annoying-write-file-in-minibuffer)))

(defun inf-clojure--string-boundaries (string prompt &optional beg-regexp end-regexp)
  "Calculate the STRING boundaries, including PROMPT.
Return a list of positions (beginning end prompt).  If the
optional BEG-REGEXP and END-REGEXP are present, the boundaries
are going to match those."
  (list (or (and beg-regexp (string-match beg-regexp string)) 0)
        (or (and end-regexp (when (string-match end-regexp string)
                              (match-end 0)))
            (length string))
        (or (string-match prompt string) (length string))))

(defun inf-clojure--get-redirect-buffer ()
  "Get the redirection buffer, creating it if necessary.

It is the buffer used for processing REPL responses, see variable
\\[inf-clojure--redirect-buffer-name]."
  (or (get-buffer inf-clojure--redirect-buffer-name)
      (let ((buffer (generate-new-buffer inf-clojure--redirect-buffer-name)))
        (with-current-buffer buffer
          (hack-dir-local-variables-non-file-buffer)
          buffer))))

;; Originally from:
;;   https://github.com/glycerine/lush2/blob/master/lush2/etc/lush.el#L287
(defun inf-clojure--process-response (command process &optional beg-regexp end-regexp)
  "Send COMMAND to PROCESS and return the response.
Return the result of COMMAND, filtering it from BEG-REGEXP to the
end of the matching END-REGEXP if non-nil.
If BEG-REGEXP is nil, the result string will start from (point)
in the results buffer.  If END-REGEXP is nil, the result string
will end at (point-max) in the results buffer.  It cuts out the
output from and including the `inf-clojure-prompt`."
  (let ((redirect-buffer-name inf-clojure--redirect-buffer-name)
        (sanitized-command (inf-clojure--sanitize-command command)))
    (when (not (string-empty-p sanitized-command))
      (inf-clojure--log-string command "----CMD->")
      (with-current-buffer (inf-clojure--get-redirect-buffer)
        (erase-buffer)
        (comint-redirect-send-command-to-process sanitized-command redirect-buffer-name process nil t))
      ;; Wait for the process to complete
      (with-current-buffer (process-buffer process)
        (while (and (null comint-redirect-completed)
                    (accept-process-output process 1 0 t))
          (sleep-for 0.01)))
      ;; Collect the output
      (with-current-buffer redirect-buffer-name
        (goto-char (point-min))
        (let* ((buffer-string (buffer-substring-no-properties (point-min) (point-max)))
               (boundaries (inf-clojure--string-boundaries buffer-string inf-clojure-prompt beg-regexp end-regexp))
               (beg-pos (car boundaries))
               (end-pos (car (cdr boundaries)))
               (prompt-pos (car (cdr (cdr boundaries))))
               (response-string (substring buffer-string beg-pos (min end-pos prompt-pos))))
          (inf-clojure--log-string buffer-string "<-RES----")
          response-string)))))

(defun inf-clojure--nil-string-match-p (string)
  "Return non-nil iff STRING is not nil.
This function also takes into consideration weird escape
character and matches if nil is anywhere within the input
string."
  (string-match-p "\\Ca*nil\\Ca*" string))

(defun inf-clojure--some (data)
  "Return DATA unless nil or includes \"nil\" as string."
  (cond
   ((null data) nil)
   ((and (stringp data)
         (inf-clojure--nil-string-match-p data)) nil)
   (t data)))

(defun inf-clojure--read-or-nil (response)
  "Read RESPONSE and return it as data.

If response is nil or includes the \"nil\" string return nil
instead.

Note that the read operation will always return the first
readable sexp only."
  ;; The following reads the first LISP expression
  (inf-clojure--some
   (when response
     (ignore-errors (read response)))))

(defun inf-clojure--process-response-match-p (match-p proc form)
  "Eval MATCH-P on the response of sending to PROC the input FORM.
Note that this function will add a \n to the end of the string
for evaluation, therefore FORM should not include it."
  (let ((response (inf-clojure--process-response form proc)))
    (when response (funcall match-p response))))

(defun inf-clojure--some-response-p (proc form)
  "Return non-nil iff PROC's response after evaluating FORM is not nil."
  (inf-clojure--process-response-match-p
   (lambda (string)
     (not (inf-clojure--nil-string-match-p (string-trim string))))
   proc form))

;;;; Commands
;;;; ========

(defun inf-clojure-arglists (fn)
  "Send a query to the inferior Clojure for the arglists for function FN.
See variable `inf-clojure-arglists-form'."
  (when-let ((proc (inf-clojure-proc 'no-error)))
    (when-let ((arglists-form (inf-clojure-get-feature proc 'arglists)))
      (thread-first (format arglists-form fn)
        (inf-clojure--process-response proc "(" ")")
        (inf-clojure--some)))))

(defun inf-clojure-show-arglists (prompt-for-symbol)
  "Show the arglists for function FN in the mini-buffer.
See variable `inf-clojure-arglists-form'.  When invoked with a
prefix argument PROMPT-FOR-SYMBOL, it prompts for a symbol name."
  (interactive "P")
  (let* ((fn (if prompt-for-symbol
                 (car (inf-clojure-symprompt "Arglists" (inf-clojure-fn-called-at-pt)))
               (inf-clojure-fn-called-at-pt)))
         (eldoc (inf-clojure-arglists fn)))
    (if eldoc
        (message "%s: %s" fn eldoc)
      (message "Arglists not supported for this repl"))))

(defun inf-clojure-show-ns-vars (prompt-for-ns)
  "Send a query to the inferior Clojure for the public vars in NS.
See variable `inf-clojure-ns-vars-form'.  When invoked with a
prefix argument PROMPT-FOR-NS, it prompts for a namespace name."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (ns (if prompt-for-ns
                 (car (inf-clojure-symprompt "Ns vars" (clojure-find-ns)))
               (clojure-find-ns)))
         (ns-vars-form (inf-clojure-get-feature proc 'ns-vars)))
    (inf-clojure--send-string proc (format ns-vars-form ns))))

(defun inf-clojure-set-ns (prompt-for-ns)
  "Set the ns of the inferior Clojure process to NS.
See variable `inf-clojure-set-ns-form'.  It defaults to the ns of
the current buffer.  When invoked with a prefix argument
PROMPT-FOR-NS, it prompts for a namespace name."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (ns (if prompt-for-ns
                 (car (inf-clojure-symprompt "Set ns to" (clojure-find-ns)))
               (clojure-find-ns)))
         (set-ns-form (inf-clojure-get-feature proc 'set-ns)))
    (when (or (not ns) (equal ns ""))
      (user-error "No namespace selected"))
    (inf-clojure--send-string proc (format set-ns-form ns))))

(defun inf-clojure-apropos (expr)
  "Send an expression to the inferior Clojure for apropos.
EXPR can be either a regular expression or a stringable
thing.  See variable `inf-clojure-apropos-form'."
  (interactive (inf-clojure-symprompt "Var apropos" (inf-clojure-symbol-at-point)))
  (let* ((proc (inf-clojure-proc))
         (apropos-form (inf-clojure-get-feature proc 'apropos)))
    (inf-clojure--send-string proc (format apropos-form expr))))

(defun inf-clojure-macroexpand (&optional macro-1)
  "Send a form to the inferior Clojure for macro expansion.
See variable `inf-clojure-macroexpand-form'.
With a prefix arg MACRO-1 uses function `inf-clojure-macroexpand-1-form'."
  (interactive "P")
  (let* ((proc (inf-clojure-proc))
         (last-sexp (buffer-substring-no-properties (save-excursion (backward-sexp) (point)) (point)))
         (macroexpand-form (inf-clojure-get-feature proc
                                                    (if macro-1
                                                        'macroexpand-1
                                                      'macroexpand))))
    (inf-clojure--send-string
     proc
     (format macroexpand-form last-sexp))))

(defun inf-clojure--list-or-nil (data)
  "Return DATA if and only if it is a list."
  (when (listp data) data))

(defun inf-clojure-list-completions (response-str)
  "Parse completions from RESPONSE-STR.

Its only ability is to parse a Lisp list of candidate strings,
every other EXPR will be discarded and nil will be returned."
  (thread-first
      response-str
    (inf-clojure--read-or-nil)
    (inf-clojure--list-or-nil)))

(defcustom inf-clojure-completions-fn 'inf-clojure-list-completions
  "The function that parses completion results.

It is a single-arity function that will receive the REPL
evaluation result of \\[inf-clojure-completion-form] as string and
should return elisp data compatible with your completion mode.

The easiest possible data passed in input is a list of
candidates (e.g.: (\"def\" \"defn\")) but more complex libraries
like `alexander-yakushev/compliment' can return other things like
edn.

The expected return depends on the mode that you use for
completion: usually it is something compatible with
\\[completion-at-point-functions] but other modes like
`company-mode' allow an even higher level of sophistication.

The default value is the `inf-clojure-list-completions' function,
which is able to parse results in list form only.  You can peek
at its implementation for getting to know some utility functions
you might want to use in your customization."
  :type 'function
  :package-version '(inf-clojure . "2.1.0"))

(defun inf-clojure-completions (expr)
  "Return completions for the Clojure expression starting with EXPR.

Under the hood it calls the function
\\[inf-clojure-completions-fn] passing in the result of
evaluating \\[inf-clojure-completion-form] at the REPL."
  (let* ((proc (inf-clojure-proc 'no-error))
         (completion-form (inf-clojure-get-feature proc 'completion t)))
    (when (and proc completion-form (not (string-blank-p expr)))
      (let ((completion-expr (format completion-form (substring-no-properties expr))))
        (funcall inf-clojure-completions-fn
                 (inf-clojure--process-response completion-expr proc  "(" ")"))))))

(defconst inf-clojure-clojure-expr-break-chars "^[] \"'`><,;|&{()[@\\^]"
  "Regexp are hard.

This regex has been built in order to match the first of the
listed chars.  There are a couple of quirks to consider:

- the ] is always a special in elisp regex so you have to put it
  directly AFTER [ if you want to match it as literal.
- The ^ needs to be escaped with \\^.

Tests and `re-builder' are your friends.")

(defun inf-clojure--kw-to-symbol (kw)
  "Convert the keyword KW to a symbol.

This guy was taken from CIDER, thanks folks."
  (when kw
    (replace-regexp-in-string "\\`:+" "" kw)))

(defun inf-clojure-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let* ((end (point))
             (skipped-back (skip-chars-backward inf-clojure-clojure-expr-break-chars))
             (start (+ end skipped-back))
             (chars (or (thing-at-point 'symbol)
                        (inf-clojure--kw-to-symbol (buffer-substring start end)))))
        (when (> (length chars) 0)
          (let ((first-char (substring-no-properties chars 0 1)))
            (when (string-match-p "[^0-9]" first-char)
              (cons (point) end))))))))

(defun inf-clojure-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (inf-clojure-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring (car bounds) (cdr bounds)))))

(defun inf-clojure-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (let ((bounds (inf-clojure-completion-bounds-of-expr-at-point)))
    (when (and bounds (inf-clojure-get-feature (inf-clojure-proc) 'completion 'no-error))
      (list (car bounds) (cdr bounds)
            (if (fboundp 'completion-table-with-cache)
                (completion-table-with-cache #'inf-clojure-completions)
              (completion-table-dynamic #'inf-clojure-completions))))))

;;;; ElDoc
;;;; =====

(defvar inf-clojure-extra-eldoc-commands '("yas-expand")
  "Extra commands to be added to eldoc's safe commands list.")

(defvar-local inf-clojure-eldoc-last-symbol nil
  "The eldoc information for the last symbol we checked.")

(defun inf-clojure-eldoc-format-thing (thing)
  "Format the eldoc THING."
  (propertize thing 'face 'font-lock-function-name-face))

(defun inf-clojure-eldoc-beginning-of-sexp ()
  "Move to the beginning of current sexp.

Return the number of nested sexp the point was over or after."
  (let ((parse-sexp-ignore-comments t)
        (num-skipped-sexps 0))
    (condition-case _
        (progn
          ;; First account for the case the point is directly over a
          ;; beginning of a nested sexp.
          (condition-case _
              (let ((p (point)))
                (forward-sexp -1)
                (forward-sexp 1)
                (when (< (point) p)
                  (setq num-skipped-sexps 1)))
            (error))
          (while
              (let ((p (point)))
                (forward-sexp -1)
                (when (< (point) p)
                  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

(defun inf-clojure-eldoc-info-in-current-sexp ()
  "Return a list of the current sexp and the current argument index."
  (save-excursion
    (let ((argument-index (1- (inf-clojure-eldoc-beginning-of-sexp))))
      ;; If we are at the beginning of function name, this will be -1.
      (when (< argument-index 0)
        (setq argument-index 0))
      ;; Don't do anything if current word is inside a string, vector,
      ;; hash or set literal.
      (if (member (or (char-after (1- (point))) 0) '(?\" ?\{ ?\[))
          nil
        (list (inf-clojure-symbol-at-point) argument-index)))))

(defun inf-clojure-eldoc-arglists (thing)
  "Return the arglists for THING."
  (when (and thing
             (not (string= thing ""))
             (not (string-prefix-p ":" thing)))
    ;; check if we can used the cached eldoc info
    (if (string= thing (car inf-clojure-eldoc-last-symbol))
        (cdr inf-clojure-eldoc-last-symbol)
      (let ((arglists (inf-clojure-arglists (substring-no-properties thing))))
        (when arglists
          (setq inf-clojure-eldoc-last-symbol (cons thing arglists))
          arglists)))))

(defun inf-clojure-eldoc ()
  "Backend function for eldoc to show argument list in the echo area."
  ;; todo: this never gets unset once connected and is a lie
  (when (and (inf-clojure-connected-p)
             inf-clojure-enable-eldoc
             ;; don't clobber an error message in the minibuffer
             (not (member last-command '(next-error previous-error))))
    (let* ((info (inf-clojure-eldoc-info-in-current-sexp))
           (thing (car info))
           (value (inf-clojure-eldoc-arglists thing)))
      (when value
        (format "%s: %s"
                (inf-clojure-eldoc-format-thing thing)
                value)))))

(defun inf-clojure-eldoc-setup ()
  "Turn on eldoc mode in the current buffer."
  (setq-local eldoc-documentation-function #'inf-clojure-eldoc)
  (apply #'eldoc-add-command inf-clojure-extra-eldoc-commands))

(defun inf-clojure-display-version ()
  "Display the current `inf-clojure' in the minibuffer."
  (interactive)
  (message "inf-clojure (version %s)" inf-clojure-version))

(defun inf-clojure-select-target-repl ()
  "Find or select an ‘inf-clojure’ buffer to operate on.

Useful for commands that can invoked outside of an ‘inf-clojure’ buffer
\\(e.g. from a Clojure buffer\\)."
  ;; if we're in a inf-clojure buffer we simply return in
  (if (eq major-mode 'inf-clojure-mode)
      (current-buffer)
    ;; otherwise we sift through all the inf-clojure buffers that are available
    (let ((repl-buffers (cl-remove-if-not (lambda (buf)
                                            (with-current-buffer buf
                                              (eq major-mode 'inf-clojure-mode)))
                                          (buffer-list))))
      (cond
       ((null repl-buffers) (user-error "No inf-clojure buffers found"))
       ((= (length repl-buffers) 1) (car repl-buffers))
       (t (get-buffer (completing-read "Select target inf-clojure buffer: "
                                       (mapcar #'buffer-name repl-buffers))))))))

(defun inf-clojure--response-match-p (form match-p proc)
  "Send FORM and apply MATCH-P on the result of sending it to PROC.
Note that this function will add a \n to the end of the string
for evaluation, therefore FORM should not include it."
  (funcall match-p (inf-clojure--process-response form proc nil)))

(provide 'inf-clojure)

;; Local variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; inf-clojure.el ends here
