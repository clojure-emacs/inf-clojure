;;; inf-clojure.el --- an inferior-clojure mode -*- lexical-binding: t; -*-

;; Copyright (C) Bozhidar Batsov

;; Authors: Bozhidar Batsov <bozhidar@batsov.com>
;;       Olin Shivers <shivers@cs.cmu.edu>
;; URL: http://github.com/clojure-emacs/inf-clojure
;; Keywords: processes, clojure
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.1") (clojure-mode "4.0"))

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
;; inf-lisp adapted for Clojure.
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;
;;    (autoload 'inf-clojure "inf-clojure" "Run an inferior Clojure process" t)
;;    (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)

;;; Code:

(require 'comint)
(require 'clojure-mode)


(defgroup inf-clojure nil
  "Run an outside Clojure in an Emacs buffer."
  :group 'clojure)

(defcustom inf-clojure-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-clojure)

(defcustom inf-clojure-filter-regexp
  "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "What not to save on inferior Clojure's input history.
Input matching this regexp is not saved on the input history in Inferior Clojure
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword
\(as in :a, :c, etc.)"
  :type 'regexp
  :group 'inf-clojure)

(defvar inf-clojure-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-x\C-e" 'inf-clojure-eval-last-sexp)
    (define-key map "\C-c\C-l" 'inf-clojure-load-file)
    (define-key map "\C-c\C-a" 'inf-clojure-show-arglist)
    (define-key map "\C-c\C-v" 'inf-clojure-show-var-documentation)
    (define-key map "\C-c\C-s" 'inf-clojure-show-var-source)
    map))

(easy-menu-define
  inf-clojure-menu
  inf-clojure-mode-map
  "Inferior Clojure Menu"
  '("Inf-Clojure"
    ["Eval Last Sexp" inf-clojure-eval-last-sexp t]
    "--"
    ["Load File..." inf-clojure-load-file t]
    "--"
    ["Show Arglist..." inf-clojure-show-arglist t]
    ["Show Documentation for Var..." inf-clojure-show-var-documentation t]
    ["Show Source for Var..." inf-clojure-show-var-source t]))

(defvar inf-clojure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x"  'inf-clojure-eval-defun)     ; Gnu convention
    (define-key map "\C-x\C-e" 'inf-clojure-eval-last-sexp) ; Gnu convention
    (define-key map "\C-c\C-e" 'inf-clojure-eval-defun)
    (define-key map "\C-c\C-r" 'inf-clojure-eval-region)
    (define-key map "\C-c\C-n" 'inf-clojure-eval-form-and-next)
    (define-key map "\C-c\C-p" 'inf-clojure-eval-paragraph)
    (define-key map "\C-c\C-z" 'inf-clojure-switch-to-repl)
    (define-key map "\C-c\C-i" 'inf-clojure-show-ns-vars)
    (define-key map "\C-c\C-A" 'inf-clojure-apropos)
    (define-key map "\C-c\C-m" 'inf-clojure-macroexpand)
    (define-key map "\C-c\C-l" 'inf-clojure-load-file)
    (define-key map "\C-c\C-a" 'inf-clojure-show-arglist)
    (define-key map "\C-c\C-v" 'inf-clojure-show-var-documentation)
    (define-key map "\C-c\C-s" 'inf-clojure-show-var-source)
    (define-key map "\C-c\M-n" 'inf-clojure-set-ns)
    map))

;;;###autoload
(define-minor-mode inf-clojure-minor-mode
  "Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}"
  :lighter "" :keymap inf-clojure-minor-mode-map
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               'inf-clojure-completion-at-point))

(defcustom inf-clojure-program "lein repl"
  "Program name for invoking an inferior Clojure in Inferior Clojure mode."
  :type 'string
  :group 'inf-clojure)

(defcustom inf-clojure-load-command "(clojure.core/load-file \"%s\")\n"
  "Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Clojure expression that will command the inferior Clojure
to load that file.~"
  :type 'string
  :group 'inf-clojure)

(defcustom inf-clojure-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the Inferior Clojure mode."
  :type 'regexp
  :group 'inf-clojure)

(defvar inf-clojure-buffer nil "The current inf-clojure process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Clojure processes, you start the first up
with \\[inf-clojure].  It will be in a buffer named `*inf-clojure*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inf-clojure].  It will be in a new buffer,
named `*inf-clojure*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Clojure processes --
like `inf-clojure-eval-defun' or `inf-clojure-show-arglist' -- have to choose a process
to send to, when you have more than one Clojure process around.  This
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

(defvar inf-clojure-mode-hook '()
  "Hook for customizing Inferior Clojure mode.")

(put 'inf-clojure-mode 'mode-class 'special)

(define-derived-mode inf-clojure-mode comint-mode "Inferior Clojure"
  "Major mode for interacting with an inferior Clojure process.
Runs a Clojure interpreter as a subprocess of Emacs, with Clojure I/O through an
Emacs buffer.  Variable `inf-clojure-program' controls which Clojure interpreter
is run.  Variables `inf-clojure-prompt', `inf-clojure-filter-regexp' and
`inf-clojure-load-command' can customize this mode for different Clojure
interpreters.

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
  (setq comint-prompt-regexp inf-clojure-prompt)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (setq comint-get-old-input (function inf-clojure-get-old-input))
  (setq comint-input-filter (function inf-clojure-input-filter))
  (set (make-local-variable 'comint-prompt-read-only) inf-clojure-prompt-read-only)
  (add-hook 'comint-preoutput-filter-functions 'inf-clojure-preoutput-filter nil t)
  (add-hook 'completion-at-point-functions 'inf-clojure-completion-at-point nil t))

(defun inf-clojure-get-old-input ()
  "Return a string containing the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-clojure-input-filter (str)
  "t if STR does not match `inf-clojure-filter-regexp'."
  (not (string-match inf-clojure-filter-regexp str)))

(defun inf-clojure-chomp (string)
  (if (string-match "[\n]\\'" string)
      (replace-match "" t t string)
    string))

(defun inf-clojure-preoutput-filter (str)
  "Preprocess the output STR from interactive commands."
  (cond
   ((string-prefix-p "inf-clojure-" (symbol-name (or this-command last-command)))
    ;; prepend a newline to the output string
    (inf-clojure-chomp (concat "\n" str)))
   (t str)))

;;;###autoload
(defun inf-clojure (cmd)
  "Run an inferior Clojure process, input and output via buffer `*inf-clojure*'.
If there is a process already running in `*inf-clojure*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inf-clojure-program').  Runs the hooks from
`inf-clojure-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run Clojure: " inf-clojure-program)
                       inf-clojure-program)))
  (if (not (comint-check-proc "*inf-clojure*"))
      (let ((cmdlist (split-string cmd)))
        (set-buffer (apply (function make-comint)
                           "inf-clojure" (car cmdlist) nil (cdr cmdlist)))
        (inf-clojure-mode)))
  (setq inf-clojure-buffer "*inf-clojure*")
  (pop-to-buffer-same-window "*inf-clojure*"))

;;;###autoload
(defalias 'run-clojure 'inf-clojure)

(defun inf-clojure-eval-paragraph (&optional and-go)
  "Send the current paragraph to the inferior Clojure process.
Prefix argument means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (mark-paragraph)
    (inf-clojure-eval-region (point) (mark) and-go)))

(defun inf-clojure-eval-region (start end &optional and-go)
  "Send the current region to the inferior Clojure process.
Prefix argument means switch to the Clojure buffer afterwards."
  (interactive "r\nP")
  (comint-send-region (inf-clojure-proc) start end)
  (comint-send-string (inf-clojure-proc) "\n")
  (if and-go (inf-clojure-switch-to-repl t)))

(defun inf-clojure-eval-string (string)
  "Send the string to the inferior Clojure process to be executed."
  (comint-send-string (inf-clojure-proc) (concat string "\n")))

(defun inf-clojure-eval-defun (&optional and-go)
  "Send the current defun to the inferior Clojure process.
Prefix argument means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (skip-chars-backward " \t\n\r\f") ;  Makes allegro happy
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (inf-clojure-eval-region (point) end)))
  (if and-go (inf-clojure-switch-to-repl t)))

(defun inf-clojure-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the inferior Clojure process.
Prefix argument means switch to the Clojure buffer afterwards."
  (interactive "P")
  (inf-clojure-eval-region (save-excursion (backward-sexp) (point)) (point) and-go))

(defun inf-clojure-eval-form-and-next ()
  "Send the previous sexp to the inferior Clojure process and move to the next one."
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (inf-clojure-eval-last-sexp)
  (forward-sexp))

(defun inf-clojure-switch-to-repl (eob-p)
  "Switch to the inferior Clojure process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-clojure-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-clojure-buffer t))))
        (pop-to-buffer inf-clojure-buffer))
    (run-clojure inf-clojure-program))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))


;;; Now that inf-clojure-eval-/defun/region takes an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes, for backwards functionality, and because it's
;;; easier to type C-c e than C-u C-c C-e.

(defun inf-clojure-eval-region-and-go (start end)
  "Send the current region to the inferior Clojure, and switch to its buffer."
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
  :type '(repeat symbol)
  :group 'inf-clojure)

(defun inf-clojure-load-file (file-name)
  "Load a Clojure file into the inferior Clojure process."
  (interactive (comint-get-source "Load Clojure file: " inf-clojure-prev-l/c-dir/file
                                  inf-clojure-source-modes nil)) ; nil because LOAD
                                        ; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq inf-clojure-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                        (file-name-nondirectory file-name)))
  (comint-send-string (inf-clojure-proc)
                      (format inf-clojure-load-command file-name))
  (inf-clojure-switch-to-repl t))


;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command strings
;;; ===============

(defvar inf-clojure-var-doc-command
  "(clojure.repl/doc %s)\n"
  "Command to query inferior Clojure for a var's documentation.")

(defvar inf-clojure-var-source-command
  "(clojure.repl/source %s)\n"
  "Command to query inferior Clojure for a var's source.")

(defvar inf-clojure-arglist-command
  "(:arglists (clojure.core/meta #'%s))\n"
  "Command to query inferior Clojure for a function's arglist.")

(defvar inf-clojure-completion-command
  "(complete.core/completions \"%s\")\n"
  "Command to query inferior Clojure for completion candidates.")

(defvar inf-clojure-ns-vars-command
  "(clojure.repl/dir %s)\n"
  "Command to show the public vars in a namespace.")

(defvar inf-clojure-set-ns-command
  "(clojure.core/in-ns '%s)\n"
  "Command to set the namespace of the inferior Clojure process.")

(defvar inf-clojure-apropos-command
  "(doseq [var (sort (clojure.repl/apropos \"%s\"))]
     (println (str var)))\n"
  "Command to invoke apropos.")

(defvar inf-clojure-macroexpand-command
  "(clojure.core/macroexpand '%s)\n")

(defvar inf-clojure-macroexpand-1-command
  "(clojure.core/macroexpand-1 '%s)\n")

;;; Ancillary functions
;;; ===================

;;; Reads a string from the user.
(defun inf-clojure-symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))


;;; Adapted from function-called-at-point in help.el.
(defun inf-clojure-fn-called-at-pt ()
  "Returns the name of the function called in the current call.
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


;;; Adapted from variable-at-point in help.el.
(defun inf-clojure-var-at-pt ()
  (condition-case ()
      (save-excursion
        (forward-sexp -1)
        (skip-chars-forward "'")
        (let ((obj (read (current-buffer))))
          (and (symbolp obj) obj)))
    (error nil)))


;;; Documentation functions: var doc and arglist.
;;; ======================================================================

(defun inf-clojure-show-var-documentation (var)
  "Send a command to the inferior Clojure to give documentation for VAR.
See variable `inf-clojure-var-doc-command'."
  (interactive (inf-clojure-symprompt "Var doc" (inf-clojure-var-at-pt)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-var-doc-command var)))

(defun inf-clojure-show-var-source (var)
  "Send a command to the inferior Clojure to give source for VAR.
See variable `inf-clojure-var-source-command'."
  (interactive (inf-clojure-symprompt "Var source" (inf-clojure-var-at-pt)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-var-source-command var)))

(defun inf-clojure-show-arglist (fn)
  "Send a query to the inferior Clojure for the arglist for function FN.
See variable `inf-clojure-arglist-command'."
  (interactive (inf-clojure-symprompt "Arglist" (inf-clojure-fn-called-at-pt)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-arglist-command fn)))

(defun inf-clojure-show-ns-vars (ns)
  "Send a query to the inferior Clojure for the public vars in NS.
See variable `inf-clojure-ns-vars-command'."
  (interactive (inf-clojure-symprompt "Ns vars" (clojure-find-ns)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-ns-vars-command ns)))

(defun inf-clojure-set-ns (ns)
  "Set the ns of the inferior Clojure process to NS.
Defaults to the ns of the current buffer."
  (interactive (inf-clojure-symprompt "Set ns to" (clojure-find-ns)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-set-ns-command ns)))

(defun inf-clojure-apropos (var)
  "Send a command to the inferior Clojure to give apropos for VAR.
See variable `inf-clojure-apropos-command'."
  (interactive (inf-clojure-symprompt "Var apropos" (inf-clojure-var-at-pt)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-apropos-command var)))

(defun inf-clojure-macroexpand (&optional macro-1)
  "Send a command to the inferior Clojure to give apropos for VAR.
See variable `inf-clojure-macroexpand-command'.
With a prefix arg MACRO-1 uses `inf-clojure-macroexpand-1-command'."
  (interactive "P")
  (let ((last-sexp (buffer-substring-no-properties (save-excursion (backward-sexp) (point)) (point))))
    (comint-send-string
     (inf-clojure-proc)
     (format (if macro-1
                 inf-clojure-macroexpand-1-command
               inf-clojure-macroexpand-command)
             last-sexp))))


;;  "Returns the current inferior Clojure process.
;; See variable `inf-clojure-buffer'."
(defun inf-clojure-proc ()
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-clojure-mode)
                                      (current-buffer)
                                    inf-clojure-buffer))))
    (or proc
        (error "No Clojure subprocess; see variable `inf-clojure-buffer'"))))

(defun inf-clojure-completions (expr)
  "Return a list of completions for the Clojure expression starting with EXPR."
  (let* ((proc (inf-clojure-proc))
         (line (buffer-substring (save-excursion (move-beginning-of-line 1)
                                                 (point))
                                 (point)))
         (comint-filt (process-filter proc))
         (kept "")
         completions)
    (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
    (unwind-protect
        (let ((completion-snippet
               (format
                inf-clojure-completion-command (substring-no-properties expr))))
          (process-send-string proc completion-snippet)
          (while (and (not (string-match inf-clojure-prompt kept))
                      (accept-process-output proc 2)))
          (setq completions (read kept))
          ;; Subprocess echoes output on Windows and OS X.
          (when (and completions (string= (concat (car completions) "\n") completion-snippet))
            (setq completions (cdr completions))))
      (set-process-filter proc comint-filt))
    completions))

(defconst inf-clojure-clojure-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun inf-clojure-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward (concat "^" inf-clojure-clojure-expr-break-chars))
        (cons (point) end)))))

(defun inf-clojure-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (inf-clojure-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring (car bounds) (cdr bounds)))))

(defun inf-clojure-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (let ((bounds (inf-clojure-completion-bounds-of-expr-at-point)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (if (fboundp 'completion-table-with-cache)
                (completion-table-with-cache #'inf-clojure-completions)
              (completion-table-dynamic #'inf-clojure-completions))))))

(provide 'inf-clojure)

;;; inf-clojure.el ends here
