;;; inf-clojure.el --- Run an external Clojure process in an Emacs buffer -*- lexical-binding: t; -*-

;; Copyright © 2014-2017 Bozhidar Batsov

;; Authors: Bozhidar Batsov <bozhidar@batsov.com>
;;       Olin Shivers <shivers@cs.cmu.edu>
;; URL: http://github.com/clojure-emacs/inf-clojure
;; Keywords: processes, clojure
;; Version: 2.0.0-snapshot
;; Package-Requires: ((emacs "24.4") (clojure-mode "5.6"))

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
;; inferior-lisp adapted for Clojure.
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


(defgroup inf-clojure nil
  "Run an external Clojure process (REPL) in an Emacs buffer."
  :prefix "inf-clojure-"
  :group 'clojure
  :link '(url-link :tag "GitHub" "https://github.com/clojure-emacs/inf-clojure")
  :link '(emacs-commentary-link :tag "Commentary" "inf-clojure"))

(defconst inf-clojure-version "2.0.0-snapshot"
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

(defvar inf-clojure-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-x\C-e" #'inf-clojure-eval-last-sexp)
    (define-key map "\C-c\C-l" #'inf-clojure-load-file)
    (define-key map "\C-c\C-a" #'inf-clojure-show-arglist)
    (define-key map "\C-c\C-v" #'inf-clojure-show-var-documentation)
    (define-key map "\C-c\C-s" #'inf-clojure-show-var-source)
    (define-key map "\C-c\M-o" #'inf-clojure-clear-repl-buffer)
    (define-key map "\C-c\C-q" #'inf-clojure-quit)
    (easy-menu-define inf-clojure-mode-menu map
      "Inferior Clojure REPL Menu"
      '("Inf-Clojure REPL"
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        "--"
        ["Load file" inf-clojure-load-file t]
        "--"
        ["Show arglist" inf-clojure-show-arglist t]
        ["Show documentation for var" inf-clojure-show-var-documentation t]
        ["Show source for var" inf-clojure-show-var-source t]
        "--"
        ["Clear REPL" inf-clojure-clear-repl-buffer]
        ["Restart" inf-clojure-restart]
        ["Quit" inf-clojure-quit]
        "--"
        ["Version" inf-clojure-display-version]))
    map))

(defvar inf-clojure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-x"  #'inf-clojure-eval-defun)     ; Gnu convention
    (define-key map "\C-x\C-e" #'inf-clojure-eval-last-sexp) ; Gnu convention
    (define-key map "\C-c\C-e" #'inf-clojure-eval-last-sexp)
    (define-key map "\C-c\C-c" #'inf-clojure-eval-defun)     ; SLIME/CIDER style
    (define-key map "\C-c\C-b" #'inf-clojure-eval-buffer)
    (define-key map "\C-c\C-r" #'inf-clojure-eval-region)
    (define-key map "\C-c\C-n" #'inf-clojure-eval-form-and-next)
    (define-key map "\C-c\C-z" #'inf-clojure-switch-to-repl)
    (define-key map "\C-c\C-i" #'inf-clojure-show-ns-vars)
    (define-key map "\C-c\C-A" #'inf-clojure-apropos)
    (define-key map "\C-c\C-m" #'inf-clojure-macroexpand)
    (define-key map "\C-c\C-l" #'inf-clojure-load-file)
    (define-key map "\C-c\C-a" #'inf-clojure-show-arglist)
    (define-key map "\C-c\C-v" #'inf-clojure-show-var-documentation)
    (define-key map "\C-c\C-s" #'inf-clojure-show-var-source)
    (define-key map "\C-c\M-n" #'inf-clojure-set-ns)
    (define-key map "\C-c\C-q" #'inf-clojure-quit)
    (easy-menu-define inf-clojure-minor-mode-menu map
      "Inferior Clojure Minor Mode Menu"
      '("Inf-Clojure"
        ["Eval top-level sexp at point" inf-clojure-eval-defun t]
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        ["Eval region" inf-clojure-eval-region t]
        ["Eval buffer" inf-clojure-eval-buffer t]
        "--"
        ["Load file..." inf-clojure-load-file t]
        "--"
        ["Switch to REPL" inf-clojure-switch-to-repl t]
        ["Set REPL ns" inf-clojure-set-ns t]
        "--"
        ["Show arglist" inf-clojure-show-arglist t]
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
(define-minor-mode inf-clojure-minor-mode
  "Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}"
  :lighter "" :keymap inf-clojure-minor-mode-map
  (setq comint-input-sender 'inf-clojure--send-string)
  (inf-clojure-eldoc-setup)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'inf-clojure-completion-at-point))

(defcustom inf-clojure-lein-cmd "lein repl"
  "The command used to start a Clojure REPL for Leiningen projects.

Alternatively you can specify a TCP connection cons pair, instead
of command, consisting of a host and port
number (e.g. (\"localhost\" . 5555)).  That's useful if you're
often connecting to a remote REPL process."
  :type '(choice (string)
                 (cons string integer))
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-program 'inf-clojure-lein-cmd "2.0.0")

(defcustom inf-clojure-boot-cmd "boot repl"
  "The command used to start a Clojure REPL for Boot projects.

Alternatively you can specify a TCP connection cons pair, instead
of command, consisting of a host and port
number (e.g. (\"localhost\" . 5555)).  That's useful if you're
often connecting to a remote REPL process."
  :type '(choice (string)
                 (cons string integer))
  :package-version '(inf-clojure . "2.0.0"))

(defcustom inf-clojure-generic-cmd "lein repl"
  "The command used to start a Clojure REPL outside Lein/Boot projects.

Alternatively you can specify a TCP connection cons pair, instead
of command, consisting of a host and port
number (e.g. (\"localhost\" . 5555)).  That's useful if you're
often connecting to a remote REPL process."
  :type '(choice (string)
                 (cons string integer))
  :package-version '(inf-clojure . "2.0.0"))

(defvar-local inf-clojure-repl-type nil
  "Symbol to define your REPL type.
Its root binding is nil and it can be further customized using
either `setq-local` or an entry in `.dir-locals.el`." )

(defvar inf-clojure--repl-type-lock nil
  "Global lock for protecting against proc filter race conditions.
See http://blog.jorgenschaefer.de/2014/05/race-conditions-in-emacs-process-filter.html")

(defun inf-clojure--detect-repl-type (proc)
  "Identifies the current REPL type for PROC."
  (when (not inf-clojure--repl-type-lock)
    (let ((inf-clojure--repl-type-lock t))
      (cond
       ((inf-clojure--lumo-p proc) 'lumo)
       (t 'clojure)))))

(defun inf-clojure--set-repl-type (proc)
  "Set the REPL type if has not already been set.
It requires a REPL PROC for inspecting the correct type."
  (if (not inf-clojure-repl-type)
      (setq inf-clojure-repl-type (inf-clojure--detect-repl-type proc))
    inf-clojure-repl-type))

(defun inf-clojure--send-string (proc string)
  "A custom `comint-input-sender` / `comint-send-string`.
Perform the required side effects on every send for PROC and
STRING (for example set the buffer local REPL type).  It should
be used instead of `comint-send-string`."
  (inf-clojure--set-repl-type proc)
  (comint-simple-send proc string))

(defcustom inf-clojure-load-form "(clojure.core/load-file \"%s\")\n"
  "Format-string for building a Clojure expression to load a file.
This format string should use `%s' to substitute a file name and
should result in a Clojure form that will be sent to the inferior
Clojure to load that file."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-load-command 'inf-clojure-load-form "2.0.0")

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
  :package-version '(inf-clojure . "2.0.0"))

(defvar inf-clojure-buffer nil
  "The current inf-clojure process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
To run multiple Clojure processes, you start the first up
with \\[inf-clojure].  It will be in a buffer named `*inf-clojure*'.
Rename this buffer with \\[rename-buffer].  You may now start up a new
process with another \\[inf-clojure].  It will be in a new buffer,
named `*inf-clojure*'.  You can switch between the different process
buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Clojure processes --
like `inf-clojure-eval-defun' or `inf-clojure-show-arglist' -- have to choose a
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
  (inf-clojure-eldoc-setup)
  (setq comint-get-old-input #'inf-clojure-get-old-input)
  (setq comint-input-filter #'inf-clojure-input-filter)
  (setq-local comint-prompt-read-only inf-clojure-prompt-read-only)
  (add-hook 'comint-preoutput-filter-functions #'inf-clojure-preoutput-filter nil t)
  (add-hook 'comint-output-filter-functions 'inf-clojure--ansi-filter)
  (add-hook 'completion-at-point-functions #'inf-clojure-completion-at-point nil t)
  (ansi-color-for-comint-mode-on))

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

(defconst inf-clojure--ansi-clear-line "\\[1G\\|\\[0J\\|\\[13G"
  "Ansi codes sent by the lumo repl that we need to clear." )

(defun inf-clojure--ansi-filter (string)
  "Filter unwanted ansi character from STRING."
  (save-excursion
    ;; go to start of first line just inserted
    (comint-goto-process-mark)
    (goto-char (max (point-min) (- (point) (string-width string))))
    (forward-line 0)
    (while (re-search-forward inf-clojure--ansi-clear-line nil t)
      (replace-match ""))))

(defun inf-clojure-preoutput-filter (str)
  "Preprocess the output STR from interactive commands."
  (cond
   ((string-prefix-p "inf-clojure-" (symbol-name (or this-command last-command)))
    ;; Remove subprompts and prepend a newline to the output string
    (inf-clojure-chomp (concat "\n" (inf-clojure-remove-subprompts str))))
   (t str)))

(defvar inf-clojure-project-root-files
  '("project.clj" "build.boot")
  "A list of files that can be considered project markers.")

(defun inf-clojure-project-root ()
  "Retrieve the root directory of a project if available.

Fallback to `default-directory.' if not within a project."
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           inf-clojure-project-root-files)))
      default-directory))

(defun inf-clojure-project-type ()
  "Determine the type, either leiningen or boot of the current project."
  (let ((default-directory (inf-clojure-project-root)))
    (cond ((file-exists-p "project.clj") "lein")
          ((file-exists-p "build.boot") "boot")
          (t nil))))

(defun inf-clojure-cmd (project-type)
  "Determine the command `inf-clojure' needs to invoke for the PROJECT-TYPE."
  (pcase project-type
    ("lein" inf-clojure-lein-cmd)
    ("boot" inf-clojure-boot-cmd)
    (_ inf-clojure-generic-cmd)))

(defun inf-clojure-clear-repl-buffer ()
  "Clear the REPL buffer."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;;;###autoload
(defun inf-clojure (cmd)
  "Run an inferior Clojure process, input and output via buffer `*inf-clojure*'.
If there is a process already running in `*inf-clojure*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inf-clojure-*-cmd').  Runs the hooks from
`inf-clojure-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
                         (read-string "Run Clojure: " (inf-clojure-cmd (inf-clojure-project-type)))
                       (inf-clojure-cmd (inf-clojure-project-type)))))
  (if (not (comint-check-proc "*inf-clojure*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory (inf-clojure-project-root))
            (cmdlist (if (consp cmd)
                         (list cmd)
                       (split-string cmd))))
        (message "Starting Clojure REPL via `%s'..." cmd)
        (set-buffer (apply #'make-comint
                           "inf-clojure" (car cmdlist) nil (cdr cmdlist)))
        (inf-clojure-mode)))
  (setq inf-clojure-buffer "*inf-clojure*")
  (if inf-clojure-repl-use-same-window
      (pop-to-buffer-same-window "*inf-clojure*")
    (pop-to-buffer "*inf-clojure*")))

(defun inf-clojure-eval-region (start end &optional and-go)
  "Send the current region to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "r\nP")
  ;; replace multiple newlines at the end of the region by a single one
  ;; or add one if there was no newline
  (let ((str (replace-regexp-in-string
              "[\n]*\\'" "\n"
              (buffer-substring-no-properties start end))))
    (inf-clojure--send-string (inf-clojure-proc) str))
  (if and-go (inf-clojure-switch-to-repl t)))

(defun inf-clojure-eval-string (code)
  "Send the string CODE to the inferior Clojure process to be executed."
  (inf-clojure--send-string (inf-clojure-proc) (concat code "\n")))

(defun inf-clojure-eval-defun (&optional and-go)
  "Send the current defun to the inferior Clojure process.
Prefix argument AND-GO means switch to the Clojure buffer afterwards."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (inf-clojure-eval-region (point) end and-go))))

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

(defun inf-clojure-switch-to-repl (eob-p)
  "Switch to the inferior Clojure process buffer.
With prefix argument EOB-P, positions cursor at end of buffer."
  (interactive "P")
  (if (get-buffer-process inf-clojure-buffer)
      (let ((pop-up-frames
             ;; Be willing to use another frame
             ;; that already has the window in it.
             (or pop-up-frames
                 (get-buffer-window inf-clojure-buffer t))))
        (pop-to-buffer inf-clojure-buffer))
    (inf-clojure (inf-clojure-cmd (inf-clojure-project-type))))
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
  :type '(repeat symbol))

(defun inf-clojure-load-file (&optional switch-to-repl file-name)
  "Load a Clojure file FILE-NAME into the inferior Clojure process.

The prefix argument SWITCH-TO-REPL controls whether to switch to REPL after the file is loaded or not."
  (interactive "P")
  (let ((file-name (or file-name
                       (car (comint-get-source "Load Clojure file: " inf-clojure-prev-l/c-dir/file
                                               ;; nil because doesn't need an exact name
                                               inf-clojure-source-modes nil)))))
    (comint-check-source file-name) ; Check to see if buffer needs saved.
    (setq inf-clojure-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                              (file-name-nondirectory file-name)))
    (inf-clojure--send-string (inf-clojure-proc)
                              (format inf-clojure-load-form file-name))
    (when switch-to-repl
      (inf-clojure-switch-to-repl t))))

(defun inf-clojure-connected-p ()
  "Return t if inferior Clojure is currently connected, nil otherwise."
  (not (null inf-clojure-buffer)))


;;; Documentation functions: function doc, var doc, arglist, and
;;; describe symbol.
;;; ===========================================================================

;;; Command forms
;;; =============

(defcustom inf-clojure-var-doc-form
  "(clojure.repl/doc %s)\n"
  "Form to query inferior Clojure for a var's documentation."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-var-doc-command 'inf-clojure-var-doc-form "2.0.0")

(defcustom inf-clojure-var-doc-form-lumo
  "(lumo.repl/doc %s)\n"
  "Lumo form to query inferior Clojure for a var's documentation."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(defun inf-clojure-var-doc-form ()
  "Return the form to query inferior Clojure for a var's documentation.
If you are using REPL types, it will pickup the most approapriate
`inf-clojure-var-doc-form` variant."
  (pcase (inf-clojure--set-repl-type (inf-clojure-proc))
    (`lumo inf-clojure-var-doc-form-lumo)
    (_ inf-clojure-var-doc-form)))

(defcustom inf-clojure-var-source-form
  "(clojure.repl/source %s)\n"
  "Form to query inferior Clojure for a var's source."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-var-source-command 'inf-clojure-var-source-form "2.0.0")

(defcustom inf-clojure-arglists-form
  "(try
     (:arglists
      (clojure.core/meta
       (clojure.core/resolve
        (clojure.core/read-string \"%s\"))))
     (catch Throwable t nil))\n"
  "Form to query inferior Clojure for a function's arglist."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-arglist-command 'inf-clojure-arglists-form "2.0.0")

(defcustom inf-clojure-arglists-form-lumo
  "(lumo.repl/get-arglists \"%s\")"
  "Lumo form to query inferior Clojure for a function's arglist."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(defun inf-clojure-arglists-form ()
  "Return the form to query inferior Clojure for arglists of a var.
If you are using REPL types, it will pickup the most approapriate
`inf-clojure-arglist-form` variant."
  (pcase (inf-clojure--set-repl-type (inf-clojure-proc))
    (`lumo inf-clojure-arglists-form-lumo)
    (_ inf-clojure-arglists-form)))

(defcustom inf-clojure-completion-form
  "(complete.core/completions \"%s\")\n"
  "Form to query inferior Clojure for completion candidates."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-completion-command 'inf-clojure-completion-form "2.0.0")

(defcustom inf-clojure-completion-form-lumo
  "(doall (map str (lumo.repl/get-completions \"%s\")))\n"
  "Lumo form to query inferior Clojure for completion candidates."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(defun inf-clojure-completion-form ()
  "Return the form to query inferior Clojure for a var's documentation.
If you are using REPL types, it will pickup the most approapriate
`inf-clojure-completion-form` variant."
  (pcase (inf-clojure--set-repl-type (inf-clojure-proc))
    (`lumo inf-clojure-completion-form-lumo)
    (_ inf-clojure-completion-form)))

(defcustom inf-clojure-ns-vars-form
  "(clojure.repl/dir %s)\n"
  "Form to show the public vars in a namespace."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(defcustom inf-clojure-ns-vars-form-lumo
  "(lumo.repl/dir %s)\n"
  "Lumo form to show the public vars in a namespace."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(defun inf-clojure-ns-vars-form ()
  "Return the form to query inferior Clojure for public vars in a namespace.
If you are using REPL types, it will pickup the most approapriate
`inf-clojure-ns-vars-form` variant."
  (pcase (inf-clojure--set-repl-type (inf-clojure-proc))
    (`lumo inf-clojure-ns-vars-form-lumo)
    (_ inf-clojure-ns-vars-form)))

(define-obsolete-variable-alias 'inf-clojure-ns-vars-command 'inf-clojure-ns-vars-form "2.0.0")

(defcustom inf-clojure-set-ns-form
  "(clojure.core/in-ns '%s)\n"
  "Form to set the namespace of the inferior Clojure process."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-set-ns-command 'inf-clojure-set-ns-form "2.0.0")

(defcustom inf-clojure-apropos-form
  "(doseq [var (sort (clojure.repl/apropos \"%s\"))]
     (println (str var)))\n"
  "Form to invoke apropos."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-apropos-command 'inf-clojure-apropos-form "2.0.0")

(defcustom inf-clojure-macroexpand-form
  "(clojure.core/macroexpand '%s)\n"
  "Form to invoke macroexpand."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-macroexpand-command 'inf-clojure-macroexpand-form "2.0.0")

(defcustom inf-clojure-macroexpand-1-form
  "(clojure.core/macroexpand-1 '%s)\n"
  "Form to invoke macroexpand-1."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(define-obsolete-variable-alias 'inf-clojure-macroexpand-1-command 'inf-clojure-macroexpand-1-form "2.0.0")

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

;;; Documentation functions: var doc and arglist.
;;; ======================================================================

(defun inf-clojure-show-var-documentation (prompt-for-symbol)
  "Send a form to the inferior Clojure to give documentation for VAR.
See function `inf-clojure-var-doc-form'.  When invoked with a
prefix argument PROMPT-FOR-SYMBOL, it prompts for a symbol name."
  (interactive "P")
  (let ((var (if prompt-for-symbol
                 (car (inf-clojure-symprompt "Var doc" (inf-clojure-symbol-at-point)))
               (inf-clojure-symbol-at-point))))
    (comint-proc-query (inf-clojure-proc) (format (inf-clojure-var-doc-form) var))))

(defun inf-clojure-show-var-source (prompt-for-symbol)
  "Send a command to the inferior Clojure to give source for VAR.
See variable `inf-clojure-var-source-form'.  When invoked with a
prefix argument PROMPT-FOR-SYMBOL, it prompts for a symbol name."
  (interactive "P")
  (let ((var (if prompt-for-symbol
                 (car (inf-clojure-symprompt "Var source" (inf-clojure-symbol-at-point)))
               (inf-clojure-symbol-at-point))))
    (comint-proc-query (inf-clojure-proc) (format inf-clojure-var-source-form var))))

(defun inf-clojure-match-arglists (input-form string)
  "Return the arglists match from INPUT-FORM and STRING.
The output depends on the correct REPL type.  We assume the
`inf-clojure-repl-type` var is already set, therefore this is
safe to call only from inside `inf-clojure-arglist`."
  (pcase inf-clojure-repl-type
    (`lumo (let ((input-end (and (string-match input-form string) (match-end 0))))
             (and (string-match "(.+)" string input-end) (match-string 0 string))))
    (_ (and (string-match "(.+)" string) (match-string 0 string)))))

(defun inf-clojure-arglist (fn)
  "Send a query to the inferior Clojure for the arglist for function FN.
See variable `inf-clojure-arglist-form'."
  (let* ((proc (inf-clojure-proc))
         (comint-filt (process-filter proc))
         (kept "")
         eldoc)
    (set-process-filter proc (lambda (_proc string) (setq kept (concat kept string))))
    (unwind-protect
        (let ((eldoc-snippet (format (inf-clojure-arglists-form) fn)))
          (inf-clojure--send-string proc eldoc-snippet)
          (while (and (not (string-match inf-clojure-prompt kept))
                      (accept-process-output proc 2)))
          (setq eldoc (inf-clojure-match-arglists eldoc-snippet kept)))
      (set-process-filter proc comint-filt))
    eldoc))

(defun inf-clojure-show-arglist (prompt-for-symbol)
  "Show the arglist for function FN in the mini-buffer.
See variable `inf-clojure-arglist-form'.  When invoked with a
prefix argument PROMPT-FOR-SYMBOL, it prompts for a symbol name."
  (interactive "P")
  (let* ((fn (if prompt-for-symbol
                 (car (inf-clojure-symprompt "Arglist" (inf-clojure-fn-called-at-pt)))
               (inf-clojure-fn-called-at-pt)))
         (eldoc (inf-clojure-arglist fn)))
    (when eldoc
      (message "%s: %s" fn eldoc))))

(defun inf-clojure-show-ns-vars (prompt-for-ns)
  "Send a query to the inferior Clojure for the public vars in NS.
See variable `inf-clojure-ns-vars-form'.  When invoked with a
prefix argument PROMPT-FOR-NS, it prompts for a namespace name."
  (interactive "P")
  (let ((ns (if prompt-for-ns
                (car (inf-clojure-symprompt "Ns vars" (clojure-find-ns)))
              (clojure-find-ns))))
    (comint-proc-query (inf-clojure-proc) (format (inf-clojure-ns-vars-form) ns))))

(defun inf-clojure-set-ns (prompt-for-ns)
  "Set the ns of the inferior Clojure process to NS.
See variable `inf-clojure-set-ns-form`.  It defaults to the ns of
the current buffer.  When invoked with a prefix argument
PROMPT-FOR-NS, it prompts for a namespace name."
  (interactive "P")
  (let ((ns (if prompt-for-ns
                (car (inf-clojure-symprompt "Set ns to" (clojure-find-ns)))
              (clojure-find-ns))))
    (when (or (not ns) (equal ns ""))
      (user-error "No namespace selected"))
    (comint-proc-query (inf-clojure-proc) (format inf-clojure-set-ns-form ns))))

(defun inf-clojure-apropos (var)
  "Send a form to the inferior Clojure to give apropos for VAR.
See variable `inf-clojure-apropos-form'."
  (interactive (inf-clojure-symprompt "Var apropos" (inf-clojure-symbol-at-point)))
  (comint-proc-query (inf-clojure-proc) (format inf-clojure-apropos-form var)))

(defun inf-clojure-macroexpand (&optional macro-1)
  "Send a form to the inferior Clojure to give apropos for VAR.
See variable `inf-clojure-macroexpand-form'.
With a prefix arg MACRO-1 uses `inf-clojure-macroexpand-1-form'."
  (interactive "P")
  (let ((last-sexp (buffer-substring-no-properties (save-excursion (backward-sexp) (point)) (point))))
    (inf-clojure--send-string
     (inf-clojure-proc)
     (format (if macro-1
                 inf-clojure-macroexpand-1-form
               inf-clojure-macroexpand-form)
             last-sexp))))


(defun inf-clojure-proc ()
  "Return the current inferior Clojure process.
See variable `inf-clojure-buffer'."
  (let ((proc (get-buffer-process (if (derived-mode-p 'inf-clojure-mode)
                                      (current-buffer)
                                    inf-clojure-buffer))))
    (or proc
        (error "No Clojure subprocess; see variable `inf-clojure-buffer'"))))

(defun inf-clojure-completions (expr)
  "Return a list of completions for the Clojure expression starting with EXPR."
  (let* ((proc (inf-clojure-proc))
         (comint-filt (process-filter proc))
         (kept "")
         completions)
    (set-process-filter proc (lambda (_proc string) (setq kept (concat kept string))))
    (unwind-protect
        (let ((completion-snippet
               (format
                (inf-clojure-completion-form) (substring-no-properties expr))))
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

(defun inf-clojure-eldoc-arglist (thing)
  "Return the arglist for THING."
  (when (and thing
             (not (string= thing ""))
             (not (string-prefix-p ":" thing)))
    ;; check if we can used the cached eldoc info
    (if (string= thing (car inf-clojure-eldoc-last-symbol))
        (cdr inf-clojure-eldoc-last-symbol)
      (let ((arglist (inf-clojure-arglist (substring-no-properties thing))))
        (when arglist
          (setq inf-clojure-eldoc-last-symbol (cons thing arglist))
          arglist)))))

(defun inf-clojure-eldoc ()
  "Backend function for eldoc to show argument list in the echo area."
  (when (and (inf-clojure-connected-p)
             ;; don't clobber an error message in the minibuffer
             (not (member last-command '(next-error previous-error))))
    (let* ((info (inf-clojure-eldoc-info-in-current-sexp))
           (thing (car info))
           (value (inf-clojure-eldoc-arglist thing)))
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
  "Find or select an inf-clojure buffer to operate on.

Useful for commands that can invoked outside of an inf-clojure buffer
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
       (t (get-buffer (completing-read "Select target inf-clojure buffer: " (mapcar #'buffer-name repl-buffers))))))))

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
    (inf-clojure (inf-clojure-cmd (inf-clojure-project-type)))
    (rename-buffer target-buffer-name)))

(defun inf-clojure--response-match-p (form match-p proc)
  "Return MATCH-P on the result of sending FORM to PROC.
Note that this function will add a \n to the end of the string
for evaluation, therefore FORM should not include it."
  (let* ((kept "")
         (is-matching nil)
         (prev-filter (process-filter proc)))
    (set-process-filter proc (lambda (_ string) (setq kept (concat kept string))))
    (unwind-protect
        ;; use the real comind-send-string in order to avoid stack overflows
        (comint-send-string proc (concat form "\n"))
      ;; yey, loads of procedural code again
      (while (and (not is-matching)
                  (not (string-match inf-clojure-prompt kept))
                  (accept-process-output proc 2))
        (setq is-matching (funcall match-p kept)))
      (set-process-filter proc prev-filter))
    is-matching))

;;;; Lumo
;;;; ====

(defcustom inf-clojure--lumo-repl-form
  "(js/global.hasOwnProperty \"$$LUMO_GLOBALS\")"
  "Form to invoke in order to verify that we launched a Lumo REPL."
  :type 'string
  :package-version '(inf-clojure . "2.0.0"))

(defalias 'inf-clojure--lumo-p
  (apply-partially 'inf-clojure--response-match-p
                   inf-clojure--lumo-repl-form
                   (lambda (string)
                     (string-match-p (concat inf-clojure--lumo-repl-form "\\Ca*true\\Ca*") string)))
  "Ascertain that PROC is a Lumo REPL.")

(provide 'inf-clojure)
;;; inf-clojure.el ends here
