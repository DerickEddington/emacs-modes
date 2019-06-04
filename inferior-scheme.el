;;; inferior-scheme.el --- Scheme process in a buffer. Adapted from cmuscheme.el

;; Copyright (C) 1988, 1994, 1997, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;;
;; Authors: Olin Shivers <olin.shivers@cs.cmu.edu>
;;          Derick Eddington <derick.eddington@gmail.com>
;; Keywords: processes, lisp
;;
;; This file was part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Modified by Derick Eddington, from the cmuscheme.el that came
;; with GNU Emacs 23.0.60.1.

;;; Commentary:
;;
;;  This is a customization of comint-mode (see comint.el).

;;; Noteable differences from cmuscheme.el:
;;
;; When `inferior-scheme-input-read-only' or `inferior-scheme-output-read-only'
;; are true (the defaults), old input and/or output text are read-only.
;;
;; `inferior-scheme-send-region' (old name: `scheme-send-region') inserts the
;; sent text, excluding any leading whitespace, in the inferior Scheme buffer
;; after the last prompt (as if it was typed in) and indents and fontifies the
;; text.
;; 
;; `inferior-scheme-send-top-level-form' (old name: `scheme-send-definition')
;; inserts the sent text same as `inferior-scheme-send-region' does.
;;
;; `inferior-scheme-send-preceding-sexp' (old name: `scheme-send-last-sexp')
;; inserts the sent text same as `inferior-scheme-send-region' does.  Also, it
;; sends the entire s-expression, not just the portion before point.
;;
;; There are no commands for compiling, tracing, expanding, or loading because
;; those things are Scheme-system-specific and this library is intended to be
;; used with any system.
;;
;; There is no support for inferior Scheme process "init files".  Things like
;; that should be accomplished outside this mode and outside Emacs, e.g., by
;; using an external wrapper script.

;;; To Do:
;;
;; Make sure all motion, matching, current buffer changes, etc. are inside
;; appropriate save-excursion, save-match-data, etc.
;;
;; Try to work-around-fix comint's bug with history wrap-around inserting copy
;; of prompt

(require 'comint)
(require 'lisp-mode)   ;; For `lisp-mode-shared-map'.
(require 'scheme)      ;; Derick's Scheme mode, not the old one.

(defgroup inferior-scheme nil
  "Run a Scheme process in a buffer."
  :group 'scheme)

(defcustom inferior-scheme-command-line "not-defined"
  "Shell command-line invoked by `inferior-scheme'."
  :type 'string
  :group 'inferior-scheme)

(defvar inferior-scheme-mode-map
  (let ((m (copy-keymap lisp-mode-shared-map)))  ;; It's sparse and small.
    (set-keymap-parent m comint-mode-map)
    (define-key m "\n" 'inferior-scheme-return)
    (define-key m "\r" 'inferior-scheme-return)  ;; Needed?
    (define-key m "\M-\C-x" 'inferior-scheme-send-top-level-form) ;GNU convention
    (define-key m "\C-x\C-e" 'inferior-scheme-send-preceding-sexp) ;GNU convention
    (define-key m "\C-c\C-t" 'inferior-scheme-send-top-level-form)
    (define-key m "\C-c\C-r" 'inferior-scheme-send-region)
    (define-key m "\C-c\C-p" 'inferior-scheme-send-preceding-sexp)
    m))

;; Install the process communication commands in the scheme-mode keymap.
;; The first two are GNU convention.
(define-key scheme-mode-map "\M-\C-x" 'inferior-scheme-send-top-level-form)
(define-key scheme-mode-map "\C-x\C-e" 'inferior-scheme-send-preceding-sexp)
(define-key scheme-mode-map "\C-c\C-t" 'inferior-scheme-send-top-level-form)
(define-key scheme-mode-map "\C-c\M-t" 'inferior-scheme-send-top-level-form-and-go)
(define-key scheme-mode-map "\C-c\C-r" 'inferior-scheme-send-region)
(define-key scheme-mode-map "\C-c\M-r" 'inferior-scheme-send-region-and-go)
(define-key scheme-mode-map "\C-c\C-p" 'inferior-scheme-send-preceding-sexp)
(define-key scheme-mode-map "\C-c\M-p" 'inferior-scheme-send-preceding-sexp-and-go)
(define-key scheme-mode-map "\C-c\C-z" 'inferior-scheme-switch-to-buffer)

(let ((m (lookup-key scheme-mode-map [menu-bar scheme])))
  (define-key m [separator-eval] '("--"))
  (define-key m [switch]
    '("Switch to Inferior Scheme" . inferior-scheme-switch-to-buffer))
  (define-key m [send-def-go]
    '("Send Top-Level Form & Go" . inferior-scheme-send-top-level-form-and-go))
  (define-key m [send-def]
    '("Send Top-Level Form" . inferior-scheme-send-top-level-form))
  (define-key m [send-region-go]
    '("Send Region & Go" . inferior-scheme-send-region-and-go))
  (put 'send-region-go 'menu-enable 'mark-active)  ;; Not working. Why?
  (define-key m [send-region]
    '("Send Region" . inferior-scheme-send-region))
  (put 'send-region 'menu-enable 'mark-active)  ;; Not working. Why?
  (define-key m [send-sexp-go]
    '("Send Preceding Form & Go" . inferior-scheme-send-preceding-sexp-and-go))
  (define-key m [send-sexp]
    '("Send Preceding Form" . inferior-scheme-send-preceding-sexp)))

(defvar inferior-scheme-buffer "*scheme*"
  "*The current inferior Scheme process buffer.

MULTIPLE PROCESS SUPPORT
=================================================================
inferior-scheme.el supports, in a fairly simple fashion, running
multiple Scheme processes.  To run multiple Scheme processes, you
start the first up with \\[inferior-scheme].  It will be in a
buffer named by the value of `inferior-scheme-buffer'.  Rename
this buffer with \\[rename-buffer].  From a different
non-Inferior Scheme buffer, you may start up a new process with
another \\[inferior-scheme].  It will be in a new buffer, named
by the value of `inferior-scheme-buffer'.  You can switch between
the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Scheme processes
-- like `inferior-scheme-send-top-level-form' -- have to choose a
process to send to, when you have more than one Scheme process
around.  This is determined by the variable
`inferior-scheme-buffer'.  Suppose you have three inferior
Schemes running:
    Buffer	Process
    foo		scheme
    bar		scheme<2>
    *scheme*    scheme<3>
If you do a \\[inferior-scheme-send-top-level-form]
command on some Scheme source code, what process do you send it
to?
- If you're in a process buffer (foo, bar, or *scheme*), you send
  it to that process.
- If you're in some other buffer (e.g., a source file), you send
  it to the process attached to buffer `inferior-scheme-buffer'.
This process selection is performed by function
`inferior-scheme-proc'.

If you run multiple processes, you can change
`inferior-scheme-buffer' to another process buffer with
\\[set-variable], including making it buffer-local to a
particular source file.")

(defun inferior-scheme-find-field (field start end)
  (let* ((os (text-property-any start end 'field field))
         (oe (and os
                  (next-single-property-change os 'field nil end))))
    (and os
         (cons os oe))))

(defconst inferior-scheme-input-face 'inferior-scheme-input-face)
(defface inferior-scheme-input-face
  '((t ()))
  "Face to prepend to process input text."
  :group 'inferior-scheme)

(defconst inferior-scheme-output-face 'inferior-scheme-output-face)
(defface inferior-scheme-output-face
  '((t ()))
  "Face to prepend to process output text."
  :group 'inferior-scheme)

(define-derived-mode inferior-scheme-mode comint-mode "Inferior-Scheme"
  "Major mode for interacting with an inferior Scheme process.

The following commands are available:

  RET after the end of new input sends the new input text to the
  process.

  RET inside new input calls `inferior-scheme-return-function',
  which by default is `newline-and-indent'.

  RET before the end of the last output inserts the s-expression
  before point as new input and sends it.

\\{inferior-scheme-mode-map}

A Scheme process can be fired up with: M-x inferior-scheme.

Customization: Entry to this mode runs the hooks on
`comint-mode-hook' and `inferior-scheme-mode-hook' (in that
order).

You can send text to the inferior Scheme process from other
buffers containing Scheme source.

  `inferior-scheme-switch-to-buffer' switches the current buffer
  to the Scheme process buffer.

  `inferior-scheme-send-preceding-sexp' sends the s-expression
  before point to the Scheme process.

  `inferior-scheme-send-top-level-form' sends the top-level form
  before point to the Scheme process.

  `inferior-scheme-send-region' sends the current region to the
  Scheme process.

  `inferior-scheme-send-top-level-form-and-go' and
  `inferior-scheme-send-region-and-go' and
  `inferior-scheme-send-preceding-sexp-and-go'
  switch to the Scheme process buffer after sending their text.

For information on running multiple processes in multiple
buffers, see documentation for variable `inferior-scheme-buffer'.

If you accidentally suspend your process, use
\\[comint-continue-subjob] to continue it."

  :syntax-table scheme-mode-syntax-table
  (scheme-mode-initialize)
  ;; Find output regions to prevent extra fontifying, such as keywords.
  (font-lock-add-keywords nil  ;; Add to beginning of font-lock-keywords list.
   `((,(function
        (lambda (limit)
          (while (< (point) limit)
            (let ((x (inferior-scheme-find-field 'output (point) limit)))
              (or (and x
                       (let ((os (car x)) (oe (cdr x)))
                         (put-text-property os oe 'scheme-no-fontify '(extra))
                         (goto-char oe)))
                  (goto-char limit))))
          nil))
      . ignored)))
  ;; Find input and output regions to prepend special faces to them.
  (font-lock-add-keywords nil
   `((,(function
        (lambda (limit)
          ;; Don't include newlines. Necessary so 'font-lock-multiline is not
          ;; added and to make it look nicer by not fontifying the whole line.
          (while (and (= (point) (line-end-position))
                      (< (point) (point-max)))
            (forward-line))
          (setq limit (min (line-end-position) limit))
          (let* ((p (min (point) limit))
                 (x (inferior-scheme-find-field 'output p limit)))
            (if x
                (let ((os (car x)) (oe (cdr x)))
                  (set-match-data (list p os))
                  (goto-char oe))
              (and (< p (point-max))
                   (progn (set-match-data (list p limit))
                          (goto-char limit)))))))
      0 inferior-scheme-input-face prepend)
     (,(function
        (lambda (limit)
          ;; Don't include newlines. Necessary so 'font-lock-multiline is not
          ;; added and to make it look nicer by not fontifying the whole line.
          (while (and (= (point) (line-end-position))
                      (< (point) (point-max)))
            (forward-line))
          (setq limit (min (line-end-position) limit))
          (let* ((p (min (point) limit))
                 (x (inferior-scheme-find-field 'output p limit)))
            (and x
                 (let ((os (car x)) (oe (cdr x)))
                   (set-match-data (list os oe))
                   (goto-char oe))))))
      0 inferior-scheme-output-face prepend))
   t)  ;; Add to end of font-lock-keywords list.
  (set (make-local-variable 'font-lock-extend-region-functions)
       (mapcar (function
                (lambda (func-name)
                  (if (eq func-name 'font-lock-extend-region-wholelines)
                      'inferior-scheme-extend-region-wholelines
                    func-name)))
               font-lock-extend-region-functions))
  (set (make-local-variable 'comint-prompt-regexp)
       inferior-scheme-prompt-regexp)
  (set (make-local-variable 'mode-line-process) '(":%s"))
  (set (make-local-variable 'comint-input-filter)
       (function inferior-scheme-input-filter))
  (set (make-local-variable 'comint-get-old-input)
       (function inferior-scheme-get-old-input)))

(defun inferior-scheme-extend-region-wholelines ()
  "Wrap calls to `font-lock-extend-region-wholelines' with
`inhibit-field-text-motion' set to true.  Doing so is necessary
to allow some Scheme lexeme fontifying regular expressions from
scheme.el to match when they immediately follow the prompt output
by the Scheme process because the process outputs are fields."
  (let ((inhibit-field-text-motion t))
    (font-lock-extend-region-wholelines)))

(defcustom inferior-scheme-prompt-regexp "^[^>\n]*>+ *"
  "Regular expression to match the inferior Scheme REPL's prompt,
so comint knows where it is.  The regular expression needs to
ensure it matches only at beginning of line."
  :type 'regexp
  :group 'inferior-scheme)

(defcustom inferior-scheme-filter-regexp "\\`\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of only whitespace."
  :type 'regexp
  :group 'inferior-scheme)

(defun inferior-scheme-input-filter (str)
  "Don't save anything matching `inferior-scheme-filter-regexp'."
  (not (string-match inferior-scheme-filter-regexp str)))

(defun inferior-scheme-get-preceding (f)
  (save-excursion
    (let* ((a (progn (funcall f) (point)))
           (oc (current-column))
           (b (progn (forward-sexp) (point)))
           (c (progn (backward-sexp) (point))))
      (and (= a c)
           (vector oc a b)))))

(defun inferior-scheme-get-old-input ()
  "Get the s-expression beginning before point.  All of the
s-expression is gotten.  Signal an error if point is not after
the beginning of an s-expression.  Used as the value of
`comint-get-old-input'."
  (let ((x (inferior-scheme-get-preceding 'backward-sexp)))
    (if x
        (buffer-substring-no-properties (elt x 1) (elt x 2))  ;; removes 'read-only
      (error "No preceding s-expression"))))

;;;###autoload
(defun inferior-scheme (cmd)
  "Run an inferior Scheme process with input and output via the
current buffer if it is in Inferior Scheme mode or the buffer
named by the value of `inferior-scheme-buffer'.  If there is a
process already running in that buffer, just switch to that
buffer without starting a new process.  CMD specifies the program
invocation command-line (default is value of
`inferior-scheme-command-line').

Runs the hook `inferior-scheme-mode-hook' \(after the
`comint-mode-hook' is run).

\(Type \\[describe-mode] in the process buffer for a list of
commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run Scheme: " inferior-scheme-command-line)
                       inferior-scheme-command-line)))
  (let ((isb (if (eq major-mode 'inferior-scheme-mode)
                 (buffer-name)
               inferior-scheme-buffer)))
    (unless (comint-check-proc isb)
      (let* ((cmdlist (split-string-and-unquote cmd))
             (buf (apply 'make-comint-in-buffer "scheme"
                         isb (car cmdlist) nil (cdr cmdlist))))
        ;; Set for current buffer.
        (set (make-local-variable 'inferior-scheme-command-line) cmd)
        ;; The next expression is necessary to be able to make output read-only.
        (set-process-filter (get-buffer-process buf) 'inferior-scheme-output-filter)
        (set-buffer buf)
        (inferior-scheme-mode)
        ;; Set for Inferior Scheme buffer.
        (set (make-local-variable 'inferior-scheme-command-line) cmd)))
    (pop-to-buffer isb)))

(defcustom inferior-scheme-input-read-only t
  "Controls whether old input text in inferior Scheme buffers is
read-only."
  :type 'boolean
  :group 'inferior-scheme)

(defcustom inferior-scheme-output-read-only t
  "Controls whether output text in inferior Scheme buffers is
read-only."
  :type 'boolean
  :group 'inferior-scheme)

(defun inferior-scheme-set-read-only (s e)
  (add-text-properties s e
   `(read-only t
     rear-nonsticky t
     front-sticky ,(let ((fsp (get-text-property s 'front-sticky)))
                     (if (listp fsp)
                         (append fsp '(read-only))
                       fsp)))))

(defun inferior-scheme-output-filter (proc str)
  (let ((ret-val (comint-output-filter proc str)))
    (when inferior-scheme-output-read-only
      (with-current-buffer (process-buffer proc)
        (let ((os comint-last-output-start)
              (oe (process-mark proc))
              (inhibit-modification-hooks t))
          (inferior-scheme-set-read-only os oe))))
    ret-val))

(defun inferior-scheme-send-input ()
  "Send new input from inferior Scheme buffer to inferior Scheme
process."
  (comint-send-input)
  (let ((is comint-last-input-start)
        (ie comint-last-input-end)
        (inhibit-modification-hooks t))
    ;; Remove this property comint just added. 
    (remove-list-of-text-properties is ie '(font-lock-face))
    (when font-lock-mode
      (save-excursion    ;; Not sure `save-excursion' or `save-match-data'
        (save-match-data ;; are necessary.  Use them to be safe.
          ;; Font Lock does not remove the prompt's 'font-lock-face
          ;; property when it re-fontifies the extended region.
          (font-lock-fontify-region is ie))))
    (when inferior-scheme-input-read-only
      (inferior-scheme-set-read-only is ie))))

(defun inferior-scheme-send-string (str)
  "Insert STR at the end of the inferior Scheme buffer and send
it to the inferior Scheme process.  Leading whitespace in STR is
not included in the inserted text.  The inserted text is
indented.  If `original-column' is bound during the call, use it
to preserve original relative indentation of multiple lines by
indenting them rigidly, else indent using `indent-region'.  If
there already is unsent input in the inferior Scheme buffer,
nothing is inserted nor sent and an error is signaled."
  (save-excursion
    (save-match-data
      (let ((proc (inferior-scheme-proc)))
        (set-buffer (process-buffer proc))
        (let ((x (process-mark proc))
              (s (let ((s 0))
                   (while (and (< s (length str))
                               (string-match scheme-whitespace-rx
                                             (substring str s (1+ s))))
                     (setq s (1+ s)))
                   s))
              (e (if (and (> (length str) 0)
                          (char-equal ?\n (elt str (1- (length str)))))
                     (1- (length str))
                   (length str))))
          (unless (= x (point-max))
            (error "Unsent input in inferior Scheme buffer %s" (current-buffer)))
          (goto-char x)
          (insert (substring-no-properties str s e)) ;; also removes 'read-only
          (if (boundp 'original-column)
              (let ((d (- (progn (goto-char x) (current-column))
                          original-column)))
                (forward-line)
                (indent-rigidly (point) (point-max) d))
            (indent-region x (point-max))))
        (goto-char (point-max))
        (inferior-scheme-send-input)))))

(defun inferior-scheme-send-region (start end)
  "Send the current region to the inferior Scheme process."
  (interactive "r")
  (inferior-scheme-send-string (buffer-substring start end)))

(defun inferior-scheme-send-preceding (f em)
  (let ((x (inferior-scheme-get-preceding f)))
    (if x
        (let ((original-column (elt x 0)))
          (inferior-scheme-send-region (elt x 1) (elt x 2))
          t)
      (error em))))

(defun inferior-scheme-send-top-level-form ()
  "Send the top-level form beginning before point to the inferior
Scheme process.  All of the form is sent.  Signal an error if
point is not after the beginning of a top-level form."
  (interactive)
  (inferior-scheme-send-preceding 'scheme-beginning-of-top-level-form
                                  "No preceding top-level form"))

(defun inferior-scheme-send-preceding-sexp ()
  "Send the s-expression beginning before point to the inferior
Scheme process.  All of the s-expression is sent.  Signal an
error if point is not after the beginning of an s-expression."
  (interactive)
  (inferior-scheme-send-preceding 'backward-sexp "No preceding s-expression"))

(defun inferior-scheme-switch-to-buffer (eob-p)
  "Switch to the current inferior Scheme process buffer.  With
argument, position cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer
   (or (get-buffer inferior-scheme-buffer)
       (inferior-scheme-interactively-start-process)
       (error "No current process buffer")))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defun inferior-scheme-send-region-and-go (start end)
  "Call `inferior-scheme-send-region'.
Then switch to the inferior Scheme process buffer."
  (interactive "r")
  (inferior-scheme-send-region start end)
  (inferior-scheme-switch-to-buffer t))

(defun inferior-scheme-send-top-level-form-and-go ()
  "Call `inferior-scheme-send-top-level-form'.
Then switch to the inferior Scheme process buffer."
  (interactive)
  (inferior-scheme-send-top-level-form)
  (inferior-scheme-switch-to-buffer t))

(defun inferior-scheme-send-preceding-sexp-and-go ()
  "Call `inferior-scheme-send-preceding-sexp'.
Then switch to the inferior Scheme process buffer."
  (interactive)
  (inferior-scheme-send-preceding-sexp)
  (inferior-scheme-switch-to-buffer t))

(defun inferior-scheme-proc ()
  "Return the current inferior Scheme process, starting one if
necessary.  See variable `inferior-scheme-buffer'."
  (let ((isb (if (eq major-mode 'inferior-scheme-mode)
                 (buffer-name)
               inferior-scheme-buffer)))
    (unless (and (get-buffer isb)
                 (comint-check-proc isb))
      (inferior-scheme-interactively-start-process))
    (or (get-buffer-process isb)
        (error "No current process"))))

(defun inferior-scheme-interactively-start-process ()
  "Start an inferior Scheme process.  Return the buffer of the
process started.  Since this command is run implicitly, always
ask the user for the command-line to start the Scheme process."
  (save-window-excursion
    (inferior-scheme (read-string "Run Scheme: " inferior-scheme-command-line))))

(defcustom inferior-scheme-return-function 'newline-and-indent
  "Function to use when an enter key is pressed when point is at
a position that will not cause input to be sent to the inferior
Scheme process.  E.g., so a newline can be inserted and the next
line indented."
  :type 'function
  :group 'inferior-scheme)

(defun inferior-scheme-return ()
  "Bound to return key.  If point is after last output and in a
form or not at the end of the new input, call
`inferior-scheme-return-function'.  Else, send the input (new or
old) to the inferior Scheme process."
  (interactive)
  (if (and (>= (point) (process-mark (get-buffer-process (current-buffer))))
           (or (nth 1 (syntax-ppss))
               (< (point) (point-max))))
      (funcall inferior-scheme-return-function)
    (inferior-scheme-send-input)))

(defun inferior-scheme-delete-input ()
  "Delete any new unsent input, even if it is read-only.  This is
generally useful and also useful if a bug causes unsent input
text to be read-only."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (process-mark (get-buffer-process (current-buffer)))
                   (point-max))))

;;;; Aliases that correspond to old cmuscheme.el
;; These are deprecated and should be removed in the future.

(defalias 'run-scheme 'inferior-scheme)
(defalias 'switch-to-scheme 'inferior-scheme-switch-to-buffer)
(defvaralias 'scheme-buffer 'inferior-scheme-buffer)
(defalias 'scheme-send-region 'inferior-scheme-send-region)
(defalias 'scheme-send-definition 'inferior-scheme-send-top-level-form)
(defalias 'scheme-send-last-sexp 'inferior-scheme-send-preceding-sexp)
(defalias 'scheme-send-region-and-go 'inferior-scheme-send-region-and-go)
(defalias 'scheme-send-definition-and-go 'inferior-scheme-send-top-level-form-and-go)

(message "Using Derick's Inferior Scheme mode library.")

(provide 'inferior-scheme)
