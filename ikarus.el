;;; ikarus.el --- Running Ikarus Scheme under Emacs

;; Copyright (C) 2008  Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2008  Derick Eddington <derick.eddington@gmail.com>
;;
;; Author: Andreas Rottmann <a.rottmann@gmx.at>
;; Author: Derick Eddington <derick.eddington@gmail.com>
;; Keywords: languages, tools
;;
;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a major mode derived from compilation-mode.  It
;; overrides some functionality to allow it to work with Ikarus's unhandled
;; exception print-out source positions.
;;
;; Ikarus uses character-based file locations, not line and column, which makes
;; it necessary to override code of compilation-mode instead of using it as-is,
;; but on the other hand, the overriding code is quite a bit simpler than the
;; overriden code, as character positions are easier to handle.

;;; Usage:  Read the doc-strings below.

;;; Issues:
;;
;; Escapes ("\n", "\t", etc.) in quoted command-line arguments do not work.

(require 'compile)

(defgroup ikarus nil
  "Running Ikarus as a compilation mode."
  :group 'scheme
  :link '(custom-group-link :tag "Compilation group" compilation))

(defcustom ikarus-program-name "ikarus"
  "Program invoked by `ikarus-run'."
  :type 'string
  :group 'ikarus)

(defcustom ikarus-program-arguments '("--r6rs-script")
  "Command-line arguments given to `ikarus-program-name' by
`ikarus-run'.  These will be followed by the file to run and
additional arguments."
  :type '(repeat string)
  :group 'ikarus)

(defconst ikarus-source-position-regex
  "&source-position:\n\\s-*file-name: \\(\"\\(?:\\\\\"\\|[^\"]\\)+\"\\)\n\\s-*character: \\([0-9]+\\)")

(defun ikarus-visit-preceding-source-position ()
  "Visit the file and position indicated by the last
&source-position condition.  This searches backwards in the
current buffer for a print-out of an unhandled exception with a
&source-position component, and visits that position in the other
window.  You can, for instance, use this in a shell buffer that
you have run Ikarus in.

See also `ikarus-run' and `ikarus-rerun' for a convenient way to
run files with Ikarus and visit resulting positions."
  (interactive)
  (let ((found-p (save-excursion
		   (re-search-backward ikarus-source-position-regex nil t))))
    (cond (found-p
	   (let ((file-name (car (read-from-string (match-string 1))))
		 (pos (string-to-number (match-string 2))))
	     (find-file-other-window file-name)
	     (goto-char pos)))
	  (t
	   (message "No &source-position found")))))

(defun ikarus-next-error (n &optional reset)
  "Advance to the next &source-position exception print-out and
visit the file and position it indicates.

This is the value of `next-error-function' in Ikarus mode
buffers."
  (interactive "p")
  (when reset
    (setq compilation-current-error nil))
  (let* ((columns compilation-error-screen-columns) ; buffer's local value
	 (last 1)
	 (loc (compilation-next-error (or n 1) nil
				      (or compilation-current-error
					  compilation-messages-start
					  (point-min))))
	 (marker (point-marker)))
    (setq compilation-current-error (point-marker)
	  overlay-arrow-position
	    (if (bolp)
		compilation-current-error
	      (copy-marker (line-beginning-position)))
	  loc (car loc))
    ;; If loc contains no marker, no error in that file has been visited.  If
    ;; the marker is invalid the buffer has been killed.  So, recalculate the
    ;; marker.
    (unless (and (nth 3 loc) (marker-buffer (nth 3 loc)))
      (with-current-buffer (compilation-find-file marker (caar (nth 2 loc))
						  (cadr (car (nth 2 loc))))
	(save-excursion
	  (goto-char (nth 1 loc))
	  (if (nth 3 loc)
	      (set-marker (nth 3 loc) (point))
	    (setcdr (nthcdr 2 loc) `(,(point-marker)))))))
    (compilation-goto-locus marker (nth 3 loc) nil)
    (setcdr (nthcdr 3 loc) t)))

(defvar ikarus-previous-arguments nil
  "Buffer-local variable used to save the additional command-line
arguments from the last call to `ikarus-run' for the current
buffer.  Used by `ikarus-rerun'.")
(make-variable-buffer-local 'ikarus-previous-arguments)

(defun ikarus-run (&optional file-name &rest args)
  "Run Ikarus on FILE-NAME with ARGS as additional command-line
arguments.

Interactively:
  With no prefix argument, use the current buffer's file-name and
    no additional arguments.
  With one prefix argument (C-u), use the current buffer's
    file-name and prompt for additional arguments.
  With two prefix arguments (C-u C-u), prompt for file-name and
    prompt for additional arguments.

If a buffer visiting FILE-NAME exists, that buffer's local
variable `ikarus-previous-arguments' is set to ARGS so that
`ikarus-rerun' can use it."
  (interactive  ;; C-u is '(4) and C-u C-u is '(16)
   (if (member current-prefix-arg '(nil (4) (16)))
       (let (f a)
         (when (equal '(16) current-prefix-arg)
           (setq f (read-file-name "Run Ikarus on: ")))
         (when (or (equal '(4) current-prefix-arg)
                   (equal '(16) current-prefix-arg))
           (setq a (split-string-and-unquote
                    (read-string "Arguments: "))))
         (cons f a))
     (error "Invalid prefix argument %S" current-prefix-arg)))
  (unless file-name
    (let ((fn (buffer-file-name)))
      (if fn
          (setq file-name fn)
        (error "Buffer does not have a file-name"))))
  (let ((b (get-file-buffer file-name)))
    (when b
      (with-current-buffer b
        (setq ikarus-previous-arguments args))))
  (compilation-start (combine-and-quote-strings
                      `(,ikarus-program-name ,@ikarus-program-arguments
                        ,file-name ,@args))
                     'ikarus-mode))

(defun ikarus-rerun ()
  "Call `ikarus-run' with the saved arguments from the last run.
If the current buffer has not yet been run, it will be run
without arguments."
  (interactive)
  (apply 'ikarus-run nil ikarus-previous-arguments))

(defconst ikarus-font-lock-keywords
  (append
   `((,ikarus-source-position-regex
      (1 compilation-error-face)
      (2 compilation-line-face
         nil t)
      (0 (ikarus-source-position-properties 1 2)
         append)))			; for compilation-message-face
   compilation-mode-font-lock-keywords))

(defun ikarus-source-position-properties (file char)
  (unless (< (next-single-property-change (match-beginning 0) 'directory nil (point))
	     (point))
    (setq file (car (read-from-string (match-string file))))
    (setq char (string-to-number (match-string char)))
    `(face ,compilation-message-face
      message ((nil ,char ((,file nil)) nil) 2 nil)
      help-echo "mouse-2: visit this file and position"
      keymap compilation-button-map
      mouse-face highlight)))

(define-compilation-mode ikarus-mode "Ikarus"
  "Specialization of compilation-mode for running Ikarus Scheme."
  (setq font-lock-defaults '(ikarus-font-lock-keywords t))
  (local-set-key (kbd "RET") 'ikarus-visit-preceding-source-position)
  (setq next-error-function 'ikarus-next-error))

(message "Using Derick's Ikarus mode library.")

(provide 'ikarus)
