;;; clean.el --- Clean language editing mode.

;; Copyright 2014 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;;; Commentary:
;;
;; TODO
;;
;; Made on GNU Emacs 23.3.1.  Might need to be tested on the latest release
;; version.

;;;###autoload
(define-generic-mode clean-mode
  ; Comments
  '("//" ("/*" . "*/"))
  ; Keywords
  '("case" "class" "code" "definition" "derive" "export" "foreign" "from"
    "generic" "if" "implementation" "import" "in" "infix" "infixl" "infixr"
    "instance" "let" "module" "of" "otherwise" "special" "system" "where" "with")
  ; Font-lock
  `((,(rx (seq ?' (+ (not (any ?'))) ?'))
     . font-lock-constant-face)
    (,(let* ((digit-16 'hex-digit)
             (digit-10 'digit)
             (digit-8 '(char (?0 . ?7)))
             (sign '(char ?+ ?- ?~))
             (int `(or (seq (? ,sign) "0x" (+ ,digit-16))
                       (seq (? ,sign) (+ ,digit-10))
                       (seq (? ,sign) ?0 (+ ,digit-8))))
             (real `(seq (? ,sign) (+ ,digit-10) ?. (+ ,digit-10)
                         (? (seq ?E (? ,sign) (+ ,digit-10))))))
        (rx-to-string `(seq word-boundary (or ,real ,int) word-boundary)))
     . font-lock-constant-face)
    (,(rx (seq word-boundary (or "True" "False") word-boundary))
     . font-lock-constant-face)
    (,(regexp-opt '("->" "::" ":==" "=:" "=>" "=" "\\" "(" ")" "[" "]" ":"
                    "\\\\" "<-" "<-:" "<|-" "{" "}" "." "!" "&" "#" "#!" "*"
                    "|" "," ";"))
     . emacs-lisp-paren-face))
  ; Auto-mode filename extensions
  '("\\.\\(dcl\\|icl\\)\\'")
  `(,(function
      (lambda ()
        (set (make-local-variable 'tab-width) 4)
        (set (make-local-variable 'indent-tabs-mode) t)
        (set (make-local-variable 'tab-stop-list)
             '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))
        (set (make-local-variable 'indent-line-function) 'clean-mode-indent-line)
        )))
  )


(defun clean-mode-indent-line ()
  (let ((savep (> (current-column) (current-indentation)))
        (indent
         (save-excursion
           (goto-char (line-beginning-position 0))
           (current-indentation))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))