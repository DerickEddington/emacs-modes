;;; scheme.el --- Scheme editing mode, with support for R6RS lexical syntax.

;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;;; Commentary:
;;
;; Major mode for editing Scheme source-code, with support for R6RS lexical
;; syntax.  `;' line comments, `#|...|#' multi-line nestable comments, `#;'
;; multi-line nestable s-expression comments, prefixes (``', `#,@', etc.), and
;; symbol escapes all work with syntax highlighting and motion commands.
;; Parentheses and other related lexemes, prefixes, and constant literals are
;; highlighted with their own font faces.  Customizable things are: font faces,
;; regular expressions to match first-subform symbols to highlight as keywords,
;; regular expressions to match first-subform symbols to specify indentation
;; style, regular expressions to match first-subform symbols to specify what
;; forms enclose top-level forms (e.g., "library" and "module"), and more.
;;
;; Made on GNU Emacs 23.0.60.1.  Might need to be tested on the latest release
;; version.

;;; Noteable changes from old Scheme mode:
;;
;; The RET key by default is bound to `newline-and-indent'.  `indent-tabs-mode'
;; by default is set to nil.  I think more people want this than not.  You can
;; change these in your `scheme-mode-hook' hook.
;;
;; This mode sets `beginning-of-defun-function' to
;; `scheme-beginning-of-top-level-form'.  `scheme-beginning-of-top-level-form'
;; does not need top-level forms to start at column 0, they can start anywhere.
;; Top-level forms can be enclosed in forms with names recognized by
;; `scheme-top-level-enclosing-form-names', and they are properly considered
;; top-level forms, i.e., `beginning-of-defun' and `end-of-defun' move relative
;; to the enclosing form's top-level.  By default, only "library" is a
;; recognized enclosing form name, and this can be customized.  Enclosing forms
;; can be nested, e.g., a library in a library; however, because
;; `beginning-of-defun' and `end-of-defun' move to the beginning of the line and
;; the next line, they can move out of the nested enclosing form; you can change
;; your key-bindings to use `scheme-beginning-of-top-level-form' directly
;; instead.
;;
;; The indentation specification symbol properties 'scheme-indent-function and
;; 'scheme-indent-hook are not supported.  Use the variables
;; `scheme-default-indent-specs' and/or `scheme-indent-specs' to specify
;; indentation.
;;
;; The indentation specification value 'defun, for "define"-like forms, is not
;; supported, because the value 1 should be used instead because if the subform
;; after the "define", typically the binding clause, is on the next line, 1
;; indents it twice the normal indentation, which is consistent with how other
;; binding forms are indented.

;;; Known Issues:
;;
;; Emacs currently doesn't have support for regular expressions to match
;; characters based on Unicode general categories.  So, this library computes,
;; at compile-time, regular expression character sets for the needed general
;; categories.  This takes over 10 seconds (on my computer), so you probably
;; want to compile this library so this is done only once then and not every
;; time this library is loaded.  Also, including every general category
;; specified by R6RS makes the regular expressions too large for Emacs and it
;; errors trying to use them (some of them are pretty big); so the large more
;; exotic ones are currently disabled and so the full identifier set of
;; characters is not supported, but I don't think anyone is yet using the
;; disabled ones.  I hope Emacs will get support to directly match Unicode
;; general categories, and then all the ones used by R6RS can be supported and
;; computing character sets won't be needed.
;;
;; "#;foo'bar" all gets highlighted as a comment because Emacs' `scan-sexps'
;; skips passed characters with "expression prefix" syntax and keeps going.  But
;; this only happens when datums are crammed immediately next to each other.
;; "#;foo 'bar" works.
;;
;; There are other corner cases that don't get highlighted correctly and/or
;; don't work right with the motion commands.  This is because they can't be
;; properly handled using only Emacs' syntax table and Font Lock.  It seems like
;; proper handling requires implementing our own syntax parsing and
;; fontification.

;;; To Do:
;;
;; Should other customizable variables be able to be file-locally-set like the
;; indentation specification facility?
;;
;; Describe in appropriate doc-strings that non-greedy regular expressions are
;; needed for `scheme-keyword-names' and friends.
;;
;; Imenu support.
;;
;; Variables' binding sites highlighting.  Provide utility procedures that
;; highlight the right symbols in common binding clauses.  Make customizable so
;; user can specify regular expressions for first-subform symbols and associated
;; procedures to do the highlighting.
;;
;; Highlight string and symbol escapes.

;;; Misc. Notes:
;;
;; The font lock (a.k.a. syntax highlighting) level 4 implemented by this
;; library is slow.  Level 4 highlights invalid lexical syntax as invalid, and
;; to do this, it must first highlight all identifiers.  Because it's slow and I
;; added it mostly for fun, it's commented-out.  Level 3 is not slow.  If level
;; 4 could be made fast enough, that would be cool.
;;
;; Highlighting `#;' comments, defined names, and outline comments requires
;; using Font Lock's multi-line facility.  I believe this library's relying on
;; the `font-lock-multiline' variable is reliable because for this library the
;; only place a new multi-line region can be introduced or removed is at the
;; beginning of the region and this allows it all to be marked with the
;; 'font-lock-multiline text property which from then on will allow changes to
;; any part to extend the re-fontification region to encompass the entire
;; multi-line range.

(require 'lisp-mode)
(require 'outline)
(require 'rx)

(defgroup scheme nil
  "Editing Scheme source-code."
  :group 'lisp
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces))

(defgroup scheme-faces nil
  "Faces for highlighting Scheme source-code."
  :group 'scheme
  :group 'font-lock-faces)

(defconst scheme-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap "Scheme")))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map "\n" 'newline-and-indent)
    (define-key map "\r" 'newline-and-indent)  ;; Needed?
    ;; TODO?: Whatever is bound to `delete-indentation' (typically M-^) rebind
    ;;        to our own which indents also.
    (define-key map [menu-bar scheme] (cons "Scheme" menu-map))
    (define-key menu-map [uncomment-region]
      '("Uncomment Region" . uncomment-region))
    (put 'uncomment-region 'menu-enable 'mark-active)
    (define-key menu-map [comment-region]
      '("Comment Region" . comment-region))
    (put 'comment-region 'menu-enable 'mark-active)
    (define-key menu-map [indent-region]
      '("Indent Region" . indent-region))
    (put 'indent-region 'menu-enable 'mark-active)
    (define-key menu-map [inferior-scheme]
      '("Run Inferior Scheme" . inferior-scheme))
    map)
  "Keymap for Scheme mode.")

(defconst scheme-mode-syntax-table
  (let ((st (make-syntax-table))) ;; inherits from standard syntax table
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?' "'" st)
    (modify-syntax-entry ?` "'" st)
    (modify-syntax-entry ?, "'" st)
    (modify-syntax-entry ?@ "_ p" st)
    (modify-syntax-entry ?# "' 14b" st)
    (modify-syntax-entry ?\; "<" st)
    (dolist (c '(?\n ?\r ?\x85 ?\x2028 ?\x2029))
      (modify-syntax-entry c ">" st))
    (modify-syntax-entry ?\| "_ 23bn" st)  ;; needed for mantissa width
    (dolist (c '(?! ?$ ?% ?& ?* ?/ ?: ?< ?= ?> ?? ?^ ?_ ?~ ?+ ?- ?.))
      (modify-syntax-entry c "_" st))
    st)
  "Syntax table for Scheme mode.")

;;;; Support for when Font Lock is off, i.e., no syntax highlighting.

(defun scheme-correct-syntax (beg end)
  "This makes `;' characters in `#;' comments and in `\x3BB;'
symbol escapes not have line-comment syntax and makes \"vu8\" in
`#vu8(' bytevector prefixes have expression prefix syntax.  Used
when font-lock-mode is off.  When it is on, the correcting must
instead be done by `scheme-font-lock-syntactic-keywords'."
  (let ((modified (buffer-modified-p))
        (inhibit-modification-hooks t)
        (inhibit-read-only t))
    (save-match-data
      (save-excursion
        (unwind-protect
            (progn
              (goto-char end)
              (let ((limit (line-beginning-position 2)))
                (goto-char beg)
                (goto-char (line-beginning-position))
                (while (re-search-forward
                        (rx (? (group ?#)) (? (group "\\x" (+ hex-digit)))
                            (or (group ?\;) (seq (group "vu8") ?\()))
                        limit t)
                  (cond
                   ((and (match-beginning 1)
                         (not (match-beginning 2))
                         (match-beginning 3))
                    ;; Make semi-colon be expression prefix syntax.
                    (put-text-property (match-beginning 3) (match-end 3)
                                       'syntax-table '(6)))
                   ((and (match-beginning 1)
                         (not (match-beginning 2))
                         (match-beginning 4))
                    ;; Make vu8 be expression prefix syntax.
                    (put-text-property (match-beginning 4) (match-end 4)
                                       'syntax-table '(6)))
                   ((and (not (match-beginning 1))
                         (match-beginning 2)
                         (match-beginning 3))
                    ;; Make semi-colon be symbol syntax.
                    (put-text-property (match-beginning 3) (match-end 3)
                                       'syntax-table '(3)))
                   ((and (not (match-beginning 1))
                         (not (match-beginning 2))
                         (match-beginning 3))
                    ;; Make semi-colon be comment start syntax in case we
                    ;; changed it and then what preceded it was just modified to
                    ;; no longer match.
                    (put-text-property (match-beginning 3) (match-end 3)
                                       'syntax-table '(11)))
                   ((and (not (match-beginning 1))
                         (not (match-beginning 2))
                         (match-beginning 4))
                    ;; Make vu8 be word syntax in case we changed it and then
                    ;; what preceded it was just modified to no longer match.
                    (put-text-property (match-beginning 4) (match-end 4)
                                       'syntax-table '(2)))))))
          (unless modified
            (set-buffer-modified-p nil)))))))

(defun scheme-after-change (beg end old-len)
  (unless font-lock-mode
    (scheme-correct-syntax beg end)))

(defun scheme-unfontify-buffer ()
  "When Font Lock unfontifies the buffer, it removes
'syntax-table text properties.  So, they need to be put back."
  (font-lock-default-unfontify-buffer)
  (scheme-correct-syntax (point-min) (point-max)))

;;;; Regular Expressions for Scheme lexical syntax

(eval-when-compile
  (require 'mule)  ;; for get-char-code-property
  (require 'rx))   ;; for rx-to-string

(defvar scheme-startup-vars
  ;; Regular expressions for lexemes from R6RS 4.2.1.  It is done like this so
  ;; `general-categories' can be computed at compile-time and its value used to
  ;; compute multiple variables we need at run-time.
  (eval-when-compile
    (let* ((general-categories
            ;; This is done because Emacs doesn't have general category regexps.
            ;; Had to comment-out the large ones because they make the regexp
            ;; larger than Emacs allows.  This means the full identifier syntax
            ;; is not currently recognized.  Hopefully, Emacs will get support
            ;; that allows it to be.
            (let ((al (list (list 'Lu) (list 'Ll) (list 'Lt)
                            (list 'Lm)  ;(list 'Lo) (list 'Mn)
                            (list 'Nl) (list 'No) (list 'Pd)
                            (list 'Pc) (list 'Po) (list 'Sc)
                            ;(list 'Sm) (list 'Sk) (list 'So) (list 'Co)
                            (list 'Nd) (list 'Mc) (list 'Me)
                            (list 'Zs) (list 'Zl) (list 'Zp)))
                  (c 0))
              (while (< c #x110000)
                (unless (and (<= #xD800 c) (<= c #xDFFF))
                  (let* ((gc (get-char-code-property c 'general-category))
                         (a (assq gc al)))
                    (when a (setcdr a (cons c (cdr a))))))
                (setq c (1+ c)))
              al))
           (char-set
            (lambda (gc) `(char . ,(cdr (assq gc general-categories)))))
           (char-set/non-ascii
            (lambda (gc)
              `(char . ,(remove nil (mapcar (lambda (x) (and (> x 127) x))
                                            (cdr (funcall char-set gc)))))))
           (Lu (funcall char-set/non-ascii 'Lu))
           (Ll (funcall char-set/non-ascii 'Ll))
           (Lt (funcall char-set/non-ascii 'Lt))
           (Lm (funcall char-set/non-ascii 'Lm))
           ;(Lo (funcall char-set/non-ascii 'Lo))
           ;(Mn (funcall char-set/non-ascii 'Mn))
           (Nl (funcall char-set/non-ascii 'Nl))
           (No (funcall char-set/non-ascii 'No))
           (Pd (funcall char-set/non-ascii 'Pd))
           (Pc (funcall char-set/non-ascii 'Pc))
           (Po (funcall char-set/non-ascii 'Po))
           (Sc (funcall char-set/non-ascii 'Sc))
           ;(Sm (funcall char-set/non-ascii 'Sm))
           ;(Sk (funcall char-set/non-ascii 'Sk))
           ;(So (funcall char-set/non-ascii 'So))
           ;(Co (funcall char-set/non-ascii 'Co))
           (Nd (funcall char-set 'Nd))
           (Mc (funcall char-set 'Mc))
           (Me (funcall char-set 'Me))
           (Zs (funcall char-set 'Zs))
           (Zl (funcall char-set 'Zl))
           (Zp (funcall char-set 'Zp))
           (whitespace
            `(or (char ?\x20 ?\n ?\t ?\r ?\xB ?\f ?\x85) ,Zs ,Zl ,Zp))
           (begin-delimiter
            `(seq (or ,whitespace (char ?\( ?\) ?\[ ?\] ?\") "|#" line-start)
                  (? (* (? ?#) (or ",@" (char ?' ?` ?,))))))
           (end-delimiter
            `(or ,whitespace (char ?\( ?\) ?\[ ?\] ?\" ?\; ?#) line-end))
           (paren
            `(or "#vu8(" "#(" (char ?\( ?\) ?\[ ?\])
                 (seq ,begin-delimiter (group ?.) ,end-delimiter)))
           (boolean
            `(seq (group ?# (char ?T ?t ?F ?f)) ,end-delimiter))
           (character
            ;; NOTE: The 2nd `seq' was needed to avoid a bug in rx.el
            ;;       that was fixed in GNU Emacs CVS as of 2008-12-09.
            `(seq (group (seq "#\\" (or (or "nul" "alarm" "backspace" "tab"
                                            "linefeed" "newline" "vtab" "page"
                                            "return" "esc" "space" "delete")
                                        (seq ?x (+ hex-digit))
                                        anything)))
                  ,end-delimiter))
           (identifier
            (let* ((letter
                    '(char (?a . ?z) (?A . ?Z)))
                   (constituent
                    `(or ,letter ,Lu ,Ll ,Lt ,Lm ,Nl ;,Lo ,Mn ,Sm ,Sk ,So ,Co
                         ,No ,Pd ,Pc ,Po ,Sc))
                   (special-initial
                    '(char ?! ?$ ?% ?& ?* ?/ ?: ?< ?= ?> ?? ?^ ?_ ?~))
                   (special-subsequent
                    '(char ?+ ?- ?. ?@))
                   (inline-hex-escape
                    '(seq "\\x" (+ hex-digit) ?\;))
                   (initial
                    `(or ,constituent ,special-initial ,inline-hex-escape))
                   (subsequent
                    `(or ,initial digit ,special-subsequent ,Nd ,Mc ,Me))
                   (peculiar-identifier
                    `(or ?+ ?- "..." (seq "->" (* ,subsequent))))
                   (begin-delimiter
                    `(or ,begin-delimiter "#!")))
              `(seq (? (group ,begin-delimiter))
                    (group (or (seq ,initial (* ,subsequent))
                               ,peculiar-identifier))
                    ,end-delimiter)))
           (number
            (let* ((any-true
                    (lambda (l) (< 0 (length (remove nil l)))))
                   (all-true
                    (lambda (l) (= (length l) (length (remove nil l)))))
                   (or
                    (lambda (&rest args)
                      (and (funcall any-true args)
                           `(or . ,(remove nil args)))))
                   (seq
                    (lambda (&rest args)
                      (and (funcall all-true args)
                           `(seq . ,args))))

                   (digit-16 'hex-digit)
                   (digit-10 'digit)
                   (digit-8 '(char (?0 . ?7)))
                   (digit-2 '(char ?0 ?1))
                   (digit
                    (lambda (R)
                      (cdr (assoc R `((16 . ,digit-16)
                                      (10 . ,digit-10)
                                      (8 . ,digit-8)
                                      (2 . ,digit-2))))))
                   (radix-16
                    '(seq ?# (char ?x ?X)))
                   (radix-10
                    '(seq ?# (char ?d ?D)))
                   (radix-8
                    '(seq ?# (char ?o ?O)))
                   (radix-2
                    '(seq ?# (char ?b ?B)))
                   (radix
                    (lambda (R)
                      (cdr (assoc R `((16 . ,radix-16)
                                      (10 . ,radix-10)
                                      (8 . ,radix-8)
                                      (2 . ,radix-2))))))
                   (exactness
                    '(seq ?# (char ?i ?I ?e ?E)))
                   (sign
                    '(? (char ?+ ?-)))
                   (mantissa-width
                    `(? (seq ?| (+ ,digit-10))))
                   (exponent-marker
                    '(char ?e ?E ?s ?S ?f ?F ?d ?D ?l ?L))
                   (suffix
                    `(? (seq ,exponent-marker ,sign (+ ,digit-10))))
                   (prefix
                    (lambda (R)
                      (if (= 10 R)
                          `(or (seq ,(funcall radix R) (? ,exactness))
                               (seq ,exactness (? ,(funcall radix R)))
                               (group ,begin-delimiter))
                        `(or (seq ,(funcall radix R) (? ,exactness))
                             (seq ,exactness ,(funcall radix R))))))
                   (uinteger
                    (lambda (R) `(+ ,(funcall digit R))))
                   (decimal-10
                    `(or (seq (+ ,digit-10) ?. (* ,digit-10) ,suffix)
                         (seq ?. (+ ,digit-10) ,suffix)
                         (seq ,(funcall uinteger 10) ,suffix)))
                   (decimal
                    (lambda (R)
                      (if (= 10 R) decimal-10 nil))) ; nil gets propagated
                   (ureal
                    (lambda (R)
                      (funcall or
                               `(seq ,(funcall uinteger R) ?/ ,(funcall uinteger R))
                               (funcall seq (funcall decimal R) mantissa-width)
                               (funcall uinteger R))))
                   (naninf
                    '(seq (or (seq (char ?N ?n) (char ?A ?a) (char ?N ?n))
                              (seq (char ?I ?i) (char ?N ?n) (char ?F ?f)))
                          ".0"))
                   (real
                    (lambda (R)
                      `(or (seq ,sign ,(funcall ureal R))
                           (seq ?+ ,naninf)
                           (seq ?- ,naninf))))
                   (complex
                    (lambda (R)
                      `(or (seq ,(funcall real R) ?@ ,(funcall real R))
                           (seq ,(funcall real R) ?+ ,(funcall ureal R) (char ?i ?I))
                           (seq ,(funcall real R) ?- ,(funcall ureal R) (char ?i ?I))
                           (seq ,(funcall real R) ?+ ,naninf (char ?i ?I))
                           (seq ,(funcall real R) ?- ,naninf (char ?i ?I))
                           (seq ,(funcall real R) ?+ (char ?i ?I))
                           (seq ,(funcall real R) ?- (char ?i ?I))
                           (seq ?+ ,(funcall ureal R) (char ?i ?I))
                           (seq ?- ,(funcall ureal R) (char ?i ?I))
                           (seq ?+ ,naninf (char ?i ?I))
                           (seq ?- ,naninf (char ?i ?I))
                           ,(funcall real R) ;; needs to be after the above
                           (seq ?+ (char ?i ?I))
                           (seq ?- (char ?i ?I)))))
                   (num
                    (lambda (R) `(seq ,(funcall prefix R) ,(funcall complex R)))))
              `(seq (group (or ,(funcall num 10)
                               ,(funcall num 16)
                               ,(funcall num 8)
                               ,(funcall num 2)))
                    ,end-delimiter))))
      (mapcar 'rx-to-string
              (list whitespace begin-delimiter end-delimiter paren
                    boolean character identifier number)))))

(defconst scheme-whitespace-rx      (nth 0 scheme-startup-vars))
(defconst scheme-begin-delimiter-rx (nth 1 scheme-startup-vars))
(defconst scheme-end-delimiter-rx   (nth 2 scheme-startup-vars))
(defconst scheme-paren-rx           (nth 3 scheme-startup-vars))
(defconst scheme-boolean-rx         (nth 4 scheme-startup-vars))
(defconst scheme-character-rx       (nth 5 scheme-startup-vars))
(defconst scheme-identifier-rx      (nth 6 scheme-startup-vars))
(defconst scheme-number-rx          (nth 7 scheme-startup-vars))
(setq scheme-startup-vars nil)  ;; let it be GC'ed

;;;; Syntax highlighting, a.k.a. Font Lock.

(defconst scheme-font-lock-syntactic-keywords
  `((,(rx ?# (or (group ?\;) (seq (group "vu8") ?\()))
     (1 "'" nil t)
     (2 "'" nil t))
    (,(function
       (lambda (limit)
         (let (found)
           (while (and (not found)
                       (re-search-forward
                        (rx (? (group ?#)) "\\x" (+ hex-digit) (group ?\;))
                        limit t))
             (when (not (match-beginning 1))
               (setq found t)))
           found)))
     2 "_"))
  "Font lock syntactic specification for Scheme mode.  This makes
`;' characters in `#;' comments and in `\x3BB;' symbol escapes
not have line-comment syntax and makes \"vu8\" in `#vu8('
bytevector prefixes have expression prefix syntax.  Only used
when font-lock-mode is on.  When it is off, the correcting must
instead be done by `scheme-correct-syntax'.")

(defun scheme-re-search-forward/goto-end-of (regexp limit group)
  (and (re-search-forward regexp limit t)
       (goto-char (match-end group))))

(defcustom scheme-outline-begin-rx
  (rx line-start (* (char ?\x20 ?\t)) (group ";;;"))
  "Regular expression which matches the beginning of an outline
comment.  It is set as the buffer-local value of `outline-regexp'
for `outline-minor-mode' to use if it's enabled.  It must have 1
sub-match group.  Sub-match 1 will be fontified with
`scheme-outline-begin-face'.  The purpose of sub-match 1 is to
allow surrounding text that will not be fontified.  Everything
following the end of the entire match up to but not including
sub-match 1 of `scheme-outline-end-rx' will be fontified with
`scheme-outline-rest-face'."
  :group 'scheme
  :type 'regexp)

(defcustom scheme-outline-end-rx
  (rx ?\n (group ""))
  "Regular expression which matches the end of an outline
comment.  This is set as the buffer-local value of
`outline-heading-end-regexp'for `outline-minor-mode' to use if
it's enabled.  It must have 1 sub-match group.  The beginning of
sub-match 1 delimits the end of the text fontified with
`scheme-outline-rest-face'.  The purpose of sub-match 1 is to
allow the very end to not be fontified.
See `scheme-outline-begin-rx'."
  :group 'scheme
  :type 'regexp)

(defun scheme-no-fontify-p (start end names)
  "Return t if any character from START to END has text property
'scheme-no-fontify and any one of the symbols in NAMES is
in the property's value.  Otherwise return nil."
  (let (found
        (i start))
    (while (and (not found) (< i end))
      (let ((p (text-property-not-all i end 'scheme-no-fontify nil)))
        (if p
            (if (let ((nfl (get-text-property p 'scheme-no-fontify))
                      f
                      (n names))
                  (while (and (not f) (consp n))
                    (if (memq (car n) nfl)
                        (setq f t)
                      (setq n (cdr n))))
                  f)
                (setq found t)
              (setq i (next-single-property-change p 'scheme-no-fontify nil end)))
          (setq i end))))
    found))

(defconst scheme-font-lock-keywords
  `((,(function
       ;; This might set the match end beyond limit. Since `font-lock-multiline'
       ;; is true, the Font Lock logic will notice and handle.
       (lambda (limit)
         (and (search-forward "#;" limit t)
              (let ((start (point)))
                (and (not (nth 8 (syntax-ppss (- start 2))))
                     (let ((end (condition-case err
                                    (or (scan-sexps start 1) (point-max))
                                  (scan-error (point-max)))))
                       (progn (set-match-data (list (- start 2) end))
                              (goto-char end))))))))
     0 font-lock-comment-face t) ;; override existing face
    (,(function
       (lambda (limit)
         (and (re-search-forward scheme-outline-begin-rx limit t)
              (let ((b (match-beginning 0))
                    (e (match-end 0))
                    (b1 (match-beginning 1))
                    (e1 (match-end 1)))
                ;; No limit on end search, so it can possibly be multi-line.
                (and (re-search-forward scheme-outline-end-rx nil t)
                     (progn (set-match-data
                             (list b (match-end 0)
                                   b1 e1
                                   e (match-beginning 1)))
                            t))))))
     (1 scheme-outline-begin-face prepend)
     (2 scheme-outline-rest-face prepend)))
  "Highlighting specification base level for Scheme mode.
Fontify `#;' s-expression comments and outline comments.")

(defvar scheme-keyword-names-rx nil)

(defcustom scheme-keyword-names
  '("and" "apply" "begin" "call-with-current-continuation" "call-with-values"
    "call/cc" "case" "case-lambda" "cond" "define" "define-condition-type"
    "define-record-type" "define-syntax" "define-values" "do" "dynamic-wind"
    "else" "export" "guard" "identifier-syntax" "if" "import" "lambda" "let\\*?"
    "let\\*?-values" "let-syntax" "letrec\\*?" "letrec-syntax" "library" "or"
    "quasiquote" "quasisyntax" "quote" "raise" "raise-continuable" "set!"
    "syntax" "syntax-case" "syntax-rules" "unless" "unquote" "unquote-splicing"
    "unsyntax" "unsyntax-splicing" "values" "when" "with-exception-handler"
    "with-syntax")
  "List of regular expressions which match keyword symbols.  They
do not need to ensure they match only between open-parenthesis
and end of symbol, that is done automatically.  Used by
highlighting level 1 for Scheme mode."
  :group 'scheme
  :type '(repeat regexp)
  :set (function
        (lambda (symbol value)
          (setq scheme-keyword-names-rx
                (and (> (length value) 0)
                     (rx-to-string
                      `(seq (char ?\( ?\[)
                            (group (or . ,(mapcar (function
                                                   (lambda (re) `(regexp ,re)))
                                                  value)))
                            (regexp ,scheme-end-delimiter-rx)))))
          (set-default symbol value))))

(defvar scheme-define-names-rx nil)

(defcustom scheme-define-names
  '("define" "define-condition-type" "define-record-type" "define-syntax")
  "List of regular expressions which match \"define\"-like forms.
They do not need to ensure they match only between
open-parenthesis and end of symbol, that is done automatically.
The identifier the \"define\" form binds will be highlighted with
`font-lock-variable-name-face' or `font-lock-function-name-face'.
Used by highlighting level 1 for Scheme mode."
  :group 'scheme
  :type '(repeat regexp)
  :set (function
        (lambda (symbol value)
          (setq scheme-define-names-rx
                (and (> (length value) 0)
                     (rx-to-string
                      `(seq (char ?\( ?\[)
                            (group (or . ,(mapcar (function
                                                   (lambda (re) `(regexp ,re)))
                                                  value)))
                            (regexp ,scheme-end-delimiter-rx)
                            (* (regexp ,scheme-whitespace-rx))
                            (* (char ?\( ?\[))))))
          (set-default symbol value))))

(defvar scheme-define-values-names-rx nil)

(defcustom scheme-define-values-names
  '("define-values")
  "List of regular expressions which match \"define-values\"-like
forms.  They do not need to ensure they match only between
open-parenthesis and end of symbol, that is done automatically.
The identifiers the \"define-values\" form binds will be
highlighted with `font-lock-variable-name-face'.  Used by
highlighting level 1 for Scheme mode."
  :group 'scheme
  :type '(repeat regexp)
  :set (function
        (lambda (symbol value)
          (setq scheme-define-values-names-rx
                (and (> (length value) 0)
                     (rx-to-string
                      `(seq (char ?\( ?\[)
                            (group (or . ,(mapcar (function
                                                   (lambda (re) `(regexp ,re)))
                                                  value)))
                            (regexp ,scheme-end-delimiter-rx)))))
          (set-default symbol value))))

(defvar scheme-module-names-rx nil)

(defcustom scheme-module-names
  '("library")
  "List of regular expressions which match module-like forms.
They do not need to ensure they match only between
open-parenthesis and end of symbol, that is done automatically.
The name of the module form will be highlighted with
`font-lock-variable-name-face'.  Used by highlighting level 1 for
Scheme mode."
  :group 'scheme
  :type '(repeat regexp)
  :set (function
        (lambda (symbol value)
          (setq scheme-module-names-rx
                (and (> (length value) 0)
                     (rx-to-string
                      `(seq (char ?\( ?\[)
                            (group (or . ,(mapcar (function
                                                   (lambda (re) `(regexp ,re)))
                                                  value)))
                            (regexp ,scheme-end-delimiter-rx)))))
          (set-default symbol value))))

(defun scheme-maybe-fontify-identifier (face no)
  (when (and (looking-at scheme-identifier-rx)
             (equal (match-beginning 1) (match-end 1)))
    (let ((b (match-beginning 2))
          (e (match-end 2)))
      (unless (or (text-property-not-all b e 'face nil)
                  (scheme-no-fontify-p b e no))
        (put-text-property b e 'face face)))))

(defun scheme-fontify-formals-names (face)
  "Fontify all identifiers in a \"formals\" form with face FACE.
Expects point to be immediately before the \"formals\" form, and
leaves point there."
  (save-excursion
    (cond ((looking-at (rx (char ?\( ?\[)))
           (forward-char)
           (condition-case err
               (while (not (eobp))
                 (forward-sexp)
                 (backward-sexp)
                 (scheme-maybe-fontify-identifier face '(extra))
                 (forward-sexp))
             (scan-error nil)))
          (t (scheme-maybe-fontify-identifier face '(extra))))))

(defun scheme-fontify-variable-or-function-name (var-face func-face)
  "Fontify a defined identifier as a variable or as a function.
Expects point to be immediately before the 2nd sub-form, and
leaves point there."
  (save-excursion
    (let ((face (if (looking-at (rx (+ (char ?\( ?\[))))
                    (progn (goto-char (match-end 0))
                           func-face)
                  var-face)))
      (scheme-maybe-fontify-identifier face '(extra)))))

(defun scheme-fontify-module-name (face)
  "Fontify the name of a module form.  Expects point to be
immediately before the 2nd sub-form, and leaves point there.  The
entire 2nd sub-form is fontified."
  (save-excursion
    (let* ((b (point))
           (e (progn (forward-sexp) (point))))
      (unless (or (text-property-not-all b e 'face nil)
                  (scheme-no-fontify-p b e '(extra)))
        (put-text-property b e 'face face)))))

(defun scheme-fontify-definitions (limit regexp func args)
  (while (and (< (point) limit)
              (scheme-re-search-forward/goto-end-of regexp limit 1))
    (let ((def-end (condition-case err
                       (progn (forward-sexp) (point))
                     (scan-error nil))))
      (put-text-property (match-beginning 0)
                         (or def-end (match-end 0))
                         'font-lock-multiline t)
      (when def-end
        (backward-sexp)
        (apply func args)
        (goto-char def-end)))))

(defconst scheme-font-lock-keywords-1
  (append scheme-font-lock-keywords
   `((,(function
        (lambda (limit)
          (let (found)
            (while (and (not found)
                        (search-forward "#!" limit t))
              (when (and (looking-at scheme-identifier-rx)
                         (not (match-beginning 1)))
                (goto-char (match-end 2))
                (set-match-data (list (- (match-beginning 2) 2) (match-end 2)))
                (setq found t)))
            found)))
      0 scheme-special-comment-face)
     (,(function
        (lambda (limit)
          (and scheme-keyword-names-rx
               (scheme-re-search-forward/goto-end-of
                scheme-keyword-names-rx limit 1)
               (not (scheme-no-fontify-p
                     (match-beginning 1) (match-end 1) '(extra))))))
      1 font-lock-keyword-face)
     (,(function
        (lambda (limit)
          (when scheme-define-names-rx
            (scheme-fontify-definitions limit scheme-define-names-rx
             (function scheme-fontify-variable-or-function-name)
             '(font-lock-variable-name-face font-lock-function-name-face)))
          nil))
      . ignored)
     (,(function
        (lambda (limit)
          (when scheme-define-values-names-rx
            (scheme-fontify-definitions limit scheme-define-values-names-rx
             (function scheme-fontify-formals-names)
             '(font-lock-variable-name-face)))
          nil))
      . ignored)
     (,(function
        (lambda (limit)
          (when scheme-module-names-rx
            (scheme-fontify-definitions limit scheme-module-names-rx
             (function scheme-fontify-module-name)
             '(font-lock-variable-name-face)))
          nil))
      . ignored)))
  "Highlighting specification level 1 for Scheme mode.  Fontify
keywords, variable and function names, module names, and
`#!<identifier>' special comments.")

(defconst scheme-font-lock-keywords-2
  (append scheme-font-lock-keywords-1
   `((,(function (lambda (limit)
                   (scheme-re-search-forward/goto-end-of
                    scheme-boolean-rx limit 1)))
      1 scheme-boolean-face)
     (,(function (lambda (limit)
                   (scheme-re-search-forward/goto-end-of
                    scheme-character-rx limit 1)))
      1 scheme-character-face)
     (,(function (lambda (limit)
                   (and (scheme-re-search-forward/goto-end-of
                         scheme-number-rx limit 1)
                        (progn (set-match-data (list (or (match-end 2)
                                                         (match-beginning 1))
                                                     (match-end 1)))
                               t))))
      . scheme-number-face)
     (,(rx ?# (or ?' ?` (seq ?, (? ?@)))) . scheme-template-face)
     (,(rx (or ?' ?` (seq ?, (? ?@)))) . scheme-quote-face)
     ;; TODO?: Highlight import and export forms contents specially, only when
     ;;        for library or top-level program.
     ;; TODO?: Highlight other binding sites with `font-lock-variable-face'.
     ))
  "Highlighting specification level 2 for Scheme mode.  In
addition to level 1, fontify quote and template prefixes,
booleans, characters, and numbers.")

(defconst scheme-font-lock-keywords-3
  ;; TODO?: Do escape highlighting here, using override? Can't do strings here
  ;;        because they might be multi-line? Can't do strings here because it
  ;;        might match between two strings.
  (append scheme-font-lock-keywords-2
   `((,(function
        (lambda (limit)
          (and (re-search-forward scheme-paren-rx limit t)
               (or (not (match-beginning 1))
                   (progn (goto-char (match-end 1))
                          (set-match-data (list (match-beginning 1) (match-end 1)))
                          t)))))
      . scheme-paren-face)))
  "Highlighting specification level 3 for Scheme mode.  In
addition to level 2, fontify parentheses and other related
lexemes.")

;(defconst scheme-font-lock-keywords-4
;  (append scheme-font-lock-keywords-3
;   `((,(function (lambda (limit)
;                   (let (found)
;                     (while (and (not found)
;                                 (scheme-re-search-forward/goto-end-of
;                                  scheme-identifier-rx limit 2))
;                       (when (match-beginning 1)
;                         (setq found t)))
;                     found)))
;      2 scheme-identifier-face)
;     (,(function (lambda (limit)
;                   (set-match-data (list (point) limit))
;                   (goto-char limit)))
;      0 scheme-invalid-face keep)))
;  "Highlighting specification level 4 for Scheme mode.  In
;addition to level 3, fontify identifiers, and then everything
;left as invalid.  This slowed down re-highlighting and typing
;noticeably.")

;; Emacs by default does its `font-lock-extend-region-multiline' which I think
;; works reliably for the multi-line highlightings this mode implements.  If it
;; turns out it doesn't work reliably, the below will need to be implemented.
; (defun scheme-font-lock-extend-region ()
;   )

;(defun scheme-font-lock-syntactic-face (state)
;  TODO: Highlight string (and symbol?) escapes.  Must set text's face property.
;  )

;;;; TODO: name this section

;(defvar scheme-imenu-generic-expression
;  ...)

;;;; Scheme Mode Initialization

;;;###autoload
(define-derived-mode scheme-mode fundamental-mode "Scheme"
  "A major mode for editing Scheme source-code.
Supports R6RS lexical syntax,
and more customization than old Scheme mode."
  :syntax-table scheme-mode-syntax-table
  (scheme-mode-initialize))

(defun scheme-mode-initialize ()
  "Called by `scheme-mode' and `inferior-scheme-mode'."
  (set (make-local-variable 'font-lock-defaults)
       '((scheme-font-lock-keywords scheme-font-lock-keywords-1
          scheme-font-lock-keywords-2 scheme-font-lock-keywords-3)
         nil nil nil nil
         (font-lock-unfontify-buffer-function . scheme-unfontify-buffer)
         (font-lock-syntactic-keywords . scheme-font-lock-syntactic-keywords)
         ;(font-lock-extra-managed-props syntax-table) ;; Font Lock knows
         (font-lock-mark-block-function . mark-defun)
         ;;; Setting this makes the Emacs font-lock logic notice when
         ;;; a keywords matcher indicates a match which extends beyond the
         ;;; initial region it was looking at, and it then highlights the
         ;;; entire multi-line form and automatically adds the
         ;;; `font-lock-multiline' text property to the match's range so that
         ;;; future changes to any point in the range will cause
         ;;; `font-lock-extend-region-multiline' to extend the initial region
         ;;; of the calculation of the new fontification to encompass the
         ;;; entire multi-line form so that it will all be taken into account.
         (font-lock-multiline . t)))
  ;; I don't think this will be necessary.
  ;(add-to-list font-lock-extend-region-functions
  ;             scheme-font-lock-extend-region
  ;             'append)

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  (set (make-local-variable 'beginning-of-defun-function)
       'scheme-beginning-of-top-level-form)

  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-column) 40)
  ;; Setting comment-add > 0 is necessary for `comment-indent-default' to work.
  (set (make-local-variable 'comment-add) 1)
  ;(set (make-local-variable 'comment-start-skip) ";+\\s-*")  ;; Anyone want this?

  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'scheme-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'scheme-indent)

  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  (set (make-local-variable 'adaptive-fill-mode) nil)

  (set (make-local-variable 'outline-regexp) scheme-outline-begin-rx)
  (set (make-local-variable 'outline-heading-end-regexp) scheme-outline-end-rx)

  ;TODO(set (make-local-variable 'imenu-generic-expression)
  ;     scheme-imenu-generic-expression)

  (add-hook 'after-change-functions 'scheme-after-change nil t)
  ;; In case font-lock-mode does not get turned on, correct the syntax of the
  ;; whole buffer.  I wish this could know it doesn't need to happen if
  ;; font-lock-mode will be turned on, but I couldn't figure out how that's
  ;; possible.  It doesn't take that long, so no big deal.
  (scheme-correct-syntax (point-min) (point-max)))

;;;; Forms Awareness

(defvar scheme-top-level-enclosing-form-names-rx nil)

(defcustom scheme-top-level-enclosing-form-names
  '("library")
  "List of regular expressions which match names of forms that
can enclose top-level forms.  They do not need to ensure they
match only between open-parenthesis and end of symbol, that is
done automatically.  Used by
`scheme-beginning-of-top-level-form'."
  :group 'scheme
  :type '(repeat regexp)
  :set (function
        (lambda (symbol value)
          (setq scheme-top-level-enclosing-form-names-rx
                (and (> (length value) 0)
                     (rx-to-string
                      `(seq (char ?\( ?\[)
                            (or . ,(mapcar (function (lambda (re) `(regexp ,re)))
                                           value))
                            (regexp ,scheme-end-delimiter-rx)))))
          (set-default symbol value))))

(defun scheme-beginning-of-top-level-form (&optional arg)
  "Move point backward to the beginning of the ARGth top-level
form relative to point before the move.  Top-level forms either
have no enclosing form or are enclosed in a form
`scheme-top-level-enclosing-form-names' recognizes (\"library\"
forms are recognized by default).  If ARG is negative, move
forward to the beginning of the next top-level form, instead.  If
the beginning of the ARGth top-level form was reached, return t;
else, move as far as possible and return nil."
  (unless arg (setq arg 1))
  (if (= 0 arg)
      t
    (let ((ppss (syntax-ppss)))
      ;; Back out of comment or string if point is in one.
      (when (nth 8 ppss)
        (goto-char (nth 8 ppss))))
    (let* ((initial (point))
           (p (save-match-data
                (let (found prev (p initial))
                  (while (and (not found)
                              (progn (setq prev p)
                                     (condition-case err
                                         (progn (backward-up-list) t)
                                       (scan-error nil))))
                    (setq p (point))
                    (if (and scheme-top-level-enclosing-form-names-rx
                             (looking-at scheme-top-level-enclosing-form-names-rx))
                        (setq found t)
                      (goto-char p)))
                  prev))))
      (goto-char p)
      (cond ((> arg 0)
             (unless (= p initial) ;; when =, was already at top-level
               (setq arg (1- arg)))
             (condition-case err
                 (while (and arg (> arg 0))
                   (let ((x (scan-sexps (point) -1)))
                     (goto-char (or x (point-min)))
                     (setq arg (and x (1- arg)))))
               (scan-error (setq arg nil))))
            ((< arg 0)
             (condition-case err
                 (progn (forward-sexp)
                        (backward-sexp)
                        (unless (> (point) p)
                          (forward-sexp))
                        (while (and arg (< arg 0))
                          (let ((x (scan-sexps (point) 1)))
                            (goto-char (or x (point-max)))
                            (setq arg (and x (1+ arg))))))
               (scan-error (setq arg nil)))
             (when (equal 0 arg)
               (backward-sexp))))
      (equal 0 arg))))

;;;; Forms Indentation

;; The term "distinguished subforms" means the subforms to indent specially.
;; Indentation specifications specify the number of distinguished subforms that
;; follow the first subform.  Distinguished subforms can all be on the first
;; line, or those on following lines are indented twice the normal indentation
;; of a body, and non-distinguished subforms are indented as a body.  E.g., if
;; `foo' is specified to have 2 distinguished subforms:
;;
;; (foo bar zab blah)
;;
;; (foo bar zab
;;   blah)
;;
;; (foo bar
;;     zab
;;   blah)
;;
;; (foo
;;     bar
;;     zab
;;   blah)

(defconst scheme-indent-specs-type
  '(repeat :tag "Indentation specifications"
           (choice (list (choice :tag "Form name" symbol regexp)
                         (choice (integer :tag "Distinguished subforms")
                                 (symbol :tag "Delegate function")))
                   (symbol :tag "Group reference"))))

(defcustom scheme-default-indent-specs
  '((and 0)
    (begin 0)
    (call-with-bytevector-output-port 0)
    (call-with-current-continuation 0)
    (call-with-input-file 1)
    (call-with-output-file 1)
    (call-with-port 1)
    (call-with-string-output-port 0)
    (call-with-values 0)
    (call/cc 0)
    (case 1)
    (case-lambda 0)
    (cond 0)
    (define 1)
    (define-condition-type 2)
    (define-record-type 1)
    (define-syntax 1)
    (define-values 1)
    (delay 0)
    (do 2)
    (dynamic-wind 0)
    (export 0)
    (guard 1)
    (identifier-syntax 0)
    (import 0)
    (lambda 1)
    (let scheme-let-indent)
    (let* 1)
    (let*-values 1)
    (let-syntax 1)
    (let-values 1)
    (letrec 1)
    (letrec* 1)
    (letrec-syntax 1)
    (library 1)
    (or 0)
    (raise 0)
    (raise-continuable 0)
    (syntax-case 2)
    (syntax-rules 1)
    (unless 1)
    (values 0)
    (when 1)
    (with-exception-handler 0)
    (with-input-from-file 1)
    (with-output-to-file 1)
    (with-syntax 1))
  "Indentation specifications used by all Scheme buffers.
The type of the value must be:

    <indent-specs> : (<elt> ...)
    <elt> : (<form-desc> <spec>) | <group-name-symbol>
    <form-desc> : <symbol> | <regexp-string>
    <spec> : <non-negative-integer> | <function-name-symbol>

Where a <form-desc> matches the name of a Scheme form, a <spec>
is the number of distinguished subforms or the name of a function
to delegate to, and a <group-name-symbol> is a key in
`scheme-indent-spec-groups'."
  :group 'scheme
  :type scheme-indent-specs-type)

(defvar scheme-indent-specs nil
  "Indentation specifications used in addition to, and with
greater precedence than, `scheme-default-indent-specs'.  Intended
to be used as a file-local variable to customize the indentation
per file.  The type of the value must be the same as for
`scheme-default-indent-specs'.")

(defcustom scheme-safe-indent-funcs '(scheme-let-indent)
  "List of function names which are safe to be automatically set
in file-local `scheme-indent-specs'."
  :group 'scheme
  :type '(repeat symbol))

(defun scheme-indent-specs-safe-p (value)
  "This predicate checks that the type of the value of
`scheme-indent-specs' is correct and safe.  The type is described
in the documentation for `scheme-default-indent-specs'.
Permitted function names are specified by
`scheme-safe-indent-funcs'."
  (and (listp value)
       (let ((result t))
         (dolist (elt value result)
           (unless (or (and (listp elt)
                            (= 2 (length elt))
                            (or (symbolp (car elt))
                                (stringp (car elt)))
                            (or (and (integerp (cadr elt))
                                     (<= 0 (cadr elt)))
                                (memq (cadr elt)
                                      scheme-safe-indent-funcs)))
                       (symbolp elt))
             (setq result nil))))))

(put 'scheme-indent-specs 'safe-local-variable 'scheme-indent-specs-safe-p)

(defcustom scheme-indent-spec-groups nil
  "Association-list of indentation-specification groups.  The
type of keys must be symbol, and the type of values must be the
same as for `scheme-default-indent-specs'.  Groups defined in
this variable may be referenced in `scheme-default-indent-specs'
or `scheme-indent-specs'."
  :group 'scheme
  :type `(alist :key-type (symbol :tag "Group name")
                :value-type ,scheme-indent-specs-type))

(defun scheme-lookup-indent-spec (form-name)
  "Given the name of a Scheme form (symbol or string), look-up in
the current values of `scheme-indent-specs' and
`scheme-default-indent-specs' how to indent the form."
  (let ((sym (if (stringp form-name) (intern-soft form-name) form-name))
        (str (if (symbolp form-name) (symbol-name form-name) form-name))
        (specs (append scheme-indent-specs scheme-default-indent-specs))
        found
        result)
    (while (and (not found) (consp specs))
      (let ((elt (car specs)))
        (cond ((consp elt)
               (let ((desc (car elt)))
                 (if (or (and (symbolp desc) (eq desc sym))
                         (and (stringp desc)
                              (string-match (concat "\\`" desc "\\'") str)))
                     (progn (setq result (cadr elt))
                            (setq found t))
                   (setq specs (cdr specs)))))
              ((symbolp elt)
               (let ((ap (assq elt scheme-indent-spec-groups)))
                 (setq specs (append (if ap (cdr ap)) (cdr specs))))))))
    result))

(defun scheme-indent (indent-point state)
  "Copied from `lisp-indent-function', but with uses of
`scheme-lookup-indent-spec' to decide how forms should be
indented, and the arguments to a custom indentation function are
different."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((spec
             (scheme-lookup-indent-spec
              (buffer-substring (point) (progn (forward-sexp 1) (point))))))
        (cond ((integerp spec)
               (lisp-indent-specform spec state indent-point normal-indent))
              (spec
               (funcall spec state indent-point normal-indent)))))))

(defun scheme-let-indent (state indent-point normal-indent)
  "Determine if a \"let\" form is named or not, and indent
accordingly.  Point is assumed to be between the first \"let\"
subform and the next."
  (forward-sexp)
  (backward-sexp)
  (lisp-indent-specform
   (if (and (looking-at scheme-identifier-rx)
            (not (match-beginning 1)))
       2
     1)
   state indent-point normal-indent))

(defun scheme-indent-line (&optional whole-exp)
  "Calls `lisp-indent-line' with
`scheme-top-level-enclosing-form-names-rx' dynamically bound to
nil so that the call of `beginning-of-defun' (which uses
`scheme-beginning-of-top-level-form') from `lisp-indent-line'
works correctly for when point is in a nested top-level-enclosing
form."
  (interactive "P")
  (let ((scheme-top-level-enclosing-form-names-rx nil))
    (lisp-indent-line whole-exp)))

;;;; Faces for Syntax Highlighting

(defconst scheme-paren-face 'scheme-paren-face)
(defface scheme-paren-face
  '((((class color) (background light)) (:foreground "gold4")) ;; good default?
    (((class color) (background dark))  (:foreground "#C4AC27")) ;; good default?
    (((class grayscale))                (:foreground "gray"))
    (t                                  ()))
  "Face for parentheses, brackets, prefixes like `#', and final
cdr's `.'."
  :group 'scheme-faces)

(defconst scheme-quote-face 'scheme-quote-face)
(defface scheme-quote-face
  '((t (:inherit font-lock-constant-face)))
  "Face for `'', ``', `,', `,@'."
  :group 'scheme-faces)

(defconst scheme-template-face 'scheme-template-face)
(defface scheme-template-face
  '((t (:inherit scheme-quote-face)))
  "Face for `#'', `#`', `#,', and `#,@'."
  :group 'scheme-faces)

(defconst scheme-boolean-face 'scheme-boolean-face)
(defface scheme-boolean-face
  '((t (:inherit font-lock-constant-face)))
  "Face for boolean literals."
  :group 'scheme-faces)

(defconst scheme-number-face 'scheme-number-face)
(defface scheme-number-face
  '((t (:inherit font-lock-constant-face)))
  "Face for number literals."
  :group 'scheme-faces)

(defconst scheme-character-face 'scheme-character-face)
(defface scheme-character-face
  '((t (:inherit font-lock-constant-face)))
  "Face for character literals."
  :group 'scheme-faces)

(defconst scheme-special-comment-face 'scheme-special-comment-face)
(defface scheme-special-comment-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for `#!' special comments.  E.g., `#!r6rs'."
  :group 'scheme-faces)

;(defconst scheme-identifier-face 'scheme-identifier-face)
;(defface scheme-identifier-face
;  '((t (:inherit default)))
;  "Face for identifiers."
;  :group 'scheme-faces)
;
;(defconst scheme-invalid-face 'scheme-invalid-face)
;(defface scheme-invalid-face
;  '((t (:inherit font-lock-warning-face)))
;  "Face for invalid lexical syntax."
;  :group 'scheme-faces)

(defconst scheme-outline-begin-face 'scheme-outline-begin-face)
(defface scheme-outline-begin-face
  '((((class color) (background light))
     (:foreground "#a0ffff":background "#c0ffff"))
    (((class color) (background dark))
     (:foreground "cyan2" :background "cyan4"))
    (t (:slant italic)))
  "Face for outline comments beginnings."
  :group 'scheme-faces)

(defconst scheme-outline-rest-face 'scheme-outline-rest-face)
(defface scheme-outline-rest-face
  '((((class color) (background light))
     (:foreground "cyan4" :background "#c0ffff"))
    (((class color) (background dark))
     (:foreground "white" :background "cyan4"))
    (t (:slant italic)))
  "Face for the rest of outline comments after the beginning."
  :group 'scheme-faces)


;; TODO: Move DSSSL mode stuff from old scheme.el into new dsssl.el.

;; TODO?: Aliases for old scheme.el bindings?

;; TODO: If this library gets included in Emacs, add these extensions to the
;;       definition of `auto-mode-alist' instead of adding them here.
(setq auto-mode-alist
      (cons '("\\.\\(s6l\\|s6p\\|sls\\|sps\\|scm\\|ss\\)\\'"
              . scheme-mode)
            auto-mode-alist))

(message "Using Derick's Scheme mode library.")

(provide 'scheme)
