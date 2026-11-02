;;; daml-mode.el --- Major mode for daml             -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Bártfai Tamás
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Bártfai Tamás <tamas@bartfai.dev>
;; Assisted-by: Claude:claude-opus-5-5

;; Package-Requires: ((emacs "27.1") (haskell-mode "16.1"))
;; URL: https://github.com/bartfaitamas/daml-mode
;; Package-Version: 2.0

;;; Commentary:

;; Provides a major mode for editing daml source code.  The Daml
;; language server works with eglot out of the box; for lsp-mode
;; install daml-lsp.

;;; Code:
(require 'haskell-mode)
(require 'haskell-indentation)

(defgroup daml nil
  "Major mode for editing Daml code."
  :group 'languages)

(defcustom daml-font-lock-keywords
  '("template" "with" "ensure" "signatory" "observer"
    "choice" "controller" "nonconsuming" "preconsuming" "postconsuming"
    "interface" "viewtype" "requires" "for"
    "key" "maintainer" "exception" "message")
  "Identifiers treated as reserved keywords in daml.
They are not keywords for Haskell."
  :group 'daml
  :type '(repeat string))

(defcustom daml-server-command nil
  "Program and arguments that start the Daml language server.
When nil, use \"dpm damlc multi-ide\", with dpm found on variable
`exec-path'."
  :group 'daml
  :type '(choice (const :tag "Use dpm" nil)
                 (repeat string)))

(defun daml-server-command ()
  "Return the command for starting the Daml language server.
See the variable `daml-server-command'."
  (cond (daml-server-command)
        ((executable-find "dpm") '("dpm" "damlc" "multi-ide"))
        (t (user-error "Cannot find dpm on exec-path"))))

(defun daml--font-lock-keywords ()
  "Generate font lock keywords for daml.
Mostly haskell, with some daml specific syntax."
  (append (haskell-font-lock-keywords)
          '(("\\_<\\(submit\\|submitMustFail\\|fetch\\|exercise\\|create\\)\\_>"
             0 font-lock-function-name-face))))

(define-derived-mode daml-mode haskell-mode "daml" "Major mode for daml."
  ;; haskell-mode consults this while fontifying; keep the Daml
  ;; keywords local, so Haskell buffers are unaffected.
  (setq-local haskell-font-lock-keywords
              (append (default-value 'haskell-font-lock-keywords)
                      daml-font-lock-keywords))
  (setq-local font-lock-defaults
              '((daml--font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . haskell-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition haskell-type)))))

;;;; Indentation

;; `haskell-indentation-mode', which `haskell-mode' turns on,
;; parses the code as Haskell to find the columns TAB cycles through
;; and RET picks from.  It does not know Daml's `with' blocks, and
;; finds nothing at all inside `interface instance ... for ... where'.
;; daml-mode adds the missing columns.

(defconst daml--opener-regexp
  (rx (or (seq symbol-start (or "where" "with" "do" "of" "let" "in") symbol-end)
          (seq (or bos blank) (or "=" "->" "<-" "$"))
          (in "([{"))
      eos)
  "Matches code ending in something that the next line nests under.")

(defconst daml--declaration-regexp
  (rx bos (or "template" "data" "exception"
              (seq (opt (or "nonconsuming" "preconsuming" "postconsuming") (+ blank))
                   "choice"))
      symbol-end)
  "Matches code starting a declaration.
When such a line ends in `with', the fields go at +4, so that
`where' or `controller' can follow at +2.")

(defun daml--line-code ()
  "Return the code on the current line, without comments and indentation."
  (save-excursion
    (back-to-indentation)
    (if (nth 4 (syntax-ppss))
        ""
      (let* ((start (point))
             (eol-ppss (save-excursion (end-of-line) (syntax-ppss)))
             (end (if (nth 4 eol-ppss) (nth 8 eol-ppss) (line-end-position))))
        (string-trim-right (buffer-substring-no-properties start (max start end)))))))

(defun daml--statement-column ()
  "Return the starting column of the statement on the current line.
That is the indentation, or after a block keyword when code follows
it on the line, as in \"do create this with\" or \"let x = do\"."
  (save-excursion
    (let ((column (current-indentation))
          (eol (line-end-position)))
      (beginning-of-line)
      (while (re-search-forward
              "\\_<\\(?:do\\|let\\|where\\|of\\)\\_>[ \t]+\\([^ \t\n]\\)" eol t)
        ;; `syntax-ppss' moves point to its argument.
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (setq column (save-excursion
                         (goto-char (match-beginning 1))
                         (current-column)))))
      column)))

(defun daml--previous-code-lines ()
  "Return the code lines before the current one, nearest first.
Each is (INDENTATION STATEMENT-COLUMN CODE).  Stops after the first
line at column 0, which starts the enclosing top-level declaration."
  (save-excursion
    (let (lines (done nil) (budget 1000))
      (while (and (not done) (> budget 0) (zerop (forward-line -1)))
        (setq budget (1- budget))
        (let ((code (daml--line-code)))
          (unless (string-empty-p code)
            (push (list (current-indentation) (daml--statement-column) code) lines)
            (setq done (zerop (current-indentation))))))
      (nreverse lines))))

(defvar daml--newline nil
  "Non-nil while RET indents a new line.")

(defun daml--newline-and-indent (newline-and-indent &rest args)
  "Call NEWLINE-AND-INDENT with ARGS, telling the indentation it is for RET."
  (let ((daml--newline t))
    (apply newline-and-indent args)))

(defun daml--indentations (haskell)
  "Return the columns for TAB to cycle through on the current line.
HASKELL is what `haskell-indentation-find-indentations' found."
  (let* ((lines (daml--previous-code-lines))
         (min-indent most-positive-fixnum)
         below field-columns ancestors)
    ;; Walk up through the enclosing blocks.
    (dolist (line lines)
      (pcase-let ((`(,indent ,anchor ,code) line))
        ;; A `with' block holds every line between it and the current
        ;; one; its fields line up with its first line.
        (when (and below (string-match-p "\\_<with\\'" code) (< anchor min-indent))
          (push (car below) field-columns))
        (when (< indent min-indent)
          (push indent ancestors)
          (setq min-indent indent))
        (setq below line)))
    (let ((columns (append field-columns haskell)))
      ;; haskell-indentation could not parse this part.
      (unless haskell
        (setq columns (append ancestors columns)))
      (pcase (car lines)
        (`(,indent ,anchor ,code)
         (let* ((with (string-match-p "\\_<with\\'" code))
                (opener (and (string-match-p daml--opener-regexp code)
                             (not (string-match-p "\\`module\\_>" code))))
                (nested (+ anchor
                           (if (and with (string-match-p daml--declaration-regexp code))
                               4
                             2))))
           (cond
            ;; haskell-indentation knows nothing here: the next line
            ;; nests under the statement.
            ((or with (and opener (null haskell)))
             (setq columns (cons nested
                                 (seq-filter (lambda (c) (>= c nested)) columns))))
            ;; haskell-indentation knows where the block may start, as
            ;; the statement may have begun on an earlier line; offer
            ;; the nearest column only when it, too, nests deeper.
            (opener
             (when (seq-every-p (lambda (c) (> c indent)) haskell)
               (push nested columns)))
            ;; More stops for TAB: stay level with the previous line, or
            ;; continue it.  Not for RET, which takes the first stop at
            ;; or after the previous line's, where haskell-indentation's
            ;; choice is better.
            ((not daml--newline)
             (setq columns (append (list indent nested) columns)))))))
      (sort (delete-dups columns) #'<))))

(defun daml--find-indentations (indentations)
  "Add Daml's columns to INDENTATIONS in `daml-mode' buffers.
Filters the return value of `haskell-indentation-find-indentations'."
  (let ((ppss (syntax-ppss)))
    (if (and (derived-mode-p 'daml-mode)
             (not (nth 3 ppss))
             (not (nth 4 ppss)))
        (daml--indentations indentations)
      indentations)))

(advice-add 'haskell-indentation-find-indentations
            :filter-return #'daml--find-indentations)
(advice-add 'haskell-indentation-newline-and-indent
            :around #'daml--newline-and-indent)

(defun daml-mode-unload-function ()
  "Remove the advice on `haskell-indentation', for `unload-feature'."
  (advice-remove 'haskell-indentation-find-indentations
                 #'daml--find-indentations)
  (advice-remove 'haskell-indentation-newline-and-indent
                 #'daml--newline-and-indent)
  nil)

(declare-function interactive-haskell-mode "haskell")

(defun daml--turn-off-ghci ()
  "Turn off `interactive-haskell-mode', as enabled by `haskell-mode-hook'.
Its GHCi commands do not apply to Daml, and its jump to definition
shadows the xref one, which language server clients use.  The
parent mode's hook runs after the mode body, so this has to run
from `daml-mode-hook'."
  (when (bound-and-true-p interactive-haskell-mode)
    (interactive-haskell-mode -1)))

(add-hook 'daml-mode-hook #'daml--turn-off-ghci)

(defun daml--eglot-contact (_interactive &optional _project)
  "Return the eglot contact for the Daml language server."
  (daml-server-command))

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs '(daml-mode . daml--eglot-contact)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.daml\\'" . daml-mode))

(provide 'daml-mode)
;;; daml-mode.el ends here
