;;; daml-mode-test.el --- Tests for daml-mode  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'daml-mode)

(defun daml-mode-test--face-at (mode text word)
  "Fontify TEXT in MODE and return the face of the first WORD in it."
  (with-temp-buffer
    (insert text)
    (funcall mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward-regexp (concat "\\_<" (regexp-quote word) "\\_>"))
    (get-text-property (match-beginning 0) 'face)))

(defun daml-mode-test--keyword-p (mode text word)
  "Non-nil if WORD in TEXT is fontified as a keyword in MODE.
`haskell-mode' uses `haskell-keyword-face', which inherits from
`font-lock-keyword-face'."
  (let ((face (daml-mode-test--face-at mode text word)))
    (and (symbolp face) face
         (or (eq face 'font-lock-keyword-face)
             (eq (face-attribute face :inherit nil t) 'font-lock-keyword-face)))))

(defconst daml-mode-test--template
  "module Main where

template Iou
  with
    issuer : Party
    owner : Party
  where
    signatory issuer
    observer owner
    ensure True

    nonconsuming choice Check : ()
      controller owner
      do pure ()
"
  "A small Daml 3 template.")

(ert-deftest daml-mode-test-highlights-daml-keywords ()
  (dolist (word '("template" "with" "signatory" "observer" "ensure"
                  "nonconsuming" "choice" "controller"))
    (should (daml-mode-test--keyword-p 'daml-mode daml-mode-test--template word))))

(ert-deftest daml-mode-test-highlights-exception-message ()
  (should (daml-mode-test--keyword-p
           'daml-mode
           "module M where\n\nexception E\n  with\n    m : Text\n  where\n    message m\n"
           "message")))

(ert-deftest daml-mode-test-highlights-interface-keywords ()
  (let ((text "module M where\n\ninterface I where\n  viewtype V\n  requires T\n"))
    (dolist (word '("interface" "viewtype" "requires"))
      (should (daml-mode-test--keyword-p 'daml-mode text word)))))

(ert-deftest daml-mode-test-ignores-removed-keywords ()
  ;; `agreement' and `controller ... can' were removed from Daml.
  (let ((text "module M where\n\nf agreement can = agreement\n"))
    (dolist (word '("agreement" "can"))
      (should-not (daml-mode-test--keyword-p 'daml-mode text word)))))

(ert-deftest daml-mode-test-leaves-haskell-mode-alone ()
  ;; Loading daml-mode must not turn Daml keywords on in Haskell buffers.
  (should-not (daml-mode-test--keyword-p
               'haskell-mode "module M where\n\ntemplate = 1\n" "template")))

(ert-deftest daml-mode-test-turns-off-ghci ()
  ;; `haskell-mode-hook' turns on the GHCi REPL mode, which does not
  ;; apply to Daml.
  (with-temp-buffer
    (daml-mode)
    (should-not (bound-and-true-p interactive-haskell-mode))))

(ert-deftest daml-mode-test-m-dot-uses-xref ()
  ;; The GHCi mode binds M-. to its own GHCi/tags jump, which knows
  ;; nothing about Daml; xref is what language server clients use.
  (with-temp-buffer
    (daml-mode)
    (should (eq (key-binding (kbd "M-.")) 'xref-find-definitions))))

;;;; Indentation

(defconst daml-mode-test--indentation-sample
  (expand-file-name "indentation-sample.daml"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Daml code indented the way TAB should be able to indent it.")

(defmacro daml-mode-test--with-sample (&rest body)
  "Run BODY in a `daml-mode' buffer holding the indentation sample."
  (declare (indent 0))
  `(with-temp-buffer
     (insert-file-contents daml-mode-test--indentation-sample)
     (daml-mode)
     ,@body))

(defun daml-mode-test--line-text ()
  "Return the current line, without surrounding whitespace."
  (string-trim (buffer-substring (line-beginning-position) (line-end-position))))

(ert-deftest daml-mode-test-indents-with-haskell-indentation ()
  (daml-mode-test--with-sample
    (should (bound-and-true-p haskell-indentation-mode))))

(ert-deftest daml-mode-test-every-line-is-a-tab-stop ()
  ;; TAB cycles through the offered columns; each line's own column
  ;; must be one of them, or TAB moves correct code.
  (daml-mode-test--with-sample
    (let (misses)
      (goto-char (point-min))
      (while (not (eobp))
        (unless (looking-at-p "^[ \t]*$")
          (let ((col (current-indentation))
                (stops (save-excursion
                         (back-to-indentation)
                         (haskell-indentation-find-indentations))))
            (unless (memq col stops)
              (push (list (line-number-at-pos) col stops (daml-mode-test--line-text))
                    misses))))
        (forward-line 1))
      (should-not (nreverse misses)))))

(ert-deftest daml-mode-test-newline-lands-on-next-line-column ()
  ;; RET after these lines goes where the sample's next line starts.
  (dolist (pattern '("^  with$" "^    issuer : Party$" "^      with$"
                     "do create this with$" "^          with$"
                     "^      view =$" "meta = Meta with$" "^            with$"
                     "createCmd Asset with$" "^      issuer = alice$"
                     "exerciseCmd assetCid Give with$"
                     "^module Sample where$" "^setup = script do$"
                     "^template Install with$" "^data Result = Result with$"
                     "(\\\\p -> do$" "^  -> Party$" "let names = do$"))
    (daml-mode-test--with-sample
      (goto-char (point-min))
      (re-search-forward pattern)
      (let ((expected (save-excursion (forward-line 1) (current-indentation)))
            (line (daml-mode-test--line-text)))
        (end-of-line)
        (haskell-indentation-newline-and-indent)
        (should (equal (list line (current-column))
                       (list line expected)))))))

;;;; Language server command

(defmacro daml-mode-test--with-programs (programs &rest body)
  "Run BODY with only PROGRAMS on variable `exec-path', as empty executables."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "daml-mode-test" t))
          (exec-path (list dir)))
     (unwind-protect
         (progn
           (dolist (program ,programs)
             (let ((file (expand-file-name program dir)))
               (write-region "" nil file)
               (set-file-modes file #o755)))
           ,@body)
       (delete-directory dir t))))

(ert-deftest daml-mode-test-server-command-uses-dpm ()
  (daml-mode-test--with-programs '("dpm")
    (let ((daml-server-command nil))
      (should (equal (daml-server-command) '("dpm" "damlc" "multi-ide"))))))

(ert-deftest daml-mode-test-server-command-without-dpm ()
  ;; The older Daml assistant is not supported.
  (daml-mode-test--with-programs '("daml")
    (let ((daml-server-command nil))
      (should-error (daml-server-command) :type 'user-error))))

(ert-deftest daml-mode-test-server-command-custom ()
  (daml-mode-test--with-programs '("dpm")
    (let ((daml-server-command '("my-damlc" "ide")))
      (should (equal (daml-server-command) '("my-damlc" "ide"))))))

;;;; Eglot

(ert-deftest daml-mode-test-registers-with-eglot ()
  (require 'eglot)
  (let ((contact (alist-get 'daml-mode eglot-server-programs)))
    (should (functionp contact))
    (daml-mode-test--with-programs '("dpm")
      (let ((daml-server-command nil))
        (should (equal (funcall contact nil) '("dpm" "damlc" "multi-ide")))))))

(provide 'daml-mode-test)
;;; daml-mode-test.el ends here
