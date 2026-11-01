.POSIX:
EMACS = emacs

# Dependencies are installed here, not into the user's own package dir.
DEPS_DIR = .deps
NEEDED_PACKAGES = haskell-mode lsp-mode package-lint

INIT_PACKAGES="(progn \
  (require 'package) \
  (setq package-user-dir (expand-file-name \"${DEPS_DIR}\")) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: clean-elc compile test package-lint

package-lint: package-lint-daml-lsp package-lint-daml-mode

# Warns about `with-eval-after-load', used on purpose to register with
# eglot without depending on it.
package-lint-daml-mode:
	${EMACS} -Q --eval ${INIT_PACKAGES} --eval '(setq package-lint-main-file "daml-mode.el")' -batch -f package-lint-batch-and-exit daml-mode.el

package-lint-daml-lsp:
	${EMACS} -Q --eval ${INIT_PACKAGES} --eval '(setq package-lint-main-file "daml-lsp.el")' -L . -batch -f package-lint-batch-and-exit daml-lsp.el

# Emacs 27 and 28 cannot install lsp-mode; build and test only
# daml-mode there, with eglot from GNU ELPA:
#   make NEEDED_PACKAGES="haskell-mode eglot" compile-daml-mode test-daml-mode
compile: clean-elc compile-daml-mode compile-daml-lsp

compile-daml-mode:
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile daml-mode.el

compile-daml-lsp:
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile daml-lsp.el

test: test-daml-mode test-daml-lsp

test-daml-mode:
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -L test -batch -l test/daml-mode-test.el -f ert-run-tests-batch-and-exit

test-daml-lsp:
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -L test -batch -l test/daml-lsp-test.el -f ert-run-tests-batch-and-exit

clean-elc:
	rm -f *.elc test/*.elc

clean: clean-elc
	rm -rf ${DEPS_DIR}

.PHONY: all compile compile-daml-mode compile-daml-lsp clean-elc clean package-lint package-lint-daml-mode package-lint-daml-lsp test test-daml-mode test-daml-lsp
