;;; daml-lsp-test.el --- Tests for daml-lsp  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run with `make test'.

;;; Code:

(require 'ert)
(require 'daml-lsp)

(defun daml-lsp-test--client ()
  "Return the registered Daml `lsp-mode' client."
  (gethash 'daml-ls lsp-clients))

(ert-deftest daml-lsp-test-server-command-adds-extra-arguments ()
  (let ((daml-server-command '("my-dpm" "damlc" "multi-ide"))
        (daml-lsp-extra-arguments '("--log-level=DEBUG")))
    (should (equal (daml-lsp-server-command)
                   '("my-dpm" "damlc" "multi-ide" "--log-level=DEBUG")))))

(ert-deftest daml-lsp-test-handles-server-notifications ()
  (let ((handlers (lsp--client-notification-handlers (daml-lsp-test--client))))
    (dolist (method '("daml/virtualResource/didChange"
                      "daml/virtualResource/note"
                      "daml/virtualResource/didProgress"
                      "daml/sdkInstallProgress"))
      (should (gethash method handlers)))))

(ert-deftest daml-lsp-test-disables-unsupported-features ()
  ;; The multi-IDE server answers documentHighlight and documentColor
  ;; requests with a "FATAL ERROR".
  (with-temp-buffer
    (daml-mode)
    (should-not lsp-enable-symbol-highlighting)
    (should-not lsp-enable-text-document-color))
  (should (default-value 'lsp-enable-symbol-highlighting))
  (should (default-value 'lsp-enable-text-document-color)))

(ert-deftest daml-lsp-test-closing-results-closes-script ()
  ;; The server sends the URI encoded; the buffer name shows it decoded.
  ;; Closing the buffer must stop the server running the script, so the
  ;; didClose needs the URI as sent, through the source's workspace.
  (let* ((uri "daml://compiler?file=%2Ftmp%2FMain.daml&top-level-decl=setup")
         (notified nil))
    (cl-letf (((symbol-function 'lsp-workspaces) (lambda () '(source-workspace)))
              ((symbol-function 'lsp-notify)
               (lambda (method params)
                 (push (list method
                             (plist-get (plist-get params :textDocument) :uri)
                             lsp--cur-workspace)
                       notified))))
      (daml-lsp--show-resource
       (lsp-make-command :title "Script results"
                         :command "daml.showResource"
                         :arguments (vector "Script: setup" uri)))
      (let ((buffer (get-buffer (daml-lsp--script-result-buffer-name uri))))
        (should buffer)
        (cl-letf (((symbol-function 'lsp-workspaces) (lambda () nil)))
          (kill-buffer buffer))))
    (should (equal (car notified)
                   (list "textDocument/didClose" uri 'source-workspace)))))

(provide 'daml-lsp-test)
;;; daml-lsp-test.el ends here
