;;; daml-lsp.el --- LSP client definition for daml   -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Bártfai Tamás
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Bártfai Tamás <tamas@bartfai.dev>
;; Assisted-by: Claude:claude-opus-5-5

;; Package-Requires: ((emacs "29.1") (daml-mode "2.0") (dash "2.18.0") (lsp-mode "7.0"))
;; URL: https://github.com/bartfaitamas/daml-mode
;; Package-Version: 2.0

;;; Commentary:

;; Daml-specific client definition for lsp-mode for interacting with the
;; Daml SDK-provided lsp server.  The server command comes from
;; `daml-server-command' in daml-mode.

;;; Code:

(require 'lsp-mode)
(require 'dash)
(require 'daml-mode)

(defgroup daml-lsp nil
  "Customization group for daml-lsp."
  :group 'tools)

(defcustom daml-lsp-extra-arguments nil
  "Extra arguments appended to the Daml language server command."
  :type '(repeat string)
  :group 'daml-lsp)

(defun daml-lsp-server-command ()
  "Return the Daml language server command, with extra arguments."
  (append (daml-server-command) daml-lsp-extra-arguments))

(defun daml-lsp--disable-unsupported-features ()
  "Turn off `lsp-mode' features the Daml multi-IDE server rejects.
It answers documentHighlight and documentColor requests with an
error."
  (setq-local lsp-enable-symbol-highlighting nil)
  (setq-local lsp-enable-text-document-color nil))

(add-hook 'daml-mode-hook #'daml-lsp--disable-unsupported-features)

(add-to-list 'lsp-language-id-configuration '(daml-mode . "daml"))
(add-hook 'kill-buffer-hook #'daml-lsp--virtualResource-kill-hook)

(defvar daml-lsp--scripts (make-hash-table :test #'equal)
  "Script result buffer names, mapped to (URI . WORKSPACE).
URI is as the server sent it: the buffer name shows it decoded,
which the server does not recognize.  The result buffer has no
workspace of its own to send the notification through.")

(defun daml-lsp--virtualResource-kill-hook ()
  "Run when buffer closed to free up LSP resources."
  (when-let* ((script (gethash (buffer-name) daml-lsp--scripts)))
    (remhash (buffer-name) daml-lsp--scripts)
    (with-lsp-workspace (cdr script)
      (lsp-notify
       "textDocument/didClose"
       (list :textDocument
             (list :uri (car script)
                   :languageId ""
                   :version 0
                   :text ""))))))

(defun daml-lsp--script-result-buffer-name (script-uri)
  "Generate a name for result buffer from running daml script SCRIPT-URI."
  (format "*daml-script %s" (url-unhex-string script-uri)))

(lsp-defun daml-lsp--show-resource ((&Command :arguments?))
  "Execute the show daml script action."
  (interactive)
  (let* ((script-uri (elt arguments? 1))
         (result-buf-name (daml-lsp--script-result-buffer-name script-uri)))
     (puthash result-buf-name (cons script-uri (car (lsp-workspaces)))
              daml-lsp--scripts)
     (lsp-notify
      "textDocument/didOpen"
      (list :textDocument
            (list :uri script-uri
                  :languageId ""
                  :version 0
                  :text "")))

     (if-let* ((result-buf (get-buffer result-buf-name)))
         (with-help-window result-buf
           (print "Try modifying the script to show the results"))
       (with-output-to-temp-buffer result-buf-name
         (print (format "Executing script %s"
                        (url-unhex-string script-uri)))))))

(eval-when-compile
  (lsp-interface (DAMLVirtualResourceChange (:uri :contents) nil)
                 (DAMLVirtualResourceNote (:uri :note) nil)))

(defun daml-lsp--virtualResource-note (_workspace params)
  "Display a virtual resource note with PARAMS.  WORKSPACE is ignored."

  (-let [(&DAMLVirtualResourceNote :uri :note) params]
    (daml-lsp--display-virtualResource uri note)))

(defun daml-lsp--virtualResource-change (_workspace params)
  "Display a virtual resource change with PARAMS.  WORKSPACE is ignored."
  (-let [(&DAMLVirtualResourceChange :uri :contents) params]
    (daml-lsp--display-virtualResource uri contents)))

(defun daml-lsp--display-virtualResource (uri contents)
  "Display virtual resource info for URI and CONTENTS."
  (let* ((result-buffer-name (daml-lsp--script-result-buffer-name uri))
         (dom (with-temp-buffer
                (insert contents)
                (libxml-parse-html-region (point-min) (point-max)))))
    (when (get-buffer result-buffer-name)
      (with-current-buffer (get-buffer-create result-buffer-name)
        (with-help-window result-buffer-name
          (erase-buffer)
          (shr-insert-document dom))))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   #'daml-lsp-server-command
                                   (lambda () (ignore-errors (daml-lsp-server-command))))
                  :major-modes '(daml-mode)
                  :priority -1
                  :multi-root nil
                  :server-id 'daml-ls
                  :action-handlers (lsp-ht ("daml.showResource" #'daml-lsp--show-resource))
                  :notification-handlers
                  (lsp-ht
                   ("daml/virtualResource/didChange" #'daml-lsp--virtualResource-change)
                   ("daml/virtualResource/note" #'daml-lsp--virtualResource-note)
                   ;; Progress reports with no use here; handled so
                   ;; lsp-mode does not warn about them.
                   ("daml/virtualResource/didProgress" #'ignore)
                   ("daml/sdkInstallProgress" #'ignore))))

(provide 'daml-lsp)
;;; daml-lsp.el ends here
