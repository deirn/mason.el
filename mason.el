;;; mason.el --- Mason                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dimas Firmansyah

;; Author: Dimas Firmansyah <deirn@bai.lol>
;; Version: 1.0.0
;; Homepage: https://github.com/deirn/mason.el
;; Package-Requires: ((emacs "30.1") (yaml "1.2.0"))
;; Keywords: tools lsp installer
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Installer for LSP servers, DAP servers, linters, and formatters.
;; Based on mason.nvim.  https://github.com/mason-org/mason.nvim

;;; Code:

(require 'cl-macs)
(require 'yaml)

(defgroup mason nil
  "Installer for LSP servers, DAP servers, linters, and formatters.
Based on mason.nvim.  https://github.com/mason-org/mason.nvim")

(defcustom mason-registry-repo "https://github.com/mason-org/mason-registry.git"
  "The mason registry repository."
  :type 'string :group 'mason)

(defcustom mason-registry-dir (expand-file-name "mason/registry" user-emacs-directory)
  "Directory for the mason registry repository."
  :type 'directory :group 'mason)

(defcustom mason-install-dir (expand-file-name "mason/packages" user-emacs-directory)
  "Directory for where the packages get installed."
  :type 'directory :group 'mason)

(defcustom mason-bin-dir (expand-file-name "mason/bin" user-emacs-directory)
  "Directory for where package executables in."
  :type 'directory :group 'mason)



(defconst mason-buffer "*mason*")
(defun mason-buffer ()
  "Get mason buffer."
  (get-buffer-create mason-buffer))

(defun mason--msg (format &rest args)
  "Message with prefix.  See `message' FORMAT ARGS."
  (let ((formatted (apply #'format-message format args)))
    (message formatted)
    (with-current-buffer (mason-buffer)
      (read-only-mode -1)
      (goto-char (point-max))
      (insert formatted "\n")
      (read-only-mode 1))))

(defun mason--process (exec &optional callback)
  "Run process EXEC asynchronously, with optional CALLBACK."
  (let ((msg (mapconcat #'identity exec " "))
        (buffer (generate-new-buffer "*mason async*")))
    (mason--msg "async: %s" msg)
    (with-current-buffer buffer (read-only-mode 1))
    (make-process
     :name "mason"
     :buffer buffer
     :command exec
     :sentinel
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (mason--msg "finish: %s" msg)
         (with-current-buffer (mason-buffer)
           (read-only-mode -1)
           (goto-char (point-max))
           (insert "----------------------------------------------------------------\n")
           (insert-buffer-substring buffer)
           (insert "----------------------------------------------------------------\n")
           (read-only-mode 1))
         (kill-buffer buffer)
         (when (and (functionp callback)
                    (zerop (process-exit-status proc)))
           (funcall callback proc event)))))))

(defun mason--dir-empty-p (dir)
  "Return t if DIR exists and contains no non-dot files."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp))))

(defun mason--parse-yaml (path)
  "Parse mason package spec from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (yaml-parse-string (buffer-string))))

(defconst mason--id-regexp
  ;; pkg:namespace[/name]@version
  (concat "^pkg:"
          "\\([A-Za-z0-9_-]+\\)"  ; type
          "/"
          "\\([A-Za-z0-9_/-]+\\)" ; namespace/name
          "@"
          "\\([A-Za-z0-9_.-]+\\)" ; version
          "$"))

(defun mason--parse-id (id)
  "Parse a source ID."
  (when (string-match mason--id-regexp id)
    `( :type    ,(match-string 1 id)
       :package ,(match-string 2 id)
       :version ,(match-string 3 id))))

(defconst mason--bin-regexp
  ;; [type:]path/to/bin
  (concat "^"
          "\\(" ; optional type
          "\\([A-Za-z0-9_-]+\\)"
          ":"
          "\\)?"
          "\\(" ; path/to/bin
          "[A-Za-z0-9_-]" ; disallow absolute path
          "[A-Za-z0-9_/-]+"
          "\\)"
          "$"))

(defun mason--parse-bin (bin)
  "Parse a BIN."
  (when (string-match mason--bin-regexp bin)
    `( :type ,(or (match-string 2 bin) "path")
       :path ,(match-string 3 bin))))



(defmacro mason--source! (type &rest cmds)
  "Define a mason source resolver for TYPE.
CMDS is vararg of command list, as specified in :command option
for `make-process'.

Inside each CMD, one can reference NAME, PACKAGE, VERSION, EXTRA, and PREFIX.

NAME is the name of the mason entry.
PACKAGE and VERSION is the name and version of package to install.
EXTRA is a plist containing additional query for the package.
PREFIX is the directory where the package is expected to be installed."
  (declare (indent defun))
  (let (cmd-nest)
    (dolist (cmd (nreverse cmds) cmd-nest)
      (setq cmd-nest
            `(mason--process
              ,cmd
              (lambda (&rest _)
                ,(or cmd-nest '(funcall next))))))
    `(defun ,(intern (concat "mason--source-" (symbol-name type))) (name package version extra next)
       (let ((prefix (expand-file-name name mason-install-dir)))
         ,cmd-nest))))

(mason--source! cargo
  (list "cargo" "install"
        "--root" prefix
        (concat package "@" version)))

(mason--source! pypi
  (list "python" "-m" "venv" prefix)
  (list "pip"
        "--python" (expand-file-name "bin/python" prefix)
        "install"
        "--prefix" prefix
        (concat package "==" version)))

(mason--source! npm
  (list "npm" "install" "-g"
        "--prefix" prefix
        (concat package "@" version)))



(defmacro mason--bin! (type &rest body)
  "Define a mason binary resolver for TYPE.
BODY is `progn' body.

Inside BODY, one can reference PATH and PREFIX.

PATH is the relative path of the binary.
PREFIX is where the package should've been installed."
  (declare (indent defun))
  `(defun ,(intern (concat "mason--bin-" (symbol-name type))) (name path)
     (let ((prefix (expand-file-name name mason-install-dir)))
       ,@body)))

(mason--bin! path (expand-file-name path prefix))
(mason--bin! cargo (expand-file-name (concat "bin/" path) prefix))
(mason--bin! pypi (expand-file-name (concat "bin/" path) prefix))
(mason--bin! npm (expand-file-name (concat "bin/" path) prefix))



(defvar mason--ensured nil)

;;;###autoload
(defun mason-ensure ()
  "Ensure mason is setup."
  (setenv "PATH" (concat mason-bin-dir ":" (getenv "PATH")))
  (add-to-list 'exec-path mason-bin-dir)
  (let ((dir (expand-file-name mason-registry-dir)))
    (if (not (or (not (file-exists-p dir))
                 (mason--dir-empty-p dir)))
        (setq mason--ensured t)
      (make-directory dir t)
      (mason--process
       (list "github" "clone" "--depth" "1" mason-registry-repo dir)
       (lambda (&rest _)
         (setq mason--ensured t))))))

(defun mason--get-package-list ()
  "Get list of mason packages."
  (when mason--ensured
    (let ((dir (expand-file-name "packages" mason-registry-dir)))
      (directory-files dir nil directory-files-no-dot-files-regexp))))

;;;###autoload
(cl-defun mason-install (package)
  "Install a Mason PACKAGE."
  (interactive (list (when mason--ensured
                       (completing-read "Mason: " (mason--get-package-list) nil t))))
  (unless package (user-error "Call `mason-ensure' on your init.el"))
  (unless mason--ensured (user-error "Mason is not yet ready"))
  (let* ((path (expand-file-name (concat "packages/" package "/package.yaml") mason-registry-dir))
         (spec (mason--parse-yaml path))
         (name (gethash 'name spec))
         ;; source
         (source (gethash 'source spec))
         (source-id-raw (gethash 'id source))
         (source-id (mason--parse-id source-id-raw))
         (source-supported-platforms (gethash 'supported_platforms source))
         ;; bin
         (bin (gethash 'bin spec)))
    (unless source-id
      (mason--msg "Unsupported source id %s" source-id-raw)
      (cl-return-from mason-install))
    (let* ((type (plist-get source-id :type))
           (package (plist-get source-id :package))
           (version (plist-get source-id :version))
           (source-fn (intern (concat "mason--source-" type))))
      (when (not (fboundp source-fn))
        (mason--msg "Unsupported source type %s in id %s" type source-id-raw)
        (cl-return-from mason-install))
      (funcall
       source-fn name package version source-id
       (lambda ()
         (maphash (lambda (key val-raw)
                    (cl-block nil
                      (let* ((val (mason--parse-bin val-raw))
                             (bin-type (plist-get val :type))
                             (bin-path (plist-get val :path))
                             (bin-fn (intern (concat "mason--bin-" bin-type))))
                        (when (or (null val) (not (fboundp bin-fn)))
                          (mason--msg "Unsupported binary %s" val-raw)
                          (cl-return))
                        (let ((bin-target (funcall bin-fn name bin-path))
                              (bin-link (expand-file-name (symbol-name key) mason-bin-dir)))
                          (make-directory mason-bin-dir t)
                          (when (file-exists-p bin-link)
                            (delete-file bin-link))
                          (make-symbolic-link bin-target bin-link)
                          (mason--msg "symlinked %s -> %s" bin-link bin-target)))))
                  bin))))))

(provide 'mason)
;;; mason.el ends here
