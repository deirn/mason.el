;;; mason.el --- Mason                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dimas Firmansyah

;; Author: Dimas Firmansyah <deirn@bai.lol>
;; Version: 1.0.0
;; Homepage: https://github.com/deirn/mason.el
;; Package-Requires: ((emacs "30.1") (s "1.13.1") (yaml "1.2.0"))
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
(require 'seq)
(require 's)
(require 'yaml)

(defgroup mason nil
  "Installer for LSP servers, DAP servers, linters, and formatters.
Based on mason.nvim.  https://github.com/mason-org/mason.nvim"
  :prefix "mason-"
  :group 'tools)

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

(defcustom mason-target nil
  "The package target architecture to install."
  :type '(choice (const "darwin_arm64")
                 (const "darwin_x64")
                 (const "linux_arm64_gnu")
                 (const "linux_x64_gnu")
                 (const "linux_x64_musl")
                 (const "win_x64"))
  :group 'mason)



(defconst mason-buffer "*mason*")
(defun mason-buffer ()
  "Get mason buffer."
  (or (get-buffer mason-buffer)
      (with-current-buffer (get-buffer-create mason-buffer)
        (read-only-mode 1)
        (current-buffer))))

(defun mason--msg (format &rest args)
  "Message with prefix.  See `message' FORMAT ARGS."
  (let ((formatted (apply #'format-message format args)))
    (message formatted)
    (with-current-buffer (mason-buffer)
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
              formatted
              "\n")
      (read-only-mode 1))))

(defun mason--err (format &rest args)
  "Call (mason--msg FORMAT ARGS) before throwing `error'."
  (apply #'mason--msg (concat "ERROR: " format) args)
  (apply #'error format args))

(defun mason--uerr (format &rest args)
  "Call (mason--msg FORMAT ARGS) before throwing `user-error'."
  (apply #'mason--msg (concat "USER-ERROR: " format) args)
  (apply #'user-error format args))

(defmacro mason--process-output! ()
  "Copy output of process from BUFFER to buffer command `mason-buffer'."
  `(progn
     (mason--msg "`%s' %s with status %s" msg (if success "finished" "failed") status)
     (with-current-buffer (mason-buffer)
       (let ((start (point-max)))
         (read-only-mode -1)
         (goto-char start)
         (insert-buffer-substring buffer)
         (indent-rigidly start (point) 8)
         (read-only-mode 1)))
     (kill-buffer buffer)
     (unless success (error "Failed %s" msg))))

(defun mason--process (cmd &optional callback)
  "Run process CMD asynchronously, with optional CALLBACK.
CMD is argument list as specified in `make-process' :command."
  (let ((msg (mapconcat #'identity cmd " "))
        (buffer (generate-new-buffer "*mason process*")))
    (mason--msg "Calling `%s'" msg)
    (with-current-buffer buffer (read-only-mode 1))
    (make-process
     :name "mason"
     :buffer buffer
     :command cmd
     :sentinel
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (let* ((status (process-exit-status proc))
                (success (zerop status)))
           (mason--process-output!)
           (when (functionp callback)
             (funcall callback proc event))))))))

(defun mason--process-sync (cmd)
  "Run CMD with ARGS synchronously."
  (let ((msg (mapconcat #'identity cmd " "))
        (buffer (generate-new-buffer "*mason process*"))
        status success)
    (with-current-buffer buffer
      (mason--msg "Calling `%s'" msg)
      (setq status (apply #'call-process (car cmd) nil t nil (cdr cmd)))
      (setq success (zerop status))
      (mason--process-output!)
      (cons status success))))

(defmacro mason--run-at-main (&rest body)
  "Run BODY at main thread."
  (declare (indent defun))
  `(run-at-time 0 nil (lambda () ,@body)))

(defmacro mason--async (&rest body)
  "Run BODY on separate thread."
  (declare (indent defun))
  `(let* ((buffer (generate-new-buffer "*mason async*"))
          (name (buffer-name buffer)))
     (with-current-buffer buffer
       (make-thread
        (lambda ()
          ,@body
          (mason--run-at-main (kill-buffer buffer)))
        name t))))

(defun mason--dir-empty-p (dir)
  "Return t if DIR exists and contains no non-dot files."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp))))

(defmacro mason--make-hash (test &rest kvs)
  "Make a hash table with TEST ('equal, 'eq, etc.) populated with KVS pairs."
  (declare (indent defun))
  `(let ((h (make-hash-table :test ,test)))
     ,@(cl-loop for (k v) on kvs by #'cddr
                collect `(puthash ,k ,v h))
     h))

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
          "\\([A-Za-z0-9_./-]+\\)" ; namespace/name
          "@"
          "\\([A-Za-z0-9_.-]+\\)" ; version
          "$"))

(defun mason--parse-id (id)
  "Parse a source ID."
  (if (string-match mason--id-regexp id)
      (mason--make-hash 'eq
        'type (match-string 1 id)
        'package (match-string 2 id)
        'version (match-string 3 id))
    (mason--err "Unsupported source id `%s'" id)))

(defconst mason--bin-regexp
  ;; [type:]path/to/bin
  (concat "^"
          "\\(" ; optional type
          "\\([A-Za-z0-9_-]+\\)"
          ":"
          "\\)?"
          "\\(" ; path/to/bin
          "[A-Za-z0-9_.-]" ; disallow absolute path
          "[A-Za-z0-9_./-]+"
          "\\)"
          "$"))

(defun mason--parse-bin (bin)
  "Parse a BIN."
  (if (string-match mason--bin-regexp bin)
      (mason--make-hash 'eq
        'type (or (match-string 2 bin) "path")
        'path (match-string 3 bin))
    (mason--err "Unsupported bin `%s'" bin)))

(defun mason--expand (str spec)
  "Expand STR according to SPEC."
  (setq str (replace-regexp-in-string "{{\\([^}]+\\)}}" "${\\1}" str))
  (s-format str (lambda (s)
                  (setq s (string-trim s))
                  (unless (string-match "^[A-Za-z0-9_.-]+$" s)
                    (mason--err "Unsupported expansion `%s' in `%s'" s str))
                  (let ((path (split-string s "\\."))
                        (tree spec))
                    (dolist (p path)
                      (setq tree (when tree (gethash (intern p) tree))))
                    (unless tree
                      (mason--err "Unable to expand `%s' in `%s' with spec `%S'" s str spec))
                    tree))))

(defun mason--extract (file &optional dest)
  "Extract archive FILE into DEST.
If DEST is nil, extract into directory named same as FILE."
  (require 'dired-aux)
  (let* ((rule (seq-find (lambda (e) (string-match (car e) file))
                         dired-compress-file-suffixes))
         (extension (nth 0 rule))
         (new-suffix (nth 1 rule))
         (cmd (nth 2 rule)))
    (when (or (null cmd)
              (not (string-empty-p new-suffix)))
      (mason--err "Can't extract file archive %s" file))
    (setq dest (or dest (replace-regexp-in-string extension "" file)))
    (mkdir dest t)
    (let* ((default-directory dest)
           (cmd (replace-regexp-in-string "%o" (shell-quote-argument dest) cmd))
           (cmd (replace-regexp-in-string "%i" (shell-quote-argument file) cmd)))
      (mason--process-sync (list shell-file-name
                                 shell-command-switch
                                 cmd)))))

(defun mason--download-extract (url dest)
  "Downolad archive from URL and extract to DEST."
  (let* ((filename (file-name-nondirectory (url-filename (url-generic-parse-url url))))
         (tmp (make-temp-file "mason-archive-" nil filename)))
    (mason--msg "Downloading %s to %s" url tmp)
    (unwind-protect
        (let ((status (url-copy-file url tmp t)))
          (unless status
            (mason--msg "Download failed: %s" url)
            (mason--err "Download failed: %s" url))
          (mason--extract tmp dest))
      (when (file-exists-p tmp) (ignore-errors (delete-file tmp))))))



(defmacro mason--source! (type &rest body)
  "Define a mason source resolver for TYPE.

Inside BODY, one can reference:
- NAME is the name of the mason entry.
- PACKAGE and VERSION is the name and version of package to install.
- ID is the entire id spec.
- PREFIX is the directory where the package is expected to be installed.
- SOURCE is the entire source hash-table."
  (declare (indent defun))
  `(defun ,(intern (concat "mason--source-" (symbol-name type))) (name package version id source spec next)
     (let ((prefix (expand-file-name name mason-install-dir)))
       ,@body)))

(defmacro mason--source-process! (&rest cmds)
  "CMDS is vararg of command list, as specified in :command option for `make-process'."
  (declare (indent defun))
  (let (cmd-nest)
    (dolist (cmd (nreverse cmds) cmd-nest)
      (setq cmd-nest
            `(mason--process
              ,cmd
              (lambda (&rest _)
                ,(or cmd-nest '(funcall next))))))
    cmd-nest))

(defmacro mason--source-list! (type &rest cmds)
  "Call (mason--source! TYPE (mason--source-process! CMDS))."
  (declare (indent defun))
  `(mason--source! ,type (mason--source-process! ,@cmds)))

(mason--source-list! cargo
  (list "cargo" "install"
        "--root" prefix
        (concat package "@" version)))

(mason--source-list! pypi
  (list "python" "-m" "venv" prefix)
  (list "pip"
        "--python" (expand-file-name "bin/python" prefix)
        "install"
        "--prefix" prefix
        (concat package "==" version)))

(mason--source-list! npm
  (list "npm" "install" "-g"
        "--prefix" prefix
        (concat package "@" version)))

(defconst mason--github-file-regexp
  (concat "^"
          "\\([A-ZA-Z0-9_.-]+\\)"  ; 1. file path
          "\\("                    ; 2. optional
          ":"
          "\\([A-ZA-Z0-9_./-]+\\)" ; 3. extract path
          "\\)?"
          "$"))

(mason--source! github
  (let ((asset (gethash 'asset source)))
    (unless asset (mason--err "Missing asset"))
    (when (vectorp asset)
      (unless mason-target (mason--err "Customize `mason-target' first"))
      (setq asset (seq-find (lambda (a)
                              (string= mason-target (gethash 'target a)))
                            asset))
      (unless asset (mason--err "No matching asset for target %s" mason-target))
      (puthash 'asset asset source))
    (let* ((file (gethash 'file asset))
           (file (mason--expand file id)))
      (unless (string-match mason--github-file-regexp file)
        (mason--err "Unsupported file asset `%s'" file))
      (let* ((file-path (match-string 1 file))
             (file-url (concat "https://github.com/" package "/releases/download/" version "/" file-path))
             (extract-path (match-string 3 file))
             (extract-dest (if extract-path (expand-file-name extract-path prefix) prefix)))
        (mason--async
          (mason--download-extract file-url extract-dest)
          (run-at-time 0 nil next))))))



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
(mason--bin! exec (expand-file-name path prefix))
(mason--bin! cargo (expand-file-name (concat "bin/" path) prefix))
(mason--bin! pypi (expand-file-name (concat "bin/" path) prefix))
(mason--bin! npm (expand-file-name (concat "bin/" path) prefix))



(defvar mason--ensured 0)
(defun mason--assert-ensured ()
  "Assert if `mason-ensured' is 1."
  (when (= mason--ensured 0) (mason--err "Call `mason-ensure' on your init.el"))
  (when (= mason--ensured -1) (mason--err "Mason is not ready yet")))

;;;###autoload
(defun mason-ensure ()
  "Ensure mason is setup."
  (setenv "PATH" (concat mason-bin-dir ":" (getenv "PATH")))
  (add-to-list 'exec-path mason-bin-dir)
  (let ((dir (expand-file-name mason-registry-dir)))
    (if (not (or (not (file-exists-p dir))
                 (mason--dir-empty-p dir)))
        (setq mason--ensured 1)
      (setq mason--ensured -1)
      (make-directory dir t)
      (mason--process
       (list "github" "clone" "--depth" "1" mason-registry-repo dir)
       (lambda (&rest _)
         (setq mason--ensured 1))))))

(defun mason--get-package-list ()
  "Get list of mason packages."
  (mason--assert-ensured)
  (let ((dir (expand-file-name "packages" mason-registry-dir)))
    (directory-files dir nil directory-files-no-dot-files-regexp)))

(defun mason--get-package-path (package)
  "Get PACKAGE spec path."
  (expand-file-name (concat "packages/" package "/package.yaml") mason-registry-dir))

;;;###autoload
(defun mason-spec (package)
  "Visit Mason spec file for PACKAGE."
  (interactive (list (completing-read "Mason: " (mason--get-package-list) nil t)))
  (find-file (mason--get-package-path package)))

;;;###autoload
(defun mason-install (package)
  "Install a Mason PACKAGE."
  (interactive (list nil))
  (let* ((interactive (null package))
         (package (or package (completing-read "Mason: " (mason--get-package-list) nil t)))
         (path (mason--get-package-path package))
         (spec (mason--parse-yaml path))
         (name (gethash 'name spec))
         (package-dir (expand-file-name name mason-install-dir))
         ;; source
         (source (gethash 'source spec))
         (source-id-raw (gethash 'id source))
         (source-id (mason--parse-id source-id-raw))
         (source-supported-platforms (gethash 'supported_platforms source))
         (source-type (gethash 'type source-id))
         (source-package (gethash 'package source-id))
         (source-version (gethash 'version source-id))
         (source-fn (intern (concat "mason--source-" source-type)))
         ;; bin
         (bin (gethash 'bin spec)))
    (when (not (fboundp source-fn))
      (mason--err "Unsupported source type %s in id %s" source-type source-id-raw))
    (when (and (file-directory-p package-dir)
               (not (directory-empty-p package-dir)))
      (if (not interactive)
          (mason--err "Directory %s already exists" package-dir)
        (if (y-or-n-p (format-message "Directory %s exists, delete? " package-dir))
            (progn (mason--msg "Deleting %s" package-dir)
                   (delete-directory package-dir t nil))
          (error "Cancelled"))))
    (mason--msg "Installing %s" source-id-raw)
    (funcall
     source-fn name source-package source-version source-id source spec
     (lambda ()
       (maphash (lambda (key val-raw)
                  (setq val-raw (mason--expand val-raw spec))
                  (let* ((val (mason--parse-bin val-raw))
                         (bin-type (gethash 'type val))
                         (bin-path (gethash 'path val))
                         (bin-fn (intern (concat "mason--bin-" bin-type))))
                    (when (or (null val) (not (fboundp bin-fn)))
                      (mason--err "Unsupported binary %s" val-raw))
                    (let ((bin-target (funcall bin-fn name bin-path))
                          (bin-link (expand-file-name (symbol-name key) mason-bin-dir)))
                      (make-directory mason-bin-dir t)
                      (when (file-exists-p bin-link)
                        (delete-file bin-link))
                      (make-symbolic-link bin-target bin-link)
                      (mason--msg "Symlinked %s -> %s" bin-link bin-target))))
                bin)))))

(provide 'mason)
;;; mason.el ends here
