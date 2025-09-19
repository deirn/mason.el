;;; mason.el --- Mason                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dimas Firmansyah

;; Author: Dimas Firmansyah <deirn@bai.lol>
;; Version: 1.0.0
;; Homepage: https://github.com/deirn/mason.el
;; Package-Requires: ((emacs "30.1") (s "1.13.1"))
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
(require 'json)
(require 'seq)
(require 's)
(require 'url-parse)

(defgroup mason nil
  "Installer for LSP servers, DAP servers, linters, and formatters."
  :prefix "mason-"
  :group 'tools)

(defcustom mason-dry-run nil
  "If not nil, only print messages what mason would do."
  :type 'boolean :group 'mason)

(defcustom mason-dir (expand-file-name "mason" user-emacs-directory)
  "Directory where to find mason files."
  :type 'directory :group 'mason)

(defcustom mason-registry-refresh-time (* 60 60 24 7)
  "How long in seconds the before trying to refresh the registry.
Defaults to 1 week."
  :type 'integer :group 'mason)

(defcustom mason-registries
  '(("mason" . "https://github.com/mason-org/mason-registry/releases/latest/download/registry.json.zip"))
  "Alist of registry name and registry json archive link."
  :type '(alist :key-type string :value-type string)
  :group 'mason)

(defcustom mason-show-deprecated nil
  "Wheter to show deprecated packages."
  :type 'boolean :group 'mason)

(defface mason-deprecated
  '((t :inherit shadow :strike-through t))
  "Face used for deprecated packages.")


;; Utility Functions

(defconst mason-buffer "*mason*")
(defun mason-buffer ()
  "Get mason buffer."
  (or (get-buffer mason-buffer)
      (with-current-buffer (get-buffer-create mason-buffer)
        (fundamental-mode)
        (read-only-mode 1)
        (current-buffer))))

(defun mason--msg (format &rest args)
  "Message with prefix.  See `message' FORMAT ARGS."
  (let ((formatted (apply #'format-message format args)))
    (message "%s" formatted)
    (with-current-buffer (mason-buffer)
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (when mason-dry-run (insert "[DRY] "))
      (insert formatted "\n")
      (read-only-mode 1))
    formatted))

(defun mason--err (format &rest args)
  "Call (mason--msg FORMAT ARGS) before throwing `error'."
  (apply #'mason--msg (concat "ERROR: " format) args)
  (let ((err (list (apply #'format-message format args))))
    (when mason-dry-run (push :mason err) (push t err))
    (signal 'error (nreverse err))))

(defun mason--uerr (format &rest args)
  "Call (mason--msg FORMAT ARGS) before throwing `user-error'."
  (apply #'mason--msg (concat "USER-ERROR: " format) args)
  (let ((err (list (apply #'format-message format args))))
    (when mason-dry-run (push :mason err) (push t err))
    (signal 'user-error (nreverse err))))

(defun mason--quote (str &optional always)
  "Quote STR if it contains spaces or if ALWAYS non nil."
  (if (or always (string-match-p "[[:space:]]" str))
      (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" str))
    str))

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
     (unless success (error "Failed `%s'" msg))))

(cl-defun mason--process (cmd &optional &key env then)
  "Run process CMD asynchronously.
CMD is argument list as specified in `make-process' :command.
ENV is alist of environment variable to add to `process-environment'.
THEN is a function to call after process succeed."
  (declare (indent defun))
  (let ((prog (car cmd))
        (msg (mapconcat #'mason--quote cmd " "))
        (process-environment process-environment)
        buffer)
    (when env
      (dolist (e (nreverse env))
        (let ((k (car e)) (v (cdr e)))
          (push (concat k "=" v) process-environment)
          (setq msg (concat k "=" (mason--quote v) " " msg)))))
    (mason--msg "Calling `%s'" msg)
    (when mason-dry-run
      (when (functionp then) (funcall then))
      (cl-return-from mason--process nil))
    (unless (executable-find prog)
      (mason--err "Missing program `%s'" prog))
    (setq buffer (generate-new-buffer "*mason process*"))
    (with-current-buffer buffer (read-only-mode 1))
    (make-process
     :name "mason"
     :buffer buffer
     :command cmd
     :sentinel
     (lambda (proc _)
       (when (memq (process-status proc) '(exit signal))
         (let* ((status (process-exit-status proc))
                (success (zerop status)))
           (mason--process-output!)
           (when (functionp then)
             (funcall then))))))))

(cl-defmacro mason--process-lamda (cmd &optional &key env then)
  "Wrap `mason--process' inside lambda.
CMD ENV THEN"
  (declare (indent defun))
  `(lambda () (mason--process ,cmd :env ,env :then ,then)))

(cl-defun mason--process-sync (cmd &optional &key in out)
  "Run CMD with ARGS synchronously.
See `call-process' INFILE and DESTINATION for IN and OUT."
  (let ((prog (car cmd))
        (msg (mapconcat #'mason--quote cmd " "))
        buffer status success)
    (mason--msg "Calling `%s'" msg)
    (when mason-dry-run (cl-return-from mason--process-sync nil))
    (unless (executable-find prog)
      (mason--err "Missing program `%s'" prog))
    (setq buffer (generate-new-buffer "*mason process*"))
    (with-current-buffer buffer
      (setq status (apply #'call-process prog in (or out t) nil (cdr cmd)))
      (setq success (zerop status))
      (mason--process-output!)
      (cons status success))))

(defmacro mason--run-at-main (&rest body)
  "Run BODY at main thread."
  (declare (indent defun))
  `(let ((fn (lambda () ,@body)))
     (if mason-dry-run
         (funcall fn)
       (run-at-time 0 nil fn))))

(defmacro mason--async (&rest body)
  "Run BODY on separate thread."
  (declare (indent defun))
  `(let* ((buffer (generate-new-buffer "*mason async*"))
          (name (buffer-name buffer))
          (fn (lambda ()
                (unwind-protect (progn ,@body)
                  (mason--run-at-main (kill-buffer buffer))))))
     (if mason-dry-run
         (funcall fn)
       (with-current-buffer buffer
         (make-thread fn name t)))))

(defun mason--is-cygwin ()
  "Returns non nil if `system-type' is cygwin."
  (eq system-type 'cygwin))

(defun mason--is-windows (&optional cygwin)
  "Returns non nil if `system-type' is windows-nt.
Also returns non nil if `system-type' is cygwin when CYGWIN param is non nil."
  (or (eq system-type 'windows-nt)
      (and cygwin (mason--is-cygwin))))

(defvar mason--target nil)
(defun mason--update-target ()
  "Update target detection."
  (let (os arch libc)
    (cond
     ((or (mason--is-windows t))
      (setq os '("windows")
            arch (let* ((pa (getenv "PROCESSOR_ARCHITECTURE"))
                        (wa (getenv "PROCESSOR_ARCHITEW6432"))
                        (ar (or wa pa "")))
                   (cond
                    ((string-match-p (rx bow (or "AMD64" "x86_64" "X86-64") eow) ar) "x64")
                    ((string-match-p (rx bow "ARM64" eow) ar) "arm64")
                    ((string-match-p (rx bow "ARM" eow) arch) "arm32")
                    ((string-match-p (rx bow (or "x86" "i386" "i686") eow) arch) "x86")
                    (t nil)))
            libc nil))
     ((memq system-type '(ms-dos)) (ignore))
     (t
      (setq os (cond
                ((eq system-type 'gnu/linux) '("linux" "unix"))
                ((eq system-type 'darwin) '("darwin" "unix"))
                (t '("unix")))
            arch (let ((uname (string-trim (shell-command-to-string "uname -m 2>/dev/null || true"))))
                   (cond
                    ((string-match-p (rx bow (or "x86_64" "amd64" "x64" "x86-64") eow) uname) "x64")
                    ((string-match-p (rx bow (or "aarch64" "arm64") eow) uname) "arm64")
                    ((string-match-p (rx bow (or "armv[0-9]+" "armv[0-9]+l" "arm" "armhf" "armel") eow) uname) "arm32")
                    ((string-match-p (rx bow (or "x86" "i386" "i686") eow) uname) "x86")
                    (t nil)))
            libc (let ((ldd (shell-command-to-string "ldd --version 2>&1 || true")))
                   (cond
                    ((string-match-p "musl" ldd) "musl")
                    ((string-match-p (rx (or "GNU libc" "glibc" "GNU C Library")) ldd) "gnu")
                    (t nil))))))
    (mason--run-at-main (setq mason--target (list os arch libc)))))

(defun mason--target-match (str)
  "Return non nil when target STR match current target."
  (let* ((os (nth 0 mason--target))
         (arch (nth 1 mason--target))
         (libc (nth 2 mason--target))
         (s-split (s-split-up-to "_" str 3 'omit-nulls))
         (s-os (nth 0 s-split))
         (s-arch (nth 1 s-split))
         (s-libc (nth 2 s-split))
         (match (or s-os s-arch s-libc)))
    (when s-os (setq match (and match (member s-os os))))
    (when s-arch (setq match (and match (equal arch s-arch))))
    (when s-libc (setq match (and match (equal libc s-libc))))
    match))

(defun mason--dir-empty-p (dir)
  "Return t if DIR exists and contains no non-dot files."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp))))

(defun mason--path-descendant-p (path base)
  "Return t if PATH is equal to or underneath BASE."
  (let* ((p (directory-file-name path))
         (b (directory-file-name base)))
    (string-prefix-p (file-name-as-directory b)
                     (file-name-as-directory p))))

(defun mason--expand-child-file-name (path parent)
  "Expand file PATH to PARENT, like `expand-file-name'.
Throws error when resulting path is not inside PARENT."
  (let ((res (expand-file-name path parent)))
    (unless (mason--path-descendant-p res parent)
      (mason--err "Path `%s' is not inside `%s'" res parent))
    res))

(defmacro mason--make-hash (&rest kvs)
  "Make a hash table with `equal' test populated with KVS pairs."
  (declare (indent defun))
  `(let ((h (make-hash-table :test 'equal)))
     ,@(cl-loop for (k v) on kvs by #'cddr
                collect `(puthash ,k ,v h))
     h))

(defun mason--hash-keys (table)
  "Get list of keys from TABLE."
  (when table
    (let (keys)
      (maphash (lambda (k _v) (push k keys)) table)
      (nreverse keys))))

(defun mason--expect-hash-key (table &rest keys)
  "Throw an error if hash TABLE key not in KEYS."
  (maphash
   (lambda (k _v)
     (unless (member k keys)
       (mason--err "Unexpected key `%s' in table `%s'"
                   k (json-encode table))))
   table))

(defun mason--parse-purl (string)
  "Parse a PURL STRING.
Returns a hash table of members:
- raw
- scheme
- type
- namespace
- name
- version
- qualifiers
- subpath

https://github.com/package-url/purl-spec"
  (let* ((url (url-generic-parse-url string))
         (path-and-query (url-path-and-query url))
         (scheme (url-type url))
         (path (car path-and-query))
         type name namespace version
         (q-str (cdr path-and-query))
         qualifiers
         (subpath (url-target url))
         purl)
    (let ((tn-split (s-split-up-to "/" path 1 t)))
      (unless (length= tn-split 2)
        (mason--err "Failed to parse PURL: `%s' does not contain type and name"))
      (setq type (nth 0 tn-split)
            name (nth 1 tn-split)))
    (let ((ns-split (s-split-up-to "/" name 1 t)))
      (when (length= ns-split 2)
        (setq namespace (url-unhex-string (nth 0 ns-split))
              name (nth 1 ns-split))))
    (let ((nv-split (s-split-up-to "@" name 1 t)))
      (setq name (nth 0 nv-split)
            version (nth 1 nv-split)))
    (setq name (url-unhex-string name))
    (setq purl (mason--make-hash
                 "raw" string
                 "scheme" scheme
                 "type" type
                 "namespace" namespace
                 "name" name
                 "version" version
                 "qualifiers" nil
                 "subpath" subpath))
    (when q-str
      (setq qualifiers (mason--make-hash))
      (dolist (e (url-parse-query-string q-str))
        (puthash (nth 0 e) (nth 1 e) qualifiers))
      (puthash "qualifiers" qualifiers purl))
    purl))

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
      (mason--make-hash
        "type" (or (match-string 2 bin) "path")
        "path" (match-string 3 bin))
    (mason--err "Unsupported bin `%s'" bin)))

(defun mason--unquote-string-or-nil (s)
  "If S is a double-quoted string, return its unescaped contents; otherwise nil."
  (when (and (stringp s)
             (>= (length s) 2)
             (eq (aref s 0) ?\")
             (eq (aref s (1- (length s))) ?\"))
    (let ((inner (substring s 1 (1- (length s)))))
      (replace-regexp-in-string
       "\\\\[\\\"\\\\ntbr]"   ; match backslash escapes
       (lambda (esc)
         (pcase (aref esc 1)
           (?\" "\"")
           (?\\ "\\")
           (?n "\n")
           (?t "\t")
           (?b "\b")
           (?r "\r")
           (_ (substring esc 1))))
       inner))))

(defun mason--unquote-string (s)
  "If S is double-quoted, return its unescaped contents; otherwise return S."
  (or (mason--unquote-string-or-nil s) s))

(defun mason--expand (str spec)
  "Expand STR according to hash table SPEC."
  (let* ((dollar (replace-regexp-in-string "{{\\([^}]+\\)}}" "${\\1}" str))
         expanded
         (expanded-str
          (s-format
           dollar
           (lambda (exp)
             ;; TODO: more proper splitting
             (let* ((pipes (mapcar #'string-trim (split-string exp "|" 'omit-nulls)))
                    (var (nth 0 pipes))
                    (ops (if (length< pipes 2) nil (seq-subseq pipes 1))))
               (when (null pipes)
                 (mason--err "Empty expansion expression in `%s'" str))
               (unless (string-match "^[A-Za-z0-9_.-]+$" var)
                 (mason--err "Unsupported expansion variable `%s' in `%s'" var str))
               (let ((path (split-string var "\\."))
                     (tree spec))
                 (dolist (p path)
                   (setq tree (when tree (gethash p tree))))
                 (unless tree
                   (mason--err "Unable to expand variable `%s' in `%s' with spec `%s'" var str (json-serialize spec)))
                 (setq var tree))
               (dolist (op ops)
                 (cond
                  ((string-prefix-p "strip_prefix " op)
                   (let* ((prefix (mason--unquote-string-or-nil (string-trim (substring op (length "strip_prefix "))))))
                     (unless prefix
                       (mason--err "Unable to expand `%s': strip_prefix can only accept one argument of type string" str))
                     (unless (string-prefix-p prefix var)
                       (mason--err "Unable to expand `%s': strip_prefix: `%s' is not prefixed with `%s'" str var prefix))
                     (setq var (substring var (length prefix)))))
                  (t (mason--err "Unable to expand `%s': unsupported operation `%s'" str op))))
               (setq expanded t)
               var)))))
    (when expanded
      (mason--msg "Expanded `%s' to `%s'" str expanded-str))
    expanded-str))

(defun mason--download (url newname &optional ok-if-already-exists)
  "Copy URL to NEWNAME.
OK-IF-ALREADY-EXISTS is the same in `url-copy-file'."
  (mason--msg "Downloading %s to %s" url newname)
  (or mason-dry-run (url-copy-file url newname ok-if-already-exists)))

(defun mason--delete-directory (path &optional recursive ignore-dry-run)
  "Delete directory at PATH, optionally RECURSIVE.
If IGNORE-DRY-RUN, delete anyway even if `mason-dry-run' is non nil."
  (when (or (not mason-dry-run) ignore-dry-run)
    (delete-directory path recursive nil))
  (mason--msg "Deleted `%s'" (directory-file-name path)))

(defun mason--delete-file (path &optional ignore-dry-run)
  "Delete file at PATH.
If IGNORE-DRY-RUN, delete anyway even if `mason-dry-run' is non nil."
  (when (or (not mason-dry-run) ignore-dry-run)
    (delete-file path))
  (mason--msg "Deleted `%s'" path))

(defun mason--download-maybe-extract (url dest)
  "Download file from URL.
If it a supported archive, extract into directory DEST.
If not, simply save it as DEST, or inside DEST if it is a directory.
See `mason--extract-strategies'."
  (let* ((filename (file-name-nondirectory (url-filename (url-generic-parse-url url))))
         (tmp-dir (make-temp-file "mason-download-" 'dir))
         (tmp-file (mason--expand-child-file-name filename tmp-dir)))
    (unwind-protect
        (let ((status (mason--download url tmp-file t)))
          (unless status
            (mason--err "Download failed: %s" url))
          (if (mason--archive-name filename)
              (mason--try-extract tmp-file dest)
            (unless mason-dry-run
              (when (or (directory-name-p dest) (file-directory-p dest))
                (progn (make-directory dest t)
                       (setq dest (mason--expand-child-file-name filename dest))))
              (make-directory (file-name-parent-directory dest) t)
              (copy-file tmp-file dest))
            (mason--msg "Copied `%s' to `%s'" tmp-file dest)))
      (when (file-directory-p tmp-dir)
        (ignore-errors (mason--delete-directory tmp-dir t t))))))

(cl-defun mason--make-shell (path content &optional &key overwrite env)
  "Make a shell script at PATH with CONTENT.
Delete existing file if OVERWRITE is not nil.
ENV is alist of additional environment variable to set.
Returns the modified PATH, added with .bat extension in Windows."
  (let* ((windows (mason--is-windows t))
         (path (if windows (concat path ".bat") path))
         (c (mapconcat #'mason--quote content " ")))
    (unless mason-dry-run
      (make-directory (file-name-parent-directory path) t)
      (when (and overwrite (file-exists-p path))
        (mason--delete-file path))
      (with-temp-file path
        (cond
         (windows
          (insert "@echo off\r\n"
                  "setlocal enabledelayedexpansion\r\n"
                  "set \"args=\"\r\n"
                  "for %%A in (%*) do (\r\n"
                  "  set \"args=!args! \"%%~A\"\"\r\n"
                  ")\r\n")
          (dolist (e (nreverse env))
            (insert "set " "\"" (car e) "=" (cdr e) "\"\r\n"))
          (insert (replace-regexp-in-string "\n" "\r\n" c) "\r\n"))
         ;; unix
         (t (insert "#!/usr/bin/env bash\n")
            (dolist (e (nreverse env))
              (insert "export " (car e) "=" (mason--quote (cdr e) 't) "\n"))
            (insert c "\n"))))
      (unless windows
        (set-file-modes path #o755)))
    (mason--msg "Made wrapper script at `%s'" path)
    path))

(defun mason--shell-env (env)
  "Return shell script ENV reference."
  (if (mason--is-windows t) (concat "%" env "%")
    (concat "$" env)))

(defun mason--shell-exec ()
  "Return exec on unix."
  (if (mason--is-windows) "" "exec"))

(defun mason--shell-args ()
  "Arguments expansion for `mason--make-shell', $@ in unix."
  (if (mason--is-windows) "!args!" "\"$@\""))

(defun mason--shell-run (path)
  "Run shell at PATH synchronously."
  (let* ((windows (mason--is-windows t))
         (shell (if windows '("cmd.exe" "/c") '("bash" "-lc"))))
    (when windows
      (unless (string= "bat" (file-name-extension path))
        (setq path (concat path ".bat")))
      (setq path (subst-char-in-string ?/ ?\\ path)))
    (mason--process-sync `(,@shell ,path))))

(defun mason--link (path target &optional overwrite)
  "Create a symbolic link at PATH to TARGET.
Delete existing file if OVERWRITE is not nil."
  (unless mason-dry-run
    (make-directory (file-name-parent-directory path) t)
    (when (and overwrite (file-exists-p path))
      (mason--delete-file path))
    (make-symbolic-link target path))
  (mason--msg "Made symlink at `%s' that links to `%s'" path target))


;; Archive Extractors

(defvar mason--extractors nil)

(defmacro mason--extract! (name ext replace cmd &rest args)
  "Define an archive extractor NAME for EXT with CMD.
REPLACE occurence of EXT with the value if it not nil.
See `mason--process-sync' for CMD and ARGS."
  (declare (indent defun))
  (let* ((fn-name (concat "mason--extract-" (symbol-name name)))
         (fn (intern fn-name))
         (regexp (macroexpand `(rx "." ,ext eos))))
    `(progn
       (defun ,fn (file dest)
         (let* ((default-directory dest)
                (out-file (replace-regexp-in-string ,regexp ,replace (file-name-nondirectory file)))
                (out-file (mason--expand-child-file-name out-file dest)))
           (ignore out-file)
           (mason--process-sync ,cmd ,@args)))
       (add-to-list 'mason--extractors '(,regexp ,replace ,fn)))))

(defmacro mason--extract-stdio! (name ext replace cmd)
  "Extractor for CMD that outputs to stdout.
See `mason--extract!' for NAME, EXT, REPLACE."
  (declare (indent defun))
  `(mason--extract! ,name ,ext ,replace ,cmd :out `(:file ,out-file)))

(defun mason--try-extract (file dest)
  "Extract FILE to dir DEST, if it can be extracted.
If not, simply move FILE to DEST."
  (setq file (expand-file-name file)
        dest (file-name-as-directory (expand-file-name dest)))
  (let ((tmp-dir (make-temp-file "mason-extract-" 'dir)))
    (unwind-protect
        (let ((fn (nth 2 (seq-find (lambda (x) (string-match-p (car x) file)) mason--extractors))))
          (when fn
            (mason--msg "Extracting `%s' to `%s' using `%s'" file tmp-dir (symbol-name fn))
            (unless mason-dry-run
              (funcall fn file tmp-dir)
              (let ((result (directory-files tmp-dir 'full directory-files-no-dot-files-regexp)))
                (if (length< result 2)
                    ;; single file, try extracting it again
                    ;; file.tar.gz > file.tar > files
                    (dolist (file2 result)
                      (mason--try-extract file2 dest))
                  ;; multiple files, can't be a multistage
                  (make-directory dest t)
                  (dolist (file2 result)
                    (rename-file file2 dest))))))
          (unless (or fn mason-dry-run)
            (make-directory dest t)
            (rename-file file dest)))
      (mason--delete-directory tmp-dir t t))))

(defun mason--archive-name (archive &optional return-orig)
  "Return ARCHIVE file name, without the archive extension.
If not a supported archive, return nil if RETURN-ORIG is nil,
otherwise, return the original file name."
  (let* ((rule (seq-find (lambda (x) (string-match-p (car x) archive)) mason--extractors))
         (regexp (nth 0 rule))
         (replace (nth 1 rule)))
    (if rule (mason--archive-name (replace-regexp-in-string regexp replace archive) t)
      (when return-orig archive))))

(mason--extract! 7z  "7z"              "" `("7z" "x" "-aoa" ,(concat "-o" dest) ,file))
(mason--extract! tar "tar"             "" `("tar" "-xpf" ,file "-C" ,dest))
(mason--extract! zip (or "zip" "vsix") "" `("unzip" "-o" "-d" ,dest ,file))
(mason--extract! xar "xar"             "" `("xar" "-x" "-f" ,file "-C" ,dest))

(mason--extract-stdio! bzip2 "bz2"         ""     `("bunzip2"    "-c"  ,file))
(mason--extract-stdio! dz    "dz"          ""     `("dictunzip"  "-c"  ,file))
(mason--extract-stdio! gzip  (or "gz" "z") ""     `("gzip"       "-dc" ,file))
(mason--extract-stdio! lzip  "lz"          ""     `("lzip"       "-dc" ,file))
(mason--extract-stdio! xz    "xz"          ""     `("unxz"       "-c"  ,file))
(mason--extract-stdio! Z     "Z"           ""     `("uncompress" "-c"  ,file))
(mason--extract-stdio! zst   "zst"         ""     `("unzstd"     "-c"  ,file))
(mason--extract-stdio! tzst  "tzst"        ".tar" `("unzstd"     "-c"  ,file))


;; Source Resolvers

(cl-defmacro mason--source! (type
                             (&key (namespace  'none)
                                   (version    'must)
                                   (qualifiers 'none)
                                   (subpath    'none)
                                   (keys       '()))
                             &rest body)
  "Define a mason source resolver for TYPE.

These keys declare support for PURL member:
:NAMESPACE  none, optional or must, defaults to none
:VERSION    none, optional or must, defaults to must
:QUALIFIERS none or (\"q1\" \"q2\"), defaults to none
:SUBPATH    none, optional or must, defaults to none

- If none, ID can not have the member.
- If optional, ID can have the member.
- If must, ID must have the member.
- Special for :qualifiers, if list, ID can only have
  qualifiers from the specified list.

There also special keys:
:KEYS is a list that decide what keys the SOURCE table are allowed,
      defaults to nil

Inside BODY, one can reference:
- NAME is the name of the mason entry.
- ID is the entire `mason--parse-purl' result.
- Members of ID, prefixed with ID- (e.g.) ID-VERSION
- PREFIX is the directory where the package is expected to be installed.
- SOURCE is the entire source hash-table.
- NEXT is the function to call after the process done."
  (declare (indent 2))
  (let* ((fn-name (concat "mason--source-" (symbol-name type)))
         (values '(none optional must))
         ;; namespace
         (p-namespace
          (cond
           ((eq namespace 'none)
            `(when id-namespace
               (mason--err "`%s': `%s' must not have namespace" ,fn-name id-raw)))
           ((eq namespace 'optional)
            '(ignore))
           ((eq namespace 'must)
            `(unless id-namespace
               (mason--err "`%s': `%s' must have namespace" ,fn-name id-raw)))
           (t (error "`%s': :namespace support must be one of %S" fn-name values))))
         ;; version
         (p-version
          (cond
           ((eq version 'none)
            `(when id-version
               (mason--err "`%s': `%s' must not have version" ,fn-name id-raw)))
           ((eq version 'optional)
            '(ignore))
           ((eq version 'must)
            `(unless id-version
               (mason--err "`%s': `%s' must have version" ,fn-name id-raw)))
           (t (error "`%s': :version support must be one of %S" fn-name values))))
         ;; subpath
         (p-subpath
          (cond
           ((eq subpath 'none)
            `(when id-subpath
               (mason--err "`%s': `%s' must not have subpath" ,fn-name id-raw)))
           ((eq subpath 'optional)
            '(ignore))
           ((eq subpath 'must)
            `(unless id-subpath
               (mason--err "`%s': `%s' must have subpath" ,fn-name id-raw)))
           (t (error "`%s': :subpath support must be one of %S" fn-name values))))
         ;; qualifiers
         (p-qualifiers
          (cond
           ((eq qualifiers 'none)
            `(when id-qualifiers
               (mason--err "`%s': `%s' must not have qualifiers" ,fn-name id-raw)))
           ((and (listp qualifiers) (not (seq-empty-p qualifiers)))
            `(unless (seq-every-p (lambda (q) (member q m-qualifiers)) (mason--hash-keys id-qualifiers))
               (mason--err "`%s': `%s' must only have qualifiers of key %S" ,fn-name id-raw m-qualifiers)))
           (t (error "`%s': :qualifiers must be none or list" fn-name)))))
    (unless (listp keys)
      (error "`%s': :keys must be a list" fn-name))
    ;; resulting function
    `(defun ,(intern fn-name) (name prefix id source spec next)
       (let* ((m-qualifiers ',qualifiers)
              (m-keys       ',keys)
              (id-raw        (gethash "raw" id))
              (id-scheme     (gethash "scheme" id))
              (id-type       (gethash "type" id))
              (id-namespace  (gethash "namespace" id))
              (id-name       (gethash "name" id))
              (id-version    (gethash "version" id))
              (id-qualifiers (gethash "qualifiers" id))
              (id-subpath    (gethash "subpath" id)))
         (ignore name id source spec
                 m-qualifiers m-keys
                 id-raw id-scheme id-type id-namespace id-name id-version id-qualifiers id-subpath)
         (apply #'mason--expect-hash-key source "id" m-keys)
         ,p-namespace
         ,p-version
         ,p-subpath
         ,p-qualifiers
         ,@body))))

(defun mason--source-target (source key)
  "Get value that with matching target from SOURCE[KEY].
See `mason--target-match'"
  (let ((val (gethash key source)))
    (unless val (mason--err "Missing `%s'" key))
    (when (vectorp val)
      (setq val
            (seq-find
             (lambda (e)
               (let ((target (gethash "target" e)))
                 (unless (vectorp target)
                   (setq target (vector target)))
                 (seq-some (lambda (x) (mason--target-match x)) target)))
             val))
      (unless val (mason--err "No matching `%s' for target %s" key mason--target))
      (puthash key val source))
    val))

(defun mason--source-uninstall (_name prefix _id _source _spec next)
  "A uninstall source \"resolver\", deletes the PREFIX and call NEXT."
  (mason--delete-directory prefix t)
  (funcall next))

(mason--source! cargo (:qualifiers ("repository_url" "rev" "locked"))
  (let (repo-url rev (locked t))
    (when id-qualifiers
      (setq repo-url (gethash "repository_url" id-qualifiers)
            rev (string= (gethash "rev" id-qualifiers "") "true")
            locked (not (string= (gethash "locked" id-qualifiers "") "false"))))
    (mason--process `("cargo" "install"
                      "--root" ,prefix
                      ,id-name
                      ,@(if repo-url
                            `("--git" ,repo-url
                              ,(if rev "--rev" "--tag")
                              ,id-version)
                          `("--version" ,id-version))
                      ,@(when locked '("--locked")))
      :then next)))

(mason--source! pypi (:qualifiers ("extra")
                      :keys ("extra_packages"))
  (let (extra)
    (when id-qualifiers
      (setq extra (gethash "extra" id-qualifiers)))
    (mason--process `("python" "-m" "venv" ,prefix)
      :then
      (mason--process-lamda `("pip"
                              "--python" ,(mason--expand-child-file-name "bin/python" prefix)
                              "install"
                              "--prefix" ,prefix
                              ,(if extra (format "%s[%s]==%s" id-name extra id-version)
                                 (format "%s==%s" id-name id-version))
                              ,@(seq-into (gethash "extra_packages" source) 'list))
        :then next))))

(mason--source! npm (:namespace optional
                     :keys ("extra_packages"))
  (mason--process `("npm" "install" "-g"
                    "--prefix" ,prefix
                    ,(concat
                      id-namespace (when id-namespace "/")
                      id-name "@" id-version)
                    ,@(seq-into (gethash "extra_packages" source) 'list))
    :then next))

(mason--source! golang (:namespace must
                        :subpath must)
  (mason--process `("go" "install" ,(concat id-namespace "/" id-name "/" id-subpath "@" id-version))
    :env `(("GOBIN" . ,prefix))
    :then next))

(mason--source! nuget ()
  (mason--process `("dotnet" "tool" "install" ,id-name
                    "--version" ,id-version
                    "--tool-path" ,prefix)
    :then next))

(mason--source! luarocks (:qualifiers ("repository_url" "dev"))
  (let (server dev)
    (when id-qualifiers
      (setq server (gethash "repository_url" id-qualifiers)
            dev (string= "true" (gethash "dev" id-qualifiers ""))))
    (mason--process `("luarocks" "install"
                      "--tree" ,prefix
                      ,@(when server `("--server" ,server))
                      ,@(when dev '("--dev"))
                      ,id-name ,id-version)
      :then next)))

(mason--source! gem (:keys ("extra_packages"))
  (mason--process `("gem" "install"
                    "--no-user-install"
                    "--no-format-executable"
                    "--install-dir" ,prefix
                    "--bindir" ,(mason--expand-child-file-name "bin" prefix)
                    "--no-document"
                    ,(concat id-name ":" id-version)
                    ,@(seq-into (gethash "extra_packages" source) 'list))
    :env `(("GEM_HOME" . ,prefix))
    :then next))

(mason--source! opam ()
  (mason--process `("opam" "switch" "create" ,prefix
                    "--yes" "--assume-depexts"
                    "--packages" ,(concat id-name "." id-version))
    :then next))

(mason--source! openvsx (:namespace must
                         :keys ("download"))
  (let* ((download (gethash "download" source))
         (_ (mason--expect-hash-key download "file"))
         (file (gethash "file" download)))
    (unless file (mason--err "Missing file to download"))
    (setq file (mason--expand file id))
    (unless (string= "vsix" (file-name-extension file))
      (mason--err "File `%s' not a VSCode extension" file))
    (mason--async
      (mason--download-maybe-extract
       (concat "https://open-vsx.org/api/" id-namespace "/" id-name "/" id-version "/file/" file)
       prefix)
      (funcall next))))

(mason--source! composer (:namespace must)
  (unless mason-dry-run (make-directory prefix t))
  (mason--process `("composer" "init"
                    "--stability" "stable"
                    "--no-interaction"
                    "--working-dir" ,prefix)
    :then
    (mason--process-lamda `("composer" "require"
                            "--working-dir" ,prefix
                            "--" ,(concat id-namespace "/" id-name ":" id-version))
      :then next)))

(defconst mason--github-file-regexp
  (concat "^"
          "\\([A-ZA-Z0-9_.-]+\\)"  ; 1. file path
          "\\("                    ; 2. optional
          ":"
          "\\([A-ZA-Z0-9_./-]+\\)" ; 3. extract path
          "\\)?"
          "$"))

(mason--source! github (:namespace must
                        :keys ("asset"))
  (let* ((asset (mason--source-target source "asset"))
         (files (gethash "file" asset))
         tasks)
    (unless files (mason--err "No files"))
    (unless (vectorp files) (setq files (vector files)))
    (setq tasks
          (mapcar
           (lambda (file)
             (setq file (mason--expand file id))
             (unless (string-match mason--github-file-regexp file)
               (mason--err "Unsupported file asset `%s'" file))
             (let* ((file-path (match-string 1 file))
                    (file-url (concat "https://github.com/" id-namespace "/" id-name "/releases/download/" id-version "/" file-path))
                    (extract-path (match-string 3 file))
                    (extract-dest (if extract-path (mason--expand-child-file-name extract-path prefix) prefix)))
               (lambda ()
                 (mason--download-maybe-extract file-url extract-dest))))
           files))
    (mason--async
      (dolist (task tasks)
        (funcall task))
      (mason--run-at-main (funcall next)))))

(mason--source! generic (:namespace optional
                         :version optional
                         :keys ("download"))
  (let* ((download (mason--source-target source "download"))
         (files (gethash "files" download))
         tasks)
    (unless files (mason--err "No files"))
    (maphash (lambda (dest url)
               (setq url (mason--expand url id))
               (let ((archive (mason--archive-name dest)))
                 (when archive
                   (setq dest archive)
                   (when (string= dest name)
                     (setq dest "."))))
               (setq dest (mason--expand-child-file-name dest prefix))
               (push (lambda () (mason--download-maybe-extract url dest)) tasks))
             files)
    (mason--async
      (dolist (task (nreverse tasks))
        (funcall task))
      (mason--run-at-main (funcall next)))))


;; Binary Resolvers

(defmacro mason--bin! (type &rest body)
  "Define a mason binary resolver for TYPE.
BODY is `progn' body.

Inside BODY, one can reference:
- PREFIX is where the package should've been installed.
- PATH is where the wrapper/link should be placed.
- TARGET is the target binary, depends on the TYPE.
- UNINSTALL is wheter to install or uninstall the binary."
  (declare (indent defun))
  `(defun ,(intern (concat "mason--bin-" (symbol-name type))) (prefix path target uninstall)
     (ignore prefix path target)
     ,@body))

(defmacro mason--bin-link! (path target)
  "Call `mason--link' with PATH and TARGET."
  `(if uninstall (mason--delete-file ,path)
     (mason--link ,path ,target t)
     (unless mason-dry-run
       (set-file-modes path (file-modes-symbolic-to-number "+x" (file-modes path))))))

(defmacro mason--bin-executable! (name dir &optional win-ext)
  "Binary resolver NAME that link to binary path inside DIR.
WIN-EXT is the extension to adds when on windows."
  `(mason--bin! ,name
     ,@(when win-ext
         `((when (mason--is-windows t)
             (setq path (concat path ,win-ext)
                   target (concat target ,win-ext)))))
     (if uninstall (mason--delete-file path)
       (mason--link path (mason--expand-child-file-name (concat ,dir "/" target) prefix)))))

(cl-defmacro mason--bin-wrapper! (content &optional &key env)
  "Call `mason--make-shell' with CONTENT and ENV."
  `(if uninstall (mason--delete-file path)
     (mason--make-shell path ,content :overwrite t :env ,env)))

(defmacro mason--bin-exec! (name &rest cmd)
  "Binary resolver NAME that creates wrapper for DIR/CMD with ENV."
  (unless (listp cmd) (setq cmd (list cmd)))
  `(mason--bin! ,name
     (mason--bin-wrapper! (list (mason--shell-exec) ,@cmd (mason--expand-child-file-name target prefix) (mason--shell-args)))))

(mason--bin! path (mason--bin-link! path (mason--expand-child-file-name target prefix)))

(mason--bin-exec! exec)
(mason--bin-exec! dotnet   "dotnet")
(mason--bin-exec! java-jar "java" "-jar")
(mason--bin-exec! node     "node")
(mason--bin-exec! php      "php")
(mason--bin-exec! python   "python3")
(mason--bin-exec! ruby     "ruby")

(mason--bin-executable! npm      "bin"        ".cmd")
(mason--bin-executable! cargo    "bin"        ".exe")
(mason--bin-executable! golang   "."          ".exe")
(mason--bin-executable! nuget    "."          ".exe")
(mason--bin-executable! luarocks "bin"        ".bat")
(mason--bin-executable! opam     "_opam/bin"  ".exe")
(mason--bin-executable! composer "vendor/bin" ".bat")

(mason--bin! pypi
  (let (extension (bin-dir "bin/"))
    (when (mason--is-windows 'cygwin) (setq extension ".exe"))
    (when (mason--is-windows) (setq bin-dir "Scripts/"))
    (mason--bin-link! (concat path extension)
                      (mason--expand-child-file-name (concat bin-dir target extension) prefix))))

(mason--bin! pyvenv
  (let ((python "bin/python"))
    (when (mason--is-cygwin) (setq python "bin/python.exe"))
    (when (mason--is-windows) (setq python "Scripts/python.exe"))
    (mason--bin-wrapper! `(,(mason--shell-exec)
                           ,(mason--expand-child-file-name python prefix)
                           "-m" ,target
                           ,(mason--shell-args)))))

(mason--bin! gem
  (when (mason--is-windows t)
    (setq target (concat target ".bat")))
  (mason--bin-wrapper! `(,(mason--shell-exec)
                         ,(mason--expand-child-file-name (concat "bin/" target) prefix)
                         ,(mason--shell-args))
                       :env `(("GEM_PATH" . ,(concat prefix path-separator (mason--shell-env "GEM_PATH"))))))


;; The Installer

(defvar mason--registry 'nan)
(defvar mason--installed 'nan)

(defun mason--assert-ensured ()
  "Assert if `mason--registry' is available."
  (when (or (eq mason--registry 'nan)
            (eq mason--installed 'nan))
    (mason--err "Call `mason-ensure' on your init.el"))
  (when (or (eq mason--registry 'on-process)
            (eq mason--installed 'on-process))
    (mason--err "Mason is not ready yet")))

(defvar mason--package-list nil)
(defun mason--get-package-list ()
  "Get list of mason packages."
  (mason--assert-ensured)
  (or mason--package-list
      (setq mason--package-list (mason--hash-keys mason--registry))))

(defvar mason--category-list nil)
(defun mason--get-category-list ()
  "Get list of mason categories."
  (mason--assert-ensured)
  (or
   mason--category-list
   (setq
    mason--category-list
    (let ((table (mason--make-hash "All" t)))
      (maphash (lambda (_k v)
                 (when-let* ((cat (gethash "categories" v)))
                   (mapc (lambda (c)
                           (puthash c t table))
                         cat)))
               mason--registry)
      (mason--hash-keys table)))))

(defvar mason--language-list nil)
(defun mason--get-language-list ()
  "Get list of mason languages."
  (mason--assert-ensured)
  (or
   mason--language-list
   (setq
    mason--language-list
    (let ((table (mason--make-hash "All" t)))
      (maphash (lambda (_k v)
                 (when-let* ((cat (gethash "languages" v)))
                   (mapc (lambda (c)
                           (puthash c t table))
                         cat)))
               mason--registry)
      (mason--hash-keys table)))))

(defun mason--update-installed ()
  "Update `mason--installed'."
  (let ((installed-index (mason--expand-child-file-name "packages/index" mason-dir))
        installed)
    (if (file-exists-p installed-index)
        (with-temp-buffer
          (insert-file-contents installed-index)
          (goto-char (point-min))
          (setq installed (read (current-buffer)))
          (mason--run-at-main
            (setq mason--installed installed)))
      (mason--run-at-main
        (setq mason--installed (mason--make-hash))))))

;;;###autoload
(defun mason-update-registry ()
  "Refresh the mason registry."
  (interactive)
  (setq mason--registry 'on-process
        mason--package-list nil
        mason--category-list nil
        mason--language-list nil)
  (mason--async
    (mason--update-target)
    (mason--update-installed)
    (let ((reg (mason--make-hash))
          (reg-dir (mason--expand-child-file-name "registry" mason-dir)))
      (when (file-directory-p reg-dir)
        (mason--delete-directory reg-dir t))
      (dolist (e mason-registries)
        (let* ((name (car e))
               (url (cdr e))
               (dest (mason--expand-child-file-name name reg-dir)))
          (make-directory dest t)
          (mason--download-maybe-extract url dest)
          (dolist (file (directory-files dest 'full "\\.json\\'"))
            (mason--msg "Reading registry %s" file)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (mapc (lambda (e)
                      (puthash "registry" name e)
                      (puthash (gethash "name" e) e reg))
                    (json-parse-buffer))))))
      (with-temp-file (mason--expand-child-file-name "index" reg-dir)
        (prin1 reg (current-buffer)))
      (mason--run-at-main
        (setq mason--registry reg)
        (mason--msg "Mason registry updated")))))

;;;###autoload
(defun mason-ensure ()
  "Ensure mason is setup."
  (let* ((bin-dir (mason--expand-child-file-name "bin" mason-dir))
         (reg-index (mason--expand-child-file-name "registry/index" mason-dir))
         (reg-time (file-attribute-modification-time (file-attributes reg-index)))
         reg-age)
    (setenv "PATH" (concat bin-dir ":" (getenv "PATH")))
    (add-to-list 'exec-path bin-dir)
    (if (null reg-time)
        (mason-update-registry)
      (setq reg-age (float-time (time-subtract (current-time) reg-time)))
      (if (> reg-age mason-registry-refresh-time)
          (mason-update-registry)
        (mason--async
          (mason--update-target)
          (mason--update-installed)
          (let (reg)
            (with-temp-buffer
              (insert-file-contents reg-index)
              (goto-char (point-min))
              (setq reg (read (current-buffer))))
            (mason--run-at-main
              (setq mason--registry reg)
              (mason--msg "Mason ready"))))))))

(defvar mason--ask-package-prompt nil)
(defvar mason--ask-package-callback nil)
(defvar mason--ask-package-category nil)
(defvar mason--ask-package-language nil)
(defvar mason--ask-package-filter-function nil)
(defvar mason--ask-package-input nil)

(define-prefix-command 'mason-filter-map)
(defvar-keymap mason--ask-package-transient-map
  "M-m" 'mason-filter-map)

(defmacro mason--filter! (key name &rest body)
  "Create a filter function NAME of BODY, assign it to KEY."
  (declare (indent 2))
  `(progn
     (defun ,name (fake)
       (interactive (list t) nil)
       (setq mason--ask-package-filter-function nil)
       (unless (and mason--ask-package-prompt
                    mason--ask-package-callback)
         (user-error "Must not be called manually"))
       (when fake
         (setq mason--ask-package-filter-function ',name)
         (exit-minibuffer))
       ,@body
       (mason--ask-package-0))
     (keymap-set 'mason-filter-map ,key #',name)))

(mason--filter! "c" mason-filter-category
  (let* ((completion-extra-properties nil)
         (cat (completing-read "Category: " (mason--get-category-list) nil t)))
    (unless (string-empty-p cat)
      (setq mason--ask-package-category (if (string= cat "All") nil cat)))))

(mason--filter! "d" mason-toggle-deprecated
  (setq mason-show-deprecated (not mason-show-deprecated)))

(mason--filter! "l" mason-filter-language
  (let* ((completion-extra-properties nil)
         (lang (completing-read "Language: " (mason--get-language-list) nil t)))
    (unless (string-empty-p lang)
      (setq mason--ask-package-language (if (string= lang "All") nil lang)))))

(defun mason--ask-package-affixation-function (pkgs)
  "Affixation function for PKGS to be used with `completion-extra-properties'."
  (when pkgs
    (let* ((lens (mapcar (lambda (p) (length p)) pkgs))
           (margin (max (+ (apply #'max lens) 4) 20)))
      (mapcar
       (lambda (name)
         (let* ((len (length name))
                (spaces (make-string (- margin len) ?\s))
                (pkg (gethash name mason--registry))
                (desc (gethash "description" pkg))
                (desc (replace-regexp-in-string "\n" " " desc))
                (deprecated (gethash "deprecation" pkg))
                (installed (gethash name mason--installed)))
           (list (if deprecated (propertize name 'face 'mason-deprecated) name)
                 nil
                 (concat
                  spaces
                  (when installed (propertize "[Installed] " 'face 'success))
                  (when deprecated (propertize "[Deprecated] " 'face 'mason-deprecated))
                  (propertize desc 'face (if deprecated 'mason-deprecated 'font-lock-doc-face))))))
       pkgs))))

(defun mason--ask-package (prompt callback)
  "Ask for package with PROMPT, call CALLBACK with the selected package name."
  (unless (and prompt callback)
    (user-error "Called without prompt and callback"))
  (setq mason--ask-package-prompt prompt
        mason--ask-package-callback callback)
  (unwind-protect (mason--ask-package-0)
    (setq mason--ask-package-prompt nil
          mason--ask-package-callback nil
          mason--ask-package-category nil
          mason--ask-package-language nil
          mason--ask-package-filter-function nil
          mason--ask-package-input nil)))

(defun mason--ask-package-0 ()
  "Implementation for `mason--ask-package'."
  (set-transient-map mason--ask-package-transient-map (lambda () (minibufferp)))
  (let* ((cat mason--ask-package-category)
         (lang mason--ask-package-language)
         (filter-key (where-is-internal 'mason-filter-map mason--ask-package-transient-map 'firstonly))
         (completion-extra-properties '(:affixation-function mason--ask-package-affixation-function))
         (pkg
          (minibuffer-with-setup-hook
              (lambda () (when filter-key (message "%s to open menu" (key-description filter-key))))
            (completing-read
             (concat
              mason--ask-package-prompt
              (cond ((and cat lang) (format " (C:%s, L:%s)" cat lang))
                    (cat (format " (C:%s)" cat))
                    (lang (format " (L:%s)" lang))
                    (t nil))
              ": ")
             (seq-filter
              (lambda (p)
                (let* ((pkg (gethash p mason--registry))
                       (cats (gethash "categories" pkg []))
                       (langs (gethash "languages" pkg []))
                       (deprecation (gethash "deprecation" pkg)))
                  (and (or mason-show-deprecated
                           (null deprecation))
                       (or (null cat)
                           (seq-contains-p cats cat))
                       (or (null lang)
                           (seq-contains-p langs lang)))))
              (mason--get-package-list))
             nil t mason--ask-package-input))))
    (if (or (string-empty-p pkg) mason--ask-package-filter-function)
        (progn
          (setq mason--ask-package-input pkg)
          (funcall mason--ask-package-filter-function nil))
      (funcall mason--ask-package-callback pkg))))

;;;###autoload
(defun mason-spec (package &optional interactive)
  "Visit Mason spec file for PACKAGE.
If INTERACTIVE, ask for PACKAGE."
  (interactive '(nil nil))
  (if (and package (not interactive))
      (mason--spec-0 package)
    (mason--ask-package "Mason Spec" #'mason--spec-0)))

(defun mason--spec-0 (package)
  "Implementation of `mason-spec' PACKAGE."
  (if-let* ((spec (gethash package mason--registry))
            (buf (get-buffer-create (format "*mason spec of %s*" package))))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer)
        (goto-char (point-min))
        (insert (json-serialize spec))
        (json-pretty-print-buffer)
        (js-json-mode)
        (read-only-mode 1)
        (pop-to-buffer buf))
    (error "Invalid package `%s'" package)))

;;;###autoload
(defun mason-install (package &optional force interactive)
  "Install a Mason PACKAGE.
If FORCE non nil delete existing installation, if exists.
If INTERACTIVE, ask for PACKAGE and FORCE."
  (interactive '(nil nil t))
  (if (and package (not interactive))
      (mason--install-0 package force nil nil)
    (mason--ask-package "Mason Install"
                        (lambda (p) (mason--install-0 p nil t nil)))))

;;;###autoload
(defun mason-uninstall (package &optional interactive)
  "Uninstall a Mason PACKAGE.
If INTERACTIVE, ask for PACKAGE"
  (interactive '(nil t))
  (if (= (hash-table-count mason--installed) 0)
      (mason--msg "No package has been installed")
    (let ((mason--registry mason--installed)
          mason--package-list mason--category-list mason--language-list)
      (if (and package (not interactive))
          (mason--install-0 package nil nil t)
        (mason--ask-package "Mason Uninstall"
                            (lambda (p) (mason--install-0 p nil t t)))))))

(defun mason--install-0 (package force interactive uninstall)
  "Implementation of `mason-install' and `mason-uninstall'.
Args: PACKAGE FORCE INTERACTIVE UNINSTALL."
  (let* ((spec (gethash package mason--registry))
         (spec (read (prin1-to-string spec))) ; deep copy
         (name (gethash "name" spec))
         (deprecation (gethash "deprecation" spec))
         (packages-dir (mason--expand-child-file-name "packages" mason-dir))
         (package-dir (file-name-as-directory (mason--expand-child-file-name name packages-dir)))
         ;; source
         (source (gethash "source" spec))
         (source-id-raw (gethash "id" source))
         (source-id (mason--parse-purl source-id-raw))
         (source-type (if uninstall "uninstall" (gethash "type" source-id)))
         (source-fn (intern (concat "mason--source-" source-type)))
         ;; links
         (bin (gethash "bin" spec))
         (share (gethash "share" spec))
         (opt (gethash "opt" spec)))
    (mason--expect-hash-key spec
                            "name" "description" "homepage" "deprecation"
                            "licenses" "languages" "categories"
                            "source" "bin" "share" "opt" "schemas"
                            "registry" "neovim" "ci_skip")
    (when (and deprecation interactive (not uninstall))
      (unless (y-or-n-p (format-message
                         "Package `%s' is deprecated since `%s' with the message:\n\t%s\nInstall anyway? "
                         name (gethash "since" deprecation) (gethash "message" deprecation)))
        (error "Cancelled")))
    (if uninstall (mason--msg "Uninstalling package `%s'" name)
      (mason--msg "Installing package `%s' from source `%s'" name (url-unhex-string source-id-raw)))
    (when (not (fboundp source-fn))
      (mason--err "Unsupported source type `%s' in id `%s'" source-type source-id-raw))
    (when (and (not uninstall)
               (not mason-dry-run)
               (file-directory-p package-dir)
               (not (directory-empty-p package-dir)))
      (if (not interactive)
          (if force (mason--delete-directory package-dir t)
            (mason--err "Directory %s already exists" package-dir))
        (if (y-or-n-p (format-message "Directory %s exists, delete? " package-dir))
            (mason--delete-directory package-dir t)
          (error "Cancelled"))))
    (funcall
     source-fn name package-dir source-id source spec
     (lambda ()
       (when bin
         (maphash
          (lambda (key val-raw)
            (setq val-raw (mason--expand (mason--expand val-raw spec) source-id))
            (let* ((val (mason--parse-bin val-raw))
                   (bin-type (gethash "type" val))
                   (bin-path (gethash "path" val))
                   (bin-fn (intern (concat "mason--bin-" bin-type))))
              (when (or (null val) (not (fboundp bin-fn)))
                (mason--err "Unsupported binary `%s'" val-raw))
              (let* ((bin-dir (mason--expand-child-file-name "bin" mason-dir))
                     (bin-link (mason--expand-child-file-name key bin-dir)))
                (mason--msg "Resolving binary `%s'" val-raw)
                (funcall bin-fn package-dir bin-link bin-path uninstall))))
          bin))
       (when share (mason--link-share-opt "share" share spec source-id package-dir uninstall))
       (when opt (mason--link-share-opt "opt" opt spec source-id package-dir uninstall))
       (unless mason-dry-run
         (if uninstall (remhash name mason--installed)
           (puthash name spec mason--installed))
         (with-temp-file (mason--expand-child-file-name "index" packages-dir)
           (prin1 mason--installed (current-buffer))))
       (mason--msg "%s `%s'" (if uninstall "Uninstalled" "Installed") name)))))

(defun mason--link-share-opt (dest-dir table spec source-id package-dir uninstall)
  "Link share or opt DEST-DIR from hash TABLE relative to PACKAGE-DIR.
Expand TABLE from SPEC and SOURCE-ID, if necessary."
  (mason--msg "Symlinking %s" dest-dir)
  (setq dest-dir (mason--expand-child-file-name dest-dir mason-dir))
  (maphash
   (lambda (link-dest link-source)
     (setq link-source (mason--expand-child-file-name
                        (mason--expand (mason--expand link-source spec) source-id)
                        package-dir)
           link-dest (expand-file-name link-dest dest-dir))
     (cond
      ;; link/dest/: link/source/
      ((directory-name-p link-dest)
       (unless (directory-name-p link-source)
         (mason--err "Link source `%s' is not a directory" link-source))
       (unless mason-dry-run
         (unless (file-directory-p link-source)
           (mason--err "Link source `%s' does not exist" link-source))
         (make-directory link-dest t)
         (dolist (entry (directory-files link-source nil directory-files-no-dot-files-regexp))
           (let ((entry-dest (mason--expand-child-file-name entry link-dest))
                 (entry-source (mason--expand-child-file-name entry link-source)))
             (if uninstall (mason--delete-file entry-dest)
               (mason--link entry-dest entry-source t))))
         (when (and uninstall (directory-empty-p link-dest))
           (mason--delete-directory link-dest))))
      ;; link/dest: link/source
      (t
       (when (directory-name-p link-source)
         (mason--err "Link source `%s' is a directory" link-source))
       (unless mason-dry-run
         (unless (file-exists-p link-source)
           (mason--err "Link source `%s' does not exist" link-source))
         (make-directory (file-name-parent-directory link-dest) t)
         (if uninstall (mason--delete-file link-dest)
           (mason--link link-dest link-source t))))))
   table))

(defun mason-dry-run-install (package)
  "Dry run install a PACKAGE."
  (let ((prev-dry-run mason-dry-run)
        (prev-mason-dir mason-dir))
    (setq mason-dry-run t
          mason-dir (make-temp-file "mason-dry-run-" 'dir))
    (unwind-protect
        (mason-install package)
      (delete-directory mason-dir)
      (setq mason-dry-run prev-dry-run
            mason-dir prev-mason-dir)
      (mason-ensure))))

(defun mason-dry-run-install-all ()
  "Dry run install all packages."
  (let ((prev-dry-run mason-dry-run)
        (prev-mason-dir mason-dir)
        (total-count (length (mason--get-package-list)))
        (success-count 0))
    (setq mason-dry-run t
          mason-dir (make-temp-file "mason-dry-run-" 'dir))
    (unwind-protect
        (progn
          (with-current-buffer (mason-buffer)
            (read-only-mode -1)
            (erase-buffer)
            (read-only-mode 1))
          (mason--msg "Started dry run test in `%s'" mason-dir)
          (dolist (pkg (mason--get-package-list))
            (condition-case err
                (progn
                  (mason-install pkg)
                  (setq success-count (1+ success-count)))
              (error
               (unless (plist-get err :mason)
                 (mason--msg "EXTERNAL ERROR: %s" (error-message-string err))))))
          (mason--msg "Installed %d/%d packages" success-count total-count))
      (delete-directory mason-dir)
      (setq mason-dry-run prev-dry-run
            mason-dir prev-mason-dir)
      (mason-ensure))))

(provide 'mason)
;;; mason.el ends here
