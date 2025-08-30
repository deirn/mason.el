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
(require 'seq)
(require 's)

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
  "How long in seconds the before trying to refresh the registry.  Defaults to 1 week."
  :type 'integer :group 'mason)

(defcustom mason-registries
  '(("mason" . "https://github.com/mason-org/mason-registry/releases/latest/download/registry.json.zip"))
  "Alist of registry name and registry json archive link."
  :type '(alist :key-type string :value-type string)
  :group 'mason)

;; FIXME: Auto detect?
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
  (apply #'error format args))

(defun mason--uerr (format &rest args)
  "Call (mason--msg FORMAT ARGS) before throwing `user-error'."
  (apply #'mason--msg (concat "USER-ERROR: " format) args)
  (apply #'user-error format args))

(defun mason--quote (str)
  "Quote STR if it contains spaces."
  (if (string-match-p "[[:space:]]" str)
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

(cl-defun mason--process (cmd &optional callback)
  "Run process CMD asynchronously, with optional CALLBACK.
CMD is argument list as specified in `make-process' :command."
  (declare (indent defun))
  (let ((msg (mapconcat #'mason--quote cmd " "))
        buffer)
    (mason--msg "Calling `%s'" msg)
    (when mason-dry-run
      (when (functionp callback) (funcall callback))
      (cl-return-from mason--process nil))
    (setq buffer (generate-new-buffer "*mason process*"))
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
             (funcall callback))))))))

(cl-defmacro mason--process-lamda (_cmd &optional _callback)
  "Wrap `mason--process' inside lambda."
  (declare (indent defun))
  `(lambda () (mason--process ,_cmd ,_callback)))

(cl-defun mason--process-sync (cmd)
  "Run CMD with ARGS synchronously."
  (let ((msg (mapconcat #'mason--quote cmd " "))
        buffer status success)
    (mason--msg "Calling `%s'" msg)
    (when mason-dry-run (cl-return-from mason--process-sync nil))
    (setq buffer (generate-new-buffer "*mason process*"))
    (with-current-buffer buffer
      (setq status (apply #'call-process (car cmd) nil t nil (cdr cmd)))
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

(defun mason--dir-empty-p (dir)
  "Return t if DIR exists and contains no non-dot files."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp))))

(defun mason--path-descendant-p (path base &optional ignore-case)
  "Return t if PATH is equal to or underneath BASE.
Both PATH and BASE are expanded (`file-truename'); trailing separators are ignored.

If IGNORE-CASE is non-nil, comparison is case-insensitive."
  (let* ((p (directory-file-name (file-truename path)))
         (b (directory-file-name (file-truename base))))
    (string-prefix-p (file-name-as-directory b)
                     (file-name-as-directory p)
                     ignore-case)))

(defmacro mason--make-hash (test &rest kvs)
  "Make a hash table with TEST ('equal, 'eq, etc.) populated with KVS pairs."
  (declare (indent defun))
  `(let ((h (make-hash-table :test ,test)))
     ,@(cl-loop for (k v) on kvs by #'cddr
                collect `(puthash ,k ,v h))
     h))

(defun mason--nnlist (&rest entries)
  "Make list of non nil ENTRIES."
  (let ((res))
    (dolist (e (nreverse entries) res)
      (when e (push e res)))))

(defun mason--parse-yaml (path)
  "Parse mason package spec from PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (yaml-parse-string (buffer-string))))

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
    (let ((nv-split (s-split-up-to "@" name 1 t)))
      (setq name (nth 0 nv-split)
            version (nth 1 nv-split)))
    (let ((ns-split (s-split-up-to "/" name 1 t)))
      (when (length= ns-split 2)
        (setq namespace (url-unhex-string (nth 0 ns-split))
              name (nth 1 ns-split)))
      (setq name (url-unhex-string name)))
    (setq purl (mason--make-hash 'equal
                 "raw" string
                 "scheme" scheme
                 "type" type
                 "namespace" namespace
                 "name" name
                 "version" version
                 "qualifiers" nil
                 "subpath" subpath))
    (when q-str
      (setq qualifiers (mason--make-hash 'equal))
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
      (mason--make-hash 'equal
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
  "Expand STR according to SPEC."
  (let* ((dollar (replace-regexp-in-string "{{\\([^}]+\\)}}" "${\\1}" str))
         (expanded
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
               (dolist (op ops var)
                 (cond
                  ((string-prefix-p "strip_prefix " op)
                   (let* ((prefix (mason--unquote-string-or-nil (string-trim (substring op (length "strip_prefix "))))))
                     (unless prefix
                       (mason--err "Unable to expand `%s': strip_prefix can only accept one argument of type string" str))
                     (unless (string-prefix-p prefix var)
                       (mason--err "Unable to expand `%s': strip_prefix: `%s' is not prefixed with `%s'" str var prefix))
                     (setq var (substring var (length prefix)))))
                  (t (mason--err "Unable to expand `%s': unsupported operation `%s'" str op)))))))))
    (mason--msg "Expanded `%s' to `%s'" str expanded)
    expanded))

(cl-defun mason--download (url newname &optional ok-if-already-exists)
  "Copy URL to NEWNAME.
OK-IF-ALREADY-EXISTS is the same in `url-copy-file'."
  (mason--msg "Downloading %s to %s" url newname)
  (or mason-dry-run (url-copy-file url newname ok-if-already-exists)))

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
    (unless mason-dry-run
      (make-directory dest t))
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
    (unwind-protect
        (let ((status (mason--download url tmp t)))
          (unless status
            (mason--err "Download failed: %s" url))
          (mason--extract tmp dest))
      (when (file-exists-p tmp) (ignore-errors (delete-file tmp))))))



(cl-defmacro mason--source! (type (&key namespace version qualifiers subpath) &rest body)
  "Define a mason source resolver for TYPE.

Each keys declare support for PURL member:
:NAMESPACE  none, optional or must
:VERSION    none, optional or must
:QUALIFIERS none or (\"q1\" \"q2\")
:SUBPATH    none, optional or must

- If none, ID can not have the member.
- If optional, ID can have the member.
- If must, ID must have the member.
- Special for :qualifiers, if list, ID can only have
  qualifiers from the specified list.

Inside BODY, one can reference:
- NAME is the name of the mason entry.
- ID is the entire id `mason--purl-p' struct.
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
            `(unless (seq-every-p (lambda (q) (member q m-qualifiers)) id-qualifiers)
               (mason--err "`%s': `%s' must only have qualifiers of key %S" ,fn-name id-raw m-qualifiers)))
           (t (error "`%s': :qualifiers must be none or list" fn-name)))))
    ;; resulting function
    `(defun ,(intern fn-name) (name prefix id source spec next)
       (let* ((m-qualifiers ',qualifiers)
              (id-raw        (gethash "raw" id))
              (id-scheme     (gethash "scheme" id))
              (id-type       (gethash "type" id))
              (id-namespace  (gethash "namespace" id))
              (id-name       (gethash "name" id))
              (id-version    (gethash "version" id))
              (id-qualifiers (gethash "qualifiers" id))
              (id-subpath    (gethash "subpath" id)))
         ,p-namespace
         ,p-version
         ,p-subpath
         ,p-qualifiers
         ,@body))))

(mason--source! cargo (:namespace none
                       :version must
                       :qualifiers none
                       :subpath none)
  (mason--process `("cargo" "install"
                    "--root" ,prefix
                    ,(concat id-name "@" id-version))
    next))

(mason--source! pypi (:namespace none
                      :version must
                      :qualifiers none
                      :subpath none)
  (mason--process `("python" "-m" "venv" ,prefix)
    (mason--process-lamda `("pip"
                            "--python" ,(expand-file-name "bin/python" prefix)
                            "install"
                            "--prefix" ,prefix
                            ,(concat id-name "==" id-version))
      next)))

(mason--source! npm (:namespace optional
                     :version must
                     :qualifiers none
                     :subpath none)
  (mason--process `("npm" "install" "-g"
                    "--prefix" prefix
                    ,(concat
                      id-namespace (when id-namespace "/")
                      id-name "@" id-version))
    next))

(defconst mason--github-file-regexp
  (concat "^"
          "\\([A-ZA-Z0-9_.-]+\\)"  ; 1. file path
          "\\("                    ; 2. optional
          ":"
          "\\([A-ZA-Z0-9_./-]+\\)" ; 3. extract path
          "\\)?"
          "$"))

(mason--source! github (:namespace must
                        :version must
                        :qualifiers none
                        :subpath none)
  (let ((asset (gethash "asset" source)))
    (unless asset (mason--err "Missing asset"))
    (when (vectorp asset)
      (unless mason-target (mason--err "Customize `mason-target' first"))
      (setq asset (seq-find (lambda (a)
                              (string-prefix-p (gethash "target" a) mason-target))
                            asset))
      (unless asset (mason--err "No matching asset for target %s" mason-target))
      (puthash "asset" asset source))
    (let* ((file (gethash "file" asset))
           (file (mason--expand file id)))
      (unless (string-match mason--github-file-regexp file)
        (mason--err "Unsupported file asset `%s'" file))
      (let* ((file-path (match-string 1 file))
             (file-url (concat "https://github.com/" id-namespace "/" id-name "/releases/download/" id-version "/" file-path))
             (extract-path (match-string 3 file))
             (extract-dest (if extract-path (expand-file-name extract-path prefix) prefix)))
        (unless (mason--path-descendant-p extract-dest prefix)
          (mason--err "Path `%s' is not inside `%s'" extract-dest prefix))
        (mason--async
          (mason--download-extract file-url extract-dest)
          (mason--run-at-main (funcall next)))))))



(defmacro mason--bin! (type &rest body)
  "Define a mason binary resolver for TYPE.
BODY is `progn' body.

Inside BODY, one can reference PATH and PREFIX.

PATH is the relative path of the binary.
PREFIX is where the package should've been installed."
  (declare (indent defun))
  `(defun ,(intern (concat "mason--bin-" (symbol-name type))) (name path prefix)
     ,@body))

(mason--bin! path (expand-file-name path prefix))
(mason--bin! exec (expand-file-name path prefix))
(mason--bin! cargo (expand-file-name (concat "bin/" path) prefix))
(mason--bin! pypi (expand-file-name (concat "bin/" path) prefix))
(mason--bin! npm (expand-file-name (concat "bin/" path) prefix))



(defvar mason--registry 'nan)
(defun mason--assert-ensured ()
  "Assert if `mason--registry' is available."
  (when (eq mason--registry 'nan) (mason--err "Call `mason-ensure' on your init.el"))
  (when (eq mason--registry 'on-process) (mason--err "Mason is not ready yet")))

(defun mason--hash-keys (table)
  "Get list of keys from TABLE."
  (let (keys)
    (maphash (lambda (k _v) (push k keys)) table)
    (nreverse keys)))

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
    (let ((table (mason--make-hash 'equal)))
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
    (let ((table (mason--make-hash 'equal)))
      (maphash (lambda (_k v)
                 (when-let* ((cat (gethash "languages" v)))
                   (mapc (lambda (c)
                           (puthash c t table))
                         cat)))
               mason--registry)
      (mason--hash-keys table)))))

;;;###autoload
(defun mason-update-registry ()
  "Refresh the mason registry."
  (interactive)
  (setq mason--registry 'on-process
        mason--package-list nil
        mason--category-list nil
        mason--language-list nil)
  (mason--async
    (let ((reg (mason--make-hash 'equal))
          (reg-dir (expand-file-name "registry" mason-dir)))
      (delete-directory reg-dir t nil)
      (dolist (e mason-registries)
        (let* ((name (car e))
               (url (cdr e))
               (dest (expand-file-name name reg-dir)))
          (mason--download-extract url dest)
          (dolist (file (directory-files dest 'full "\\.json\\'"))
            (mason--msg "Reading registry %s" file)
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (mapc (lambda (e)
                      (puthash "registry" name e)
                      (puthash (gethash "name" e) e reg))
                    (json-parse-buffer))))))
      (with-temp-file (expand-file-name "index" reg-dir)
        (prin1 reg (current-buffer)))
      (mason--run-at-main
        (setq mason--registry reg)
        (mason--msg "Mason registry updated")))))

;;;###autoload
(defun mason-ensure ()
  "Ensure mason is setup."
  (let* ((bin-dir (expand-file-name "bin" mason-dir))
         (reg-index (expand-file-name "registry/index" mason-dir))
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

(defun mason-filter-category (fake)
  "Ask for category and filter current package completion list.
If FAKE, throw this function to be called again."
  (interactive (list t))
  (unless (and mason--ask-package-prompt
               mason--ask-package-callback)
    (exit-minibuffer)
    (user-error "Must not be called manually"))
  (namespace (plist-get support :namespace))
  (when fake
    (setq mason--ask-package-filter-function 'mason-filter-category)
    (exit-minibuffer))
  (let* ((completion-extra-properties nil)
         (cat (completing-read "Category: " (mason--get-category-list) nil t)))
    (unless (string-empty-p cat)
      (setq mason--ask-package-category cat)))
  (mason--ask-package-0))

(defun mason-filter-language (fake)
  "Ask for language and filter current package completion list.
If FAKE, throw this function to be called again."
  (interactive (list t))
  (unless (and mason--ask-package-prompt
               mason--ask-package-callback)
    (user-error "Must not be called manually"))
  (when fake
    (setq mason--ask-package-filter-function 'mason-filter-language)
    (exit-minibuffer))
  (let* ((completion-extra-properties nil)
         (lang (completing-read "Language: " (mason--get-language-list) nil t)))
    (unless (string-empty-p lang)
      (setq mason--ask-package-language lang)))
  (mason--ask-package-0))

(defvar-keymap mason-filter-map
  "c" #'mason-filter-category
  "l" #'mason-filter-language)

(defvar-keymap mason--ask-package-transient-map
  "M-m" mason-filter-map)

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
                (desc (replace-regexp-in-string "\n" " " desc)))
           (list name nil
                 (concat
                  spaces
                  (propertize desc 'face 'font-lock-doc-face))))
         )
       pkgs))))

(defun mason--ask-package (prompt callback)
  "Ask for package with PROMPT, call CALLBACK with the selected package name, if any."
  (unless (and prompt callback)
    (user-error "Called without prompt and callback"))
  (setq mason--ask-package-prompt prompt
        mason--ask-package-callback callback)
  (unwind-protect (mason--ask-package-0)
    (setq mason--ask-package-prompt nil
          mason--ask-package-callback nil
          mason--ask-package-category nil
          mason--ask-package-language nil
          mason--ask-package-filter-function nil)))

(defun mason--ask-package-0 ()
  "Implementation for `mason--ask-package'."
  (set-transient-map mason--ask-package-transient-map (lambda () (minibufferp)))
  (let* ((cat mason--ask-package-category)
         (lang mason--ask-package-language)
         (completion-extra-properties '(:affixation-function mason--ask-package-affixation-function))
         (pkg
          (completing-read
           (concat
            mason--ask-package-prompt
            (cond ((and cat lang) (format " (Category: %s, Language: %s)" cat lang))
                  (cat (format " (Category: %s)" cat))
                  (lang (format " (Language: %s)" lang))
                  (t nil))
            ": ")
           (seq-filter
            (lambda (p)
              (let* ((pkg (gethash p mason--registry))
                     (cats (gethash "categories" pkg []))
                     (langs (gethash "languages" pkg [])))
                (and (or (null cat)
                         (seq-contains cats cat))
                     (or (null lang)
                         (seq-contains langs lang)))))
            (mason--get-package-list))
           nil t)))
    (if (string-empty-p pkg)
        (when mason--ask-package-filter-function
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
      (mason--install-0 package force nil)
    (mason--ask-package "Mason Install"
                        (lambda (p) (mason--install-0 p nil t)))))

(defun mason--install-0 (package force interactive)
  "Implementation of `mason-install'.
Args: PACKAGE FORCE INTERACTIVE."
  (let* ((spec (gethash package mason--registry))
         (name (gethash "name" spec))
         (packages-dir (expand-file-name "packages" mason-dir))
         (package-dir (expand-file-name name packages-dir))
         ;; source
         (source (gethash "source" spec))
         (source-id-raw (gethash "id" source))
         (source-id (mason--parse-purl source-id-raw))
         (source-supported-platforms (gethash "supported_platforms" source))
         (source-type (gethash "type" source-id))
         (source-fn (intern (concat "mason--source-" source-type)))
         ;; links
         (bin (gethash "bin" spec))
         (share (gethash "share" spec))
         (opt (gethash "opt" spec)))
    (unless (mason--path-descendant-p package-dir packages-dir)
      (mason--err "Path `%s' is not inside `%s'" package-dir packages-dir))
    (when (not (fboundp source-fn))
      (mason--err "Unsupported source type %s in id %s" source-type source-id-raw))
    (when (and (not mason-dry-run)
               (file-directory-p package-dir)
               (not (directory-empty-p package-dir)))
      (if (not interactive)
          (if force
              (progn (mason--msg "Deleting %s" package-dir)
                     (delete-directory package-dir t nil))
            (mason--err "Directory %s already exists" package-dir))
        (if (y-or-n-p (format-message "Directory %s exists, delete? " package-dir))
            (progn (mason--msg "Deleting %s" package-dir)
                   (delete-directory package-dir t nil))
          (error "Cancelled"))))
    (mason--msg "Installing `%s' using `%s'" (url-unhex-string source-id-raw) source-fn)
    (funcall
     source-fn name package-dir source-id source spec
     (lambda ()
       (maphash (lambda (key val-raw)
                  (setq val-raw (mason--expand val-raw spec))
                  (let* ((val (mason--parse-bin val-raw))
                         (bin-type (gethash "type" val))
                         (bin-path (gethash "path" val))
                         (bin-fn (intern (concat "mason--bin-" bin-type))))
                    (when (or (null val) (not (fboundp bin-fn)))
                      (mason--err "Unsupported binary `%s'" val-raw))
                    (let* ((bin-source (funcall bin-fn name bin-path package-dir))
                           (bin-dir (expand-file-name "bin" mason-dir))
                           (bin-link (expand-file-name key bin-dir)))
                      (mason--msg "Symlinking binary `%s' using `%s'" val-raw bin-fn)
                      (unless (mason--path-descendant-p bin-source package-dir)
                        (mason--err "Path `%s' is not inside `%s'" bin-source package-dir))
                      (unless (mason--path-descendant-p bin-link bin-dir)
                        (mason--err "Path `%s' is not inside `%s'" bin-link bin-dir))
                      (unless mason-dry-run
                        (make-directory bin-dir t)
                        (when (file-exists-p bin-link)
                          (delete-file bin-link))
                        (make-symbolic-link bin-source bin-link))
                      (mason--msg "Symlinked `%s' to `%s'" bin-link bin-source))))
                bin)
       (when share (mason--link-share-opt "share" share spec package-dir))
       (when opt (mason--link-share-opt "opt" opt spec package-dir))
       (mason--msg "Installed `%s'" (url-unhex-string source-id-raw))))))

(defun mason--link-share-opt (dest-dir table spec package-dir)
  "Link share or opt DEST-DIR from hash TABLE relative to PACKAGE-DIR.
Expand TABLE from SPEC, if necessary."
  (mason--msg "Symlinking %s")
  (setq dest-dir (expand-file-name dest-dir mason-dir))
  (maphash
   (lambda (link-dest link-source)
     (setq link-source (mason--expand link-source spec)
           link-dest (expand-file-name link-dest dest-dir))
     (unless (mason--path-descendant-p link-source package-dir)
       (mason--err "Path `%s' is not inside `%s'" link-source package-dir))
     (unless (mason--path-descendant-p link-dest dest-dir)
       (mason--err "Path `%s' is not inside `%s'" link-dest dest-dir))
     (cond
      ;; link/dest/: link/source/
      ((directory-name-p link-dest)
       (unless (directory-name-p link-source)
         (mason--err "Link source `%s' is not a directory" link-source))
       (mason--msg "Symlinking anything inside `%s' to `%s'" link-source link-dest)
       (unless mason-dry-run
         (unless (file-directory-p link-source)
           (mason--err "Link source `%s' does not exist" link-source))
         (make-directory link-dest t)
         (dolist (entry (directory-files link-source nil directory-files-no-dot-files-regexp))
           (let ((entry-dest (expand-file-name entry link-dest))
                 (entry-source (expand-file-name entry link-source)))
             (make-symbolic-link entry-source entry-dest)
             (mason--msg "Symlinked `%s' to `%s'" entry-dest entry-source)))))
      ;; link/dest: link/source
      (t
       (when (directory-name-p link-source)
         (mason--err "Link source `%s' is a directory" link-source))
       (unless (file-exists-p link-source)
         (mason--err "Link source `%s' does not exist" link-source))
       (unless mason-dry-run
         (make-directory (file-name-parent-directory link-dest))
         (make-symbolic-link link-source link-dest))
       (mason--msg "Symlinked `%s' to `%s'" link-dest link-source))))
   table))

(defun mason-dry-run-install-all ()
  "Dry run install all packages."
  (mason-ensure)
  (let ((prev-dry-run mason-dry-run)
        (prev-mason-dir mason-dir)
        (total-count (length (mason--get-package-list)))
        (success-count 0))
    (setq mason-dry-run t
          mason-dir (make-temp-file "mason-dry-run-" 'dir))
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
        (error nil)))
    (prog1 (mason--msg "Installed %d/%d packages" success-count total-count)
      (delete-directory mason-dir)
      (setq mason-dry-run prev-dry-run
            mason-dir prev-mason-dir))))

(provide 'mason)
;;; mason.el ends here
