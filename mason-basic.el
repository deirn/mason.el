;;; mason-basic.el --- Basic utilities for mason.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Basic utilities for mason.el.
;; This contains functions that are "safe" to use in `emacs -Q --batch --eval'.

;;; Code:

(require 'url-parse)

(defgroup mason nil
  "Installer for LSP servers, DAP servers, linters, and formatters."
  :prefix "mason-"
  :group 'tools)

(defcustom mason-dry-run nil
  "If not nil, only print messages what mason would do."
  :type 'boolean :group 'mason)


;; Logging

(define-derived-mode mason-log-mode special-mode "Mason Log"
  :interactive nil)

(defconst mason-buffer " *mason*")
(defun mason-buffer ()
  "Get mason buffer."
  (or (get-buffer mason-buffer)
      (with-current-buffer (get-buffer-create mason-buffer)
        (mason-log-mode)
        (read-only-mode 1)
        (current-buffer))))

;;;###autoload
(defun mason-log ()
  "Show the Mason Log buffer."
  (interactive)
  (pop-to-buffer (mason-buffer)))

(defun mason--echo (format &rest args)
  "Add message FORMAT ARGS to echo area."
  (let ((message-log-max nil))
    (message format args)))

(defun mason--log (face prefix format args)
  "Log with FACE, PREFIX, FORMAT, and ARGS."
  (let ((formatted (apply #'format-message format args)))
    (message "%s" formatted)
    (with-current-buffer (mason-buffer)
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (propertize (format-time-string "[%F %T] ") 'face 'mason-log-time))
      (when mason-dry-run (insert (propertize "[DRY] " 'face 'mason-log-time)))
      (insert (propertize (concat prefix formatted) 'face face) "\n")
      (read-only-mode 1))
    formatted))

(defun mason--info (format &rest args)
  "Log FORMAT ARGS with info level."
  (mason--log 'mason-log-info "" format args))

(defun mason--warn (format &rest args)
  "Log FORMAT ARGS with warn level."
  (mason--log 'mason-log-warn "WARNING: " format args))

(defun mason--error (format &rest args)
  "Log FORMAT ARGS with error level."
  (mason--log 'mason-log-error "ERROR: " format args))

(defun mason--success (format &rest args)
  "Log FORMAT ARGS with info level."
  (mason--log 'mason-log-success "" format args))


;; Processes

(defun mason--quote (str &optional always)
  "Quote STR if it contains spaces or if ALWAYS non nil."
  (if (or always (string-match-p "[[:space:]]" str))
      (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" str))
    str))

(defmacro mason--process-output! ()
  "Copy output of process from BUFFER to buffer command `mason-buffer'."
  `(progn
     (if success (mason--info "`%s' finished with status %s" msg status)
       (mason--error "`%s' failed with status %s" msg status))
     (with-current-buffer (mason-buffer)
       (let ((start (point-max)))
         (read-only-mode -1)
         (goto-char start)
         (insert-buffer-substring buffer)
         (indent-rigidly start (point) 8)
         (read-only-mode 1)))
     (kill-buffer buffer)))

(cl-defun mason--process-sync (cmd &optional &key in out)
  "Run CMD with ARGS synchronously.
See `call-process' INFILE and DESTINATION for IN and OUT."
  (let ((prog (car cmd))
        (msg (mapconcat #'mason--quote cmd " "))
        buffer status success)
    (mason--info "Calling `%s'" msg)
    (when mason-dry-run (cl-return-from mason--process-sync nil))
    (unless (executable-find prog)
      (error "Missing program `%s'" prog))
    (setq buffer (generate-new-buffer "*mason process*"))
    (with-current-buffer buffer
      (setq status (apply #'call-process prog in (or out t) nil (cdr cmd)))
      (setq success (zerop status))
      (mason--process-output!)
      (unless success (error "Failed `%s'" msg))
      (cons status success))))

(defun mason--download (url newname &optional ok-if-already-exists)
  "Copy URL to NEWNAME.
OK-IF-ALREADY-EXISTS is the same in `url-copy-file'."
  (mason--info "Downloading %s to %s" url newname)
  (or mason-dry-run (url-copy-file url newname ok-if-already-exists)))


;; File Utilities

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
      (error "Path `%s' is not inside `%s'" res parent))
    res))

(defun mason--dir-empty-p (dir)
  "Return t if DIR exists and contains no non-dot files."
  (and (file-directory-p dir)
       (null (directory-files dir nil directory-files-no-dot-files-regexp))))

(defun mason--delete-directory (path &optional recursive ignore-dry-run)
  "Delete directory at PATH, optionally RECURSIVE.
If IGNORE-DRY-RUN, delete anyway even if `mason-dry-run' is non nil."
  (when (or (not mason-dry-run) ignore-dry-run)
    (delete-directory path recursive nil))
  (mason--info "Deleted `%s'" (directory-file-name path)))

(defun mason--delete-file (path &optional ignore-dry-run)
  "Delete file at PATH.
If IGNORE-DRY-RUN, delete anyway even if `mason-dry-run' is non nil."
  (when (or (not mason-dry-run) ignore-dry-run)
    (delete-file path))
  (mason--info "Deleted `%s'" path))


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
            (mason--info "Extracting `%s' to `%s' using `%s'" file tmp-dir (symbol-name fn))
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
            (error "Download failed: %s" url))
          (if (mason--archive-name filename)
              (mason--try-extract tmp-file dest)
            (unless mason-dry-run
              (when (or (directory-name-p dest) (file-directory-p dest))
                (progn (make-directory dest t)
                       (setq dest (mason--expand-child-file-name filename dest))))
              (make-directory (file-name-parent-directory dest) t)
              (copy-file tmp-file dest))
            (mason--info "Copied `%s' to `%s'" tmp-file dest)))
      (when (file-directory-p tmp-dir)
        (ignore-errors (mason--delete-directory tmp-dir t t))))))

(provide 'mason-basic)

;;; mason-basic.el ends here
