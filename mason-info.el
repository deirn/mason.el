;;; mason-info.el --- Package info viewer for mason.el -*- lexical-binding: t -*-

;; Copyright (C) 2025  Dimas Firmansyah

;; Author: Dimas Firmansyah <deirn@bai.lol>
;; Version: 1.0.0
;; Homepage: https://github.com/deirn/mason.el
;; Package-Requires: ((emacs "30.1") (mason "1.0.0"))
;; Keywords: tools lsp installer
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

;; Package information viewer for mason.el.
;; Based on elpaca-info.el.
;; https://github.com/progfolio/elpaca/blob/10e65441f34253254272aeacd300574b576894a2/elpaca-info.el

;;; Code:

(require 'mason)

(defgroup mason-info nil
  "Package information viewer for mason.el."
  :prefix "mason-info-"
  :group 'mason)

(defface mason-info-header '((t (:height 2.0 :inherit outline-1))) "Header." :group 'mason-info)
(defface mason-info-subheader '((t (:height 1.5 :inherit outline-2))) "Subheader." :group 'mason-info)
(defface mason-info-section '((t (:weight bold))) "Package section." :group 'mason-info)
(defface mason-info-deprecated '((t (:inherit error))) "Deprecated." :group 'mason-info)

(defface mason-info-key '((t (:inherit font-lock-keyword-face))) "Property key." :group 'mason-info)
(defface mason-info-array '((t (:inherit font-lock-type-face))) "Property key." :group 'mason-info)

(mason--keymap! mason-info-mode-map
  [remap quit-window]   kill-buffer-and-window
  [remap revert-buffer] mason-info-update
  "r"                   mason-info-update)

(define-derived-mode mason-info-mode special-mode "Mason Info"
  :interactive nil)

(defun mason--info-section (str)
  "Propertize STR with `mason-info-section'."
  (propertize str 'face 'mason-info-section))

;;;###autoload
(defun mason-info (package &optional interactive)
  "Visit Mason info file for PACKAGE.
If INTERACTIVE, ask for PACKAGE."
  (interactive '(nil nil))
  (mason--assert-ensured)
  (if (and package (not interactive))
      (mason-info--0 (gethash package mason--registry))
    (mason--ask-package "Mason Info" #'identity #'mason-info--0)))

(defvar-local mason-info--pkg nil)
(defun mason-info-update ()
  "Update current `mason-info-mode' buffer."
  (interactive nil mason-info-mode)
  (when mason-info--pkg
    (let* ((line (line-beginning-position))
           (char (current-column))
           max-char)
      (mason-info--0 (gethash mason-info--pkg mason--registry))
      (goto-char line)
      (setq max-char (save-excursion
                       (end-of-line)
                       (current-column)))
      (forward-char (min char max-char)))))

(defun mason-info--0 (spec)
  "Implementation of `mason-info' SPEC."
  (let* ((name (gethash "name" spec))
         (description (string-trim (gethash "description" spec)))
         (registry (gethash "registry" spec))
         (homepage (gethash "homepage" spec))
         (licenses (gethash "licenses" spec))
         (languages (gethash "languages" spec))
         (categories (gethash "categories" spec))
         (deprecation (gethash "deprecation" spec))
         (deprecation-since (when deprecation (gethash "since" deprecation)))
         (deprecation-message (when deprecation (gethash "message" deprecation)))
         (installed (gethash name mason--installed))
         (log (gethash name mason--log))
         (buf (get-buffer-create (format "*mason info for %s*" name))))
    (with-current-buffer buf
      (mason-info-mode)
      (read-only-mode -1)
      (erase-buffer)
      (goto-char (point-min))
      (insert
       (propertize name 'face 'mason-info-header) ?\n
       description ?\n
       ?\n)
      (when deprecation
        (insert
         (propertize (format "Deprecated since %s" deprecation-since) 'face 'mason-info-deprecated) ?\n
         deprecation-message ?\n
         ?\n))
      (insert
       (mason--info-section "registry  : ") registry ?\n
       (mason--info-section "homepage  : ") (buttonize homepage #'browse-url homepage) ?\n
       (mason--info-section "licenses  : ") (mapconcat #'identity licenses ", ") ?\n
       (mason--info-section "languages : ") (mapconcat #'identity languages ", ") ?\n
       (mason--info-section "categories: ") (mapconcat #'identity categories ", ") ?\n
       ?\n)
      (when installed
        (insert (propertize "installed recipe" 'face 'mason-info-subheader) ?\n)
        (mason-info--spec installed)
        (insert "\n\n"))
      (insert (propertize "recipe" 'face 'mason-info-subheader) ?\n)
      (mason-info--spec spec)
      (when log
        (insert "\n\n" (propertize "logs" 'face 'mason-info-subheader))
        (dolist (l (reverse log))
          (insert ?\n l)))
      (insert ?\n)
      (read-only-mode 1)
      (goto-char (point-min))
      (pop-to-buffer buf)
      (setq-local mason-info--pkg name))))

(defun mason-info--spec (spec)
  "Insert SPEC."
  (let ((source (gethash "source" spec))
        (bin (gethash "bin" spec))
        (share (gethash "share" spec))
        (opt (gethash "opt" spec)))
    (insert (mason--info-section "source:"))
    (mason-info--table source)
    (when bin
      (insert "\n\n" (mason--info-section "bin:"))
      (mason-info--table bin))
    (when share
      (insert "\n\n" (mason--info-section "share:"))
      (mason-info--table share))
    (when opt
      (insert "\n\n" (mason--info-section "opt:"))
      (mason-info--table opt))))

(defun mason-info--str (str &optional depth)
  "Insert STR with DEPTH."
  (setq depth (or depth 0)
        str (string-trim str))
  (if (not (s-contains-p "\n" str)) (insert str)
    (let ((spc (make-string (* depth 2) ?\s)))
      (insert "\n" spc (replace-regexp-in-string "\n" (concat "\n" spc) str)))))

(defun mason-info--table (table &optional depth ignore-first-depth)
  "TABLE info with DEPTH and IGNORE-FIRST-DEPTH."
  (setq depth (or depth 0))
  (maphash (lambda (key val)
             (if ignore-first-depth
                 (setq ignore-first-depth nil)
               (insert ?\n (make-string (* 2 depth) ?\s)))
             (insert (propertize (concat key ":") 'face 'mason-info-key))
             (cond
              ((stringp val) (insert " ") (mason-info--str val (1+ depth)))
              ((vectorp val) (mason-info--vector val (1+ depth)))
              ((hash-table-p val) (mason-info--table val (1+ depth)))
              (t (error "Invalid val `%S'" val))))
           table))

(defun mason-info--vector (vec &optional depth ignore-first-depth)
  "VEC info with DEPTH and IGNORE-FIRST-DEPTH."
  (setq depth (or depth 0))
  (mapc (lambda (val)
          (if ignore-first-depth
              (setq ignore-first-depth nil)
            (insert ?\n (make-string (* 2 depth) ?\s)))
          (insert (propertize (concat "- ") 'face 'mason-info-array))
          (cond
           ((stringp val) (mason-info--str val (1+ depth)))
           ((vectorp val) (mason-info--vector val (1+ depth) t))
           ((hash-table-p val) (mason-info--table val (1+ depth) t))
           (t (error "Invalid val `%S'" val))))
        vec))

(provide 'mason-info)

;;; mason-info.el ends here
