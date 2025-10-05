;;; mason-manager.el --- Manager for mason.el -*- lexical-binding: t -*-

;; Copyright (C) 2025  Dimas Firmansyah

;; Author: Dimas Firmansyah <deirn@bai.lol>
;; Version: 1.0.0
;; Homepage: https://github.com/deirn/mason.el
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

;; Package manager view for mason.el

;;; Code:

(require 'mason)
(require 'mason-info)

(defgroup mason-manager nil
  "Package manager view form mason.el."
  :prefix "mason-manager-"
  :group 'mason)

(defface mason-manager-package '((t (:weight bold))) "Package name." :group 'mason-manager)
(defface mason-manager-installed '((t (:weight bold :inherit success))) "Installed package name." :group 'mason-manager)
(defface mason-manager-pending '((t (:weight bold :inherit warning))) "Pending package name." :group 'mason-manager)
(defface mason-manager-deprecated '((t (:strike-through t))) "Pending package name." :group 'mason-manager)
(defface mason-manager-error '((t (:weight bold :inherit error))) "Error package name." :group 'mason-manager)

(defface mason-manager-mark-install '((t (:inherit success))) "Install Marker." :group 'mason-manager)
(defface mason-manager-mark-delete '((t (:inherit error))) "Delete Marker." :group 'mason-manager)

(defface mason-manager-language '((t (:inherit font-lock-keyword-face))) "Language filter." :group 'mason-manager)
(defface mason-manager-category '((t (:inherit font-lock-builtin-face))) "Category filter." :group 'mason-manager)


;; Internal Variables

(defconst mason-manager--buffer "*mason manager*")
(defvar-local mason-manager--rows nil)
(defvar-local mason-manager--marked nil)

(defvar mason-manager--category "All")
(defvar mason-manager--language "All")
(defvar mason-manager--installed 'show)
(defvar mason-manager--uninstalled 'show)
(defvar mason-manager--pending 'show)
(defvar mason-manager--deprecated 'hide)

(defvar-local mason-manager--header-line-advice nil)


;; Keymaps

(mason--keymap! mason-manager-map
  "?"   mason-manager-show-help
  "q"   quit-window
  "RET" mason-manager-visit
  "l"   mason-manager-visit
  "u"   mason-manager-unmark
  "U"   mason-manager-unmark-all
  "i"   mason-manager-mark-install
  "d"   mason-manager-mark-delete
  "x"   mason-manager-execute
  "f c" mason-manager-filter-category
  "f l" mason-manager-filter-language
  "t i" mason-manager-toggle-installed
  "t u" mason-manager-toggle-uninstalled
  "t p" mason-manager-toggle-pending
  "t d" mason-manager-toggle-deprecated)

(defun mason-manager-show-help ()
  "Toggle help window."
  (interactive nil mason-manager-mode)
  (mason--help-map 'mason-manager-map))

(defun mason-manager-visit ()
  "Visit info for package at point."
  (interactive nil mason-manager-mode)
  (mason-info (tabulated-list-get-id)))

(defun mason-manager-unmark ()
  "Unmark package."
  (interactive nil mason-manager-mode)
  (let ((pkg (tabulated-list-get-id)))
    (remhash pkg mason-manager--marked)
    (tabulated-list-put-tag "" t)))

(defun mason-manager-unmark-all ()
  "Unmark all packages."
  (interactive nil mason-manager-mode)
  (clrhash mason-manager--marked)
  (tabulated-list-clear-all-tags))

(defun mason-manager--mark (pkg action tag face)
  "Mark PKG at point with ACTION and TAG with FACE."
  (if (gethash pkg mason--pending)
      (message "Package %s is still being processed" pkg)
    (puthash pkg action mason-manager--marked)
    (tabulated-list-put-tag (propertize tag 'face face) t)))

(defun mason-manager-mark-install ()
  "Mark package to be installed."
  (interactive nil mason-manager-mode)
  (let ((pkg (tabulated-list-get-id)))
    (if (gethash pkg mason--installed)
        (message "Package %s already installed" pkg)
      (mason-manager--mark pkg 'install "I" 'mason-manager-mark-install))))

(defun mason-manager-mark-delete ()
  "Mark package to be removed."
  (interactive nil mason-manager-mode)
  (let ((pkg (tabulated-list-get-id)))
    (if (not (gethash pkg mason--installed))
        (message "Package %s is not installed" pkg)
      (mason-manager--mark pkg 'delete "D" 'mason-manager-mark-delete))))

(defun mason-manager-execute ()
  "Execute install/delete packages."
  (interactive nil mason-manager-mode)
  (cond
   ((hash-table-empty-p mason-manager--marked)
    (message "No marked packages"))
   ((y-or-n-p "Install/remove marked packages? ")
    (maphash
     (lambda (pkg action)
       (unless (gethash pkg mason--pending)
         (cond
          ((eq action 'install)
           (unless (gethash pkg mason--installed)
             (mason-install pkg nil t nil)))
          ((eq action 'delete)
           (when (gethash pkg mason--installed)
             (mason-uninstall pkg t nil))))))
     mason-manager--marked)
    (mason-manager-unmark-all))))

(defun mason-manager-filter-category ()
  "Filter by category."
  (interactive nil mason-manager-mode)
  (mason-manager--0 :f-category (completing-read "Category: " (mason--get-category-list) nil t nil nil "All")))

(defun mason-manager-filter-language ()
  "Filter by language."
  (interactive nil mason-manager-mode)
  (mason-manager--0 :f-language (completing-read "Language: " (mason--get-language-list) nil t nil nil "All")))

(defun mason-manager-toggle-installed ()
  "Toggle show installed."
  (interactive nil mason-manager-mode)
  (mason-manager--0 :t-installed (if (eq mason-manager--installed 'show) 'hide 'show)))

(defun mason-manager-toggle-uninstalled ()
  "Toggle show uninstalled."
  (interactive nil mason-manager-mode)
  (mason-manager--0 :t-uninstalled (if (eq mason-manager--uninstalled 'show) 'hide 'show)))

(defun mason-manager-toggle-pending ()
  "Toggle show pending."
  (interactive nil mason-manager-mode)
  (mason-manager--0 :t-pending (if (eq mason-manager--pending 'show) 'hide 'show)))

(defun mason-manager-toggle-deprecated ()
  "Toggle show deprecated."
  (interactive nil mason-manager-mode)
  (mason-manager--0 :t-deprecated (if (eq mason-manager--deprecated 'show) 'hide 'show)))

(defun mason-manager--key-description ()
  "Return key description."
  (let* (desc mapper)
    (setq mapper
          (lambda (event fn &optional prefix)
            (let ((key (key-description (vector event))))
              (if (keymapp fn)
                  (map-keymap (lambda (e f) (funcall mapper e f key)) fn)
                (push (cons (concat (when prefix (concat prefix " ")) key)
                            (nth 0 (s-split-up-to "\\." (documentation fn) 1 t)))
                      desc)))))
    (map-keymap mapper mason-manager-map)
    desc))


;; The Manager

(define-derived-mode mason-manager-mode tabulated-list-mode "Mason Manager"
  :interactive nil
  (add-hook 'quit-window-hook (lambda () (mason--help-map 'mason-manager-map 'kill)) nil t)
  (add-hook 'kill-buffer-hook (lambda () (mason--help-map 'mason-manager-map 'kill)) nil t))

;;;###autoload
(defun mason-manager ()
  "Open mason package manager."
  (interactive)
  (mason--assert-ensured)
  (mason-manager--0))

(defun mason-manager--header-line-advice ()
  "Header line for `mason-manager-mode' OG."
  (when mason-manager--header-line-advice
    (let* ((og (format-mode-line header-line-format))
           (max-len (round (/ (string-pixel-width og) (frame-char-width))))
           (og (string-trim-right og))
           (og-len (round (/ (string-pixel-width og) (frame-char-width))))
           (add
            (concat
             (mason-manager--header-text (format "L:%s" mason-manager--language) 'mason-manager-language #'mason-manager-filter-language) " "
             (mason-manager--header-text (format "C:%s" mason-manager--category) 'mason-manager-category #'mason-manager-filter-category) "   "
             (mason-manager--header-text "INS" (if (eq mason-manager--installed   'show) 'mason-manager-installed  'shadow) #'mason-manager-toggle-installed) " "
             (mason-manager--header-text "UNS" (if (eq mason-manager--uninstalled 'show) 'mason-manager-package    'shadow) #'mason-manager-toggle-uninstalled) " "
             (mason-manager--header-text "PND" (if (eq mason-manager--pending     'show) 'mason-manager-pending    'shadow) #'mason-manager-toggle-pending) " "
             (mason-manager--header-text "DEP" (if (eq mason-manager--deprecated  'show) 'mason-manager-deprecated 'shadow) #'mason-manager-toggle-deprecated)))
           (add-len (length add)))
      (setq header-line-format
            (concat og (make-string (- max-len og-len add-len 2) ?\s) add)))))

(defun mason-manager--header-text (str face cmd)
  "Propertize STR with FACE and CMD click."
  (propertize
   str
   'face face 'mouse-face 'highlight
   'help-echo (concat "mouse-1: " (documentation cmd))
   'local-map (let ((map (make-sparse-keymap)))
                (define-key map [header-line mouse-1] cmd)
                map)))

(cl-defun mason-manager--0 (&optional &key f-category f-language t-installed t-uninstalled t-pending t-deprecated)
  "Filter package and show manager ui.
Filter by F-CATEGORY F-LANGUAGE
T-INSTALLED T-UNINSTALLED T-PENDING T-DEPRECATED."
  (setq f-category    (or f-category    mason-manager--category)
        f-language    (or f-language    mason-manager--language)
        t-installed   (or t-installed   mason-manager--installed)
        t-uninstalled (or t-uninstalled mason-manager--uninstalled)
        t-pending     (or t-pending     mason-manager--pending)
        t-deprecated  (or t-deprecated  mason-manager--deprecated))
  (let ((buf (get-buffer mason-manager--buffer)))
    (when (or (null buf)
              (not (eq f-category    mason-manager--category))
              (not (eq f-language    mason-manager--language))
              (not (eq t-installed   mason-manager--installed))
              (not (eq t-uninstalled mason-manager--uninstalled))
              (not (eq t-pending     mason-manager--pending))
              (not (eq t-deprecated  mason-manager--deprecated)))
      (setq buf (get-buffer-create mason-manager--buffer))
      (with-current-buffer buf
        (mason-manager-mode)
        (read-only-mode -1)
        (erase-buffer)
        (setq mason-manager--rows (mason--make-hash)
              mason-manager--marked (mason--make-hash))
        (let (entries (name-width 20) (version-width 10))
          (maphash
           (lambda (pkg spec)
             (let* ((name (mason-manager--name pkg))
                    (deprecation (gethash "deprecation" spec))
                    (description (gethash "description" spec))
                    (description (replace-regexp-in-string "\n" " " description))
                    (description (if deprecation (concat (propertize "[Deprecated] " 'face 'error) description) description))
                    (languages (gethash "languages" spec []))
                    (categories (gethash "categories" spec []))
                    (installed (gethash pkg mason--installed))
                    (pending (gethash pkg mason--pending))
                    (source (gethash "source" spec))
                    (source-id (gethash "id" source))
                    (purl (mason--parse-purl source-id))
                    (version (gethash "version" purl))
                    (version (if (not (string-match (rx bol (literal name) (any "/@-") (group (+ any)) eol) version)) version
                               (match-string 1 version)))
                    (version (replace-regexp-in-string "^[vV]" "" version))
                    (version (replace-regexp-in-string (rx bol (or "untagged-" "0.0.0-")) "" version))
                    (version (if (not (string-match-p "^[0-9a-f]\\{20,40\\}$" version)) version
                               (concat (substring version 0 9) "…"))))
               (setq name-width (max name-width (length name))
                     version-width (max version-width (length version)))
               (when (and (or (eq 'show t-installed) (not installed))
                          (or (eq 'show t-uninstalled) installed)
                          (or (eq 'show t-pending) (not pending))
                          (or (eq 'show t-deprecated) (null deprecation))
                          (or (string= f-category "All") (seq-contains-p categories f-category))
                          (or (string= f-language "All") (seq-contains-p languages f-language)))
                 (let ((row (vector name version description "")))
                   (puthash name row mason-manager--rows)
                   (push (list pkg row) entries)))))
           mason--registry)
          (setq tabulated-list-padding 2
                tabulated-list-format (vector `("Name" ,name-width t)
                                              `("Version" ,version-width nil)
                                              '("Description" 100 nil)
                                              '("" 0 nil))
                tabulated-list-entries (nreverse entries)
                mason-manager--header-line-advice t
                mason-manager--category    f-category
                mason-manager--language    f-language
                mason-manager--installed   t-installed
                mason-manager--uninstalled t-uninstalled
                mason-manager--pending     t-pending
                mason-manager--deprecated  t-deprecated))
        (advice-add #'tabulated-list-init-header :after #'mason-manager--header-line-advice)
        (tabulated-list-init-header)
        (tabulated-list-print)
        (read-only-mode 1)
        (hl-line-mode 1)
        (mason--use-local-map mason-manager-map)))
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-same-window)))
    (mason--echo (substitute-command-keys (format "\\`%s' for help" (key-description (where-is-internal #'mason-manager-show-help mason-manager-map t)))))))

(defun mason-manager--name (pkg)
  "Propertize PKG depending on package status."
  (let* ((spec (gethash pkg mason--registry))
         (installed (gethash pkg mason--installed))
         (pending (gethash pkg mason--pending))
         (deprecated (gethash "deprecation" spec))
         (face (cond (installed 'mason-manager-installed)
                     (pending 'mason-manager-pending)
                     (t 'mason-manager-package)))
         (face (if deprecated `(,face mason-manager-deprecated) face)))
    (propertize pkg 'face face)))

(defun mason-manager--update (pkg &optional failed)
  "Update manager entry for PKG and FAILED."
  (let ((manager (get-buffer mason-manager--buffer))
        (current (current-buffer))
        row)
    (when manager
      (pop-to-buffer manager '((display-buffer-reuse-window display-buffer-same-window)))
      (with-current-buffer manager
        (setq row (gethash pkg mason-manager--rows))
        (when row
          (aset row 0 (if failed (propertize pkg 'face 'mason-manager-error) (mason-manager--name pkg)))
          (tabulated-list-print t t)))
      (pop-to-buffer current '((display-buffer-reuse-window display-buffer-same-window))))))

(provide 'mason-manager)

;;; mason-manager.el ends here
