;; -*- lexical-binding: t; eval: (flymake-mode-off); -*-

(let* ((s-url "https://raw.githubusercontent.com/magnars/s.el/dda84d38fffdaf0c9b12837b504b402af910d01d/s.el")
       (s (make-temp-file "mason-ci-s-" nil ".el"))
       (dir (file-name-directory load-file-name))
       (root (expand-file-name "../.." dir)))
  (push root load-path)
  (url-copy-file s-url s t)
  (load s)
  (require 'mason)
  (let ((mason--log-full-message t)
        (mason--log-save-on-dry-run t)
        done failed)
    (mason-ensure
     (lambda ()
       (mason-dry-run-install-all2
        (lambda (success total failed-pkgs)
          (delete-file s)
          (setq done t
                failed failed-pkgs)))))
    (while (not done)
      (accept-process-output nil 0 1))
    (unless (null failed)
      (let ((only-unsupported t))
        (dolist (pkg failed)
          (let* ((log (gethash pkg mason--log))
                 (log0 (nth 0 log))
                 (log1 (nth 1 log)))
            (unless log
              (error "No logs for `%s'" pkg))
            (unless (string-match-p "^\\[[0-9 :-]+\\] \\[DRY\\] ERROR: Installation of .* failed$" log0)
              (error "Unexpected log for `%s': %s" pkg log0))
            (cond
             ((string-match-p "^\\[[0-9 :-]+\\] \\[DRY\\] ERROR: Package .* only supports platforms .*$" log1)
              (message "`%s' doesn't have the current platform in `supported_platforms'" pkg))
             ((string-match-p "^\\[[0-9 :-]+\\] \\[DRY\\] ERROR: No matching .* for target .*$" log1)
              (message "`%s' doesn't have the current platform in `target'" pkg))
             (t (error "Unexpected log for `%s': %s" pkg log1)))))
        (unless only-unsupported
          (error "Failed"))))))
