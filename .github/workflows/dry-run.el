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
          (if (mason--source-supported-p pkg)
              (setq only-unsupported nil)
            (message "Package `%s' is unsupported for current platform" pkg)))
        (unless only-unsupported
          (error "Failed"))))))
