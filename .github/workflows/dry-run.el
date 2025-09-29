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
        failed)
    (mason-ensure
     (lambda ()
       (mason-dry-run-install-all
        (lambda (success total)
          (delete-file s)
          (setq failed (- total success))))))
    (while (null failed)
      (accept-process-output nil 0 1))
    (if (> failed 0) (error "Failed")
      (message "Success"))))
