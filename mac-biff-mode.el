(defvar mac-biff-lighter ""
  "Lighter used by `mac-biff-mode'.")

(defvar mac-biff-mail-re "\\([[:digit:]]+\\)"
  "Regular expression to match number counts in a Gnus buffer.")

(define-minor-mode mac-biff-mode
  "Minor mode to display state of new email."
  nil mac-biff-lighter nil
  :global t
  (if mac-biff-mode
      (progn (add-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
             (add-hook 'gnus-exit-group-hook 'mac-biff-update)
             (mac-biff-update))
    (remove-hook 'gnus-after-getting-new-news-hook 'mac-biff-update)
    (remove-hook 'gnus-exit-group-hook 'mac-biff-update)))

(defun mac-biff-update ()
  "Read the mail count from Gnus."
  (let ((buffer (get-buffer "*Group*"))
        (count 0))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward mac-biff-mail-re nil t)
          (setq count (+ count (string-to-number (match-string 1)))))))
    (setq mac-biff-lighter (if (= count 0)
                               ""
                             (format " [%d Mails]" count)))))

(provide 'mac-biff-mode)