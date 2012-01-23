(defun replace-doc-url ()
  (interactive)
  (search-forward "<%= doc_url \"")
  (delete-backward-char 13)
  (insert "/docs/")
  (search-forward "\"")
  (delete-char 4))

(defun is-clean-p ()
  (interactive)
  (if (not (search-forward "<%=" nil t))
      (message "Buffer clean!")
    (message "Sorry mac, more cruft to clean")))

(defvar buster-docs-cleanup-mode-map nil
  "Keymap for Buster docs cleanup mode.")

(if buster-docs-cleanup-mode-map nil
  (setq buster-docs-cleanup-mode-map (make-keymap))
  (define-key buster-docs-cleanup-mode-map (kbd "C-c d") 'replace-doc-url)
  (define-key buster-docs-cleanup-mode-map (kbd "C-c C-c") 'is-clean-p))

(define-minor-mode buster-docs-cleanup-mode
  "Buster minor mode, for cleaning up old rhtml stuff from docs

\\{buster-docs-cleanup-mode-map}"
  nil " Cleanup" buster-docs-cleanup-mode-map)
