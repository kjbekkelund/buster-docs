(defun replace-doc-url ()
  (interactive)
  (search-forward "<%= doc_url \"")
  (delete-backward-char 13)
  (insert "/docs/")
  (search-forward "\"")
  (delete-char 4))

(defun replace-id-macro ()
  (interactive)
  (if (search-forward "<%= id" nil t)
      (progn (delete-char -6)
             (delete-char 2)
             (search-forward "%>")
             (delete-char -4)
             (point))))

;; <%= anchor "createOption" %>
(defun replace-anchor-macro ()
  (interactive)
  (if (not (goto-next-anchor))
      (message "No more anchors")
    (let ((target (anchor-target))
          (text (anchor-text)))
      (insert (concat "<a href=\"#" target "\">" text "</a>"))
      (while (not (looking-at "%>"))
        (delete-char 1))
      (delete-char 2))))

(defun goto-next-anchor ()
  (interactive)
  (if (search-forward "<%= anchor" nil t)
      (progn (backward-char 10) (point)) nil))

(defun jump-to-before-next (str)
  (progn (search-forward str)
         (backward-char)
         (point)))

(defun anchor-target ()
  (save-excursion
    (let* ((beg (point))
           (bound (search-forward "%>")))
      (goto-char beg)
      (if (search-forward "," bound t)
          (buffer-substring
           (search-forward "\"")
           (jump-to-before-next "\""))
        (search-forward "\"")
        (buffer-substring
         (point)
         (jump-to-before-next "\""))))))

(defun anchor-text ()
  (save-excursion
    (search-forward "\"")
    (buffer-substring
     (point)
     (jump-to-before-next "\""))))

(defun content-in-quotes ()
  (buffer-substring (point) (progn
                              (search-forward "\"")
                              (backward-char)
                              (point))))

(defun is-clean-p ()
  (interactive)
  (if (not (search-forward "<%=" nil t))
      (progn (message "Buffer clean!") t)
    (message "Sorry mac, more cruft to clean") nil))


(defun convert-doc-buffer ()
  (interactive)
  (beginning-of-buffer)
  (while (replace-id-macro))
  (beginning-of-buffer)
  (while (goto-next-anchor)
    (replace-anchor-macro))
  (beginning-of-buffer)
  (while (not (is-clean-p))
    (replace-doc-url)))

(defvar buster-docs-cleanup-mode-map nil
  "Keymap for Buster docs cleanup mode.")

(if buster-docs-cleanup-mode-map nil
  (setq buster-docs-cleanup-mode-map (make-keymap))
  (define-key buster-docs-cleanup-mode-map (kbd "C-c d") 'replace-doc-url)
  (define-key buster-docs-cleanup-mode-map (kbd "C-c a") 'replace-anchor-macro)
  (define-key buster-docs-cleanup-mode-map (kbd "C-c i") 'replace-id-macro)
  (define-key buster-docs-cleanup-mode-map (kbd "C-c c") 'convert-doc-buffer)
  (define-key buster-docs-cleanup-mode-map (kbd "C-c C-c") 'is-clean-p))

(define-minor-mode buster-docs-cleanup-mode
  "Buster minor mode, for cleaning up old rhtml stuff from docs

\\{buster-docs-cleanup-mode-map}"
  nil " Cleanup" buster-docs-cleanup-mode-map)
