; ai-edit-region and ai-edit-defun doesnt work

(defun ai-edit-region (start end prompt)
  "Edit the selected region using the AI and perform an ediff."
  (interactive "r\nsEnter edit prompt: ")
  (let ((region-content (buffer-substring-no-properties start end))
        (original-buffer (current-buffer))
        (output-buffer (generate-new-buffer "*AI Edit Output*"))
        (input-buffer (generate-new-buffer "*AI Edit Input*")))
    (with-current-buffer input-buffer
      (erase-buffer)
      (insert region-content))
    (with-current-buffer output-buffer
      (erase-buffer)
      (setq ai--insert-position (point))
      (ai-stream ai-function-system-prompt (format "%s\n\nHere is the Emacs Lisp code to edit:\n\n%s" prompt region-content)
                     (lambda ()
                       (with-current-buffer original-buffer
                         (ediff-buffers input-buffer output-buffer '(lambda ()
                                                                       (when (y-or-n-p "Keep the edited version? ")
                                                                         (delete-region start end)
                                                                         (goto-char start)
                                                                         (insert-buffer-substring output-buffer))
                                                                       (kill-buffer input-buffer)
                                                                       (kill-buffer output-buffer)))))
                     (lambda ()
                       (message "Editing cancelled")
                       (kill-buffer input-buffer)
                       (kill-buffer output-buffer))))))

(defun ai-edit-defun (prompt)
  "Edit the current defun using AI and perform an ediff."
  (interactive "sEnter edit prompt: ")
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (ai-edit-region start (point) prompt))))
