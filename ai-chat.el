(defvar ai-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") 'ai-chat-buffer)
    map)
  "Keymap for `ai-chat-mode'.")

(defvar ai-chat-user-tag "## USER:")
(defvar ai-chat-assistant-tag "## ASSISTANT:")
(defvar ai-chat-system-tag "## SYSTEM:")

(define-derived-mode ai-chat-mode gfm-mode "AI"
  "Major mode for interacting with AI."
  (use-local-map ai-chat-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t))

(defun ai-chat ()
  "Create a new buffer and switch to `ai-chat-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*ai-chat*")))
    (switch-to-buffer buffer)
    (ai-chat-mode)
    (insert ai-chat-system-tag
            "\n\nYou are a helpful assistant.\n\n"
            ai-chat-user-tag
            "\n\n")
    (message (format "Using model %s" ai-model))))

(defun ai--chat-maybe-insert-assistant-tag ()
  (let ((found-tag nil))
    (save-excursion
      (while (and (not found-tag)
                  (re-search-backward (concat ai-chat-user-tag "\\|" ai-chat-assistant-tag "\\|" ai-chat-system-tag) nil t))
        (setq found-tag (match-string 0))))
    (unless (string= found-tag ai-chat-assistant-tag)
      (insert "\n\n" ai-chat-assistant-tag "\n\n"))))

(defun ai-chat-buffer ()
  "Send the buffer content to AI as a dialog, move to the end, insert a prefix, and insert the response at the end."
  (interactive)
  (let* ((buffer-content (buffer-string))
         (dialog (ai--parse-dialog buffer-content)))
    (goto-char (point-max))
    (ai--chat-maybe-insert-assistant-tag)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" ai-chat-user-tag "\n\n"))))
      (ai-stream-dialog dialog complete-callback))))

(defun ai--parse-dialog (buffer-content)
  "Parse the buffer content into a dialog format assuming unidentified lines as user input."
  ;; Constants defining what prefixes to detect for each role
  (let ((ai-chat-system-tag "SYSTEM: ")
        (ai-chat-user-tag "USER: ")
        (ai-chat-assistant-tag "ASSISTANT: "))
    (let ((dialog ())
          (current-role 'user)  ; Default role is user
          (current-content ""))
      (dolist (line (split-string buffer-content "\n"))
        (cond
         ((string-prefix-p ai-chat-system-tag line)
          (when current-role
            (push (cons current-role (string-trim current-content)) dialog))
          (setq current-role 'system
                current-content (substring line (length ai-chat-system-tag))))
         ((string-prefix-p ai-chat-user-tag line)
          (when current-role
            (push (cons current-role (string-trim current-content)) dialog))
          (setq current-role 'user
                current-content (substring line (length ai-chat-user-tag))))
         ((string-prefix-p ai-chat-assistant-tag line)
          (when current-role
            (push (cons current-role (string-trim current-content)) dialog))
          (setq current-role 'assistant
                current-content (substring line (length ai-chat-assistant-tag))))
         (t
          ;; If no specific tag, keep current role and append the line.
          (setq current-content (concat current-content (if (not (string-empty-p current-content)) "\n") line)))))
      ;; Add the last spoken part to the dialog if there was any.
      (when (and current-role (not (string-empty-p current-content)))
        (push (cons current-role (string-trim current-content)) dialog))
      ;; Return the reversed dialog to maintain the original order.
      (nreverse dialog))))
