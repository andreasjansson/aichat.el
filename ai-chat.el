(defvar ai-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") 'ai-prompt)
    (define-key map (kbd "M-.") 'ai-paragraph)
    (define-key map (kbd "M-<return>") 'ai-buffer)
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

(defun ai-paragraph ()
  "Fetch the previous paragraph and use it as a prompt for AI streaming after inserting two newlines."
  (interactive)
  (let (para-start para-end para-text)
    (backward-paragraph)
    (setq para-start (point))
    (forward-paragraph)
    (setq para-end (point))
    ;; Get the text of the previous paragraph
    (setq para-text (string-trim (buffer-substring-no-properties para-start para-end)))
    ;; Move back to the earlier position and insert two newlines
    (goto-char para-end)
    (insert "\n\n" ai-chat-assistant-tag "\n\n")
    ;; Now we call ai-stream using this paragraph as the prompt
    (message para-text)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" ai-chat-user-tag "\n\n"))))
      (ai-stream "You are a helpful assistant" para-text complete-callback))))

(defun ai-prompt (prompt)
  "Ask AI a question and insert the response at the current point."
  (interactive "sPrompt: ")
  (let ((system-prompt "You are a helpful assistant."))
    (ai-stream system-prompt prompt)))

(defun ai-chat-maybe-insert-assistant-tag ()
  (let ((found-tag nil))
    (save-excursion
      (while (and (not found-tag)
                  (re-search-backward (concat ai-chat-user-tag "\\|" ai-chat-assistant-tag) nil t))
        (setq found-tag (match-string 0))))
    (unless (string= found-tag ai-chat-assistant-tag)
      (insert "\n\n" ai-chat-assistant-tag "\n\n"))))

(defun ai-buffer ()
  "Send the buffer content to AI as a dialog, move to the end, insert a prefix, and insert the response at the end."
  (interactive)
  (let* ((buffer-content (buffer-string))
         (dialog (ai--parse-dialog buffer-content)))
    (goto-char (point-max))
    (ai-chat-maybe-insert-assistant-tag)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" ai-chat-user-tag "\n\n"))))
      (ai-stream-dialog dialog complete-callback))))

(defun ai--parse-dialog (buffer-content)
  "Parse the buffer content into a dialog format."
  (let ((dialog ())
        (current-role nil)
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
        (setq current-content (concat current-content "\n" line)))))
    (when current-role
      (push (cons current-role (string-trim current-content)) dialog))
    (nreverse dialog)))
