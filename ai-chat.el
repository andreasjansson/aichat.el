(defvar ai-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") 'ai-chat-buffer)
    (define-key map (kbd "C-c a") 'ai-chat-insert-assistant-tag)
    (define-key map (kbd "C-c u") 'ai-chat-insert-user-tag)
    (define-key map (kbd "C-c s") 'ai-chat-insert-system-tag)
    (define-key map (kbd "C-c f") 'ai-chat-context-file)
    (define-key map (kbd "C-c b") 'ai-chat-context-buffer)
    map)
  "Keymap for `ai-chat-mode'.")

(add-to-list 'auto-mode-alist '("\\.ai\\'" . ai-chat-mode))

(defvar ai-chat-user-tag "## USER:")
(defvar ai-chat-assistant-tag "## ASSISTANT:")
(defvar ai-chat-system-tag "## SYSTEM:")

(define-derived-mode ai-chat-mode gfm-mode "AI"
  "Major mode for interacting with AI."
  (use-local-map ai-chat-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local mode-line-misc-info '(:eval (symbol-name ai-model))))

(defun ai-chat ()
  "Create a new buffer and switch to `ai-chat-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*ai-chat*")))
    (switch-to-buffer buffer)
    (ai-chat-mode)
    (insert ;;"<!-- -*- mode: ai-chat -*- -->\n\n" ;; not working yet, need to update the parser to ignore this
            ai-chat-system-tag
            "\n\n" ai-default-system-prompt "\n\n"
            ai-chat-user-tag
            "\n\n")
    (message (format "Using model %s" ai-model))))

(defun ai-chat-insert-assistant-tag ()
  (interactive)
  (insert ai-chat-assistant-tag)
  (insert "\n\n"))

(defun ai-chat-insert-user-tag ()
  (interactive)
  (insert ai-chat-user-tag)
  (insert "\n\n"))

(defun ai-chat-insert-system-tag ()
  (interactive)
  (insert ai-chat-system-tag)
  (insert "\n\n"))

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
         (raw-dialog (ai--chat-parse-dialog buffer-content))
         (dialog (ai--map-dialog-content 'ai--chat-load-context raw-dialog)))
    (goto-char (point-max))
    (ai--chat-maybe-insert-assistant-tag)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" ai-chat-user-tag "\n\n"))))
      (ai-stream-dialog dialog complete-callback))))

(defun ai--chat-parse-dialog (buffer-content)
  "Parse the buffer content into a dialog format."
  (let ((dialog ())
        (current-role nil)
        (current-content "")
        (is-in-comment nil))
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
        (when (not current-role)
          (setq current-role 'user))
        (setq current-content (concat current-content "\n" line)))))
    (when current-role
      (push (cons current-role (string-trim current-content)) dialog))
    (nreverse dialog)))

(defun ai-chat-context-file ()
  "Prompt the user to select a file and insert a formatted reference at point."
  (interactive)
  ;; Use `read-file-name` with parameters to ensure the file exists.
  (let ((file (expand-file-name (read-file-name "Select file: " nil nil t))))
    ;; Check if the file exists to handle the case where the user inputs an invalid file path.
    (if (file-exists-p file)
        ;; Insert format string at the current point.
        (insert (format "<ai-context>%s</ai-context>\n\n" file))
      (message "File does not exist!"))))

(defun ai-chat-context-buffer ()
  (interactive)
  (let ((buffer-name (read-buffer "Select buffer: " nil t)))
    (when buffer-name
      (let ((buffer (get-buffer buffer-name)))
        (when buffer
          (let ((path (buffer-file-name buffer)))
            (insert (format "<ai-context>%s</ai-context>\n\n" path))))))))

(defun ai--chat-load-context (content)
  "Replace <ai-context> tags with the content of the specified file or URL."
  (let ((start-tag "<ai-context>")
        (end-tag "</ai-context>"))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (while (re-search-forward (concat start-tag "\\(.*?\\)" end-tag) nil t)
        (let* ((path (match-string 1))
               (content (cond
                         ((file-exists-p path)
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-string)))
                         ((string-prefix-p "http" path) ;; this doesn't currently work!
                          (with-current-buffer (url-retrieve-synchronously path)
                            (goto-char (point-min))
                            (re-search-forward "^$")
                            (buffer-substring (point) (point-max))))
                         (t
                          (message "Invalid path or URL: %s" path)
                          nil))))
          (if content
              (replace-match content t t)
            (message "Failed to load content from: %s" path))))
      (buffer-string))))
