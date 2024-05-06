;; -*- lexical-binding: t -*-

(defconst ai-available-models
  '(claude/claude-3-haiku-20240307
    claude/claude-3-opus-20240229
    openai/gpt-4-turbo
    groq/llama3-8b-8192
    groq/llama3-70b-8192
    groq/mixtral-8x7b-32768)
  "List of available models.")

(defcustom ai-model 'claude/claude-3-haiku-20240307
  "The currently used model."
  :type `(choice ,@(mapcar (lambda (model) `(const ,model)) ai-available-models))
  :group 'ai)

(defun ai-set-model ()
  "Interactive function to select and set the AI model."
  (interactive)
  (let ((model (completing-read "Choose AI model: " ai-available-models nil t)))
    (customize-set-variable 'ai-model (intern model))
    (message "Model set to %s" model)))

(defun ai--build-curl-command (url method headers data)
  "Build the curl command string based on the provided parameters."
  (let ((header-strings (mapconcat (lambda (header)
                                     (format "-H \"%s: %s\"" (car header) (cdr header)))
                                   headers " "))
        (data-string (if data (format "-d '%s'" (replace-regexp-in-string "'" "'\\\\''" data)) "")))
    (format "curl -s -X %s %s %s %s"
          method
          header-strings
          data-string
          url)))

(defun ai--process-filter (output text-extractor callback accumulated-output)
  "Process filter function for handling the output of the curl process."
  (setq accumulated-output (concat accumulated-output output))
  (let ((separator-position (string-match-p "\n\n" accumulated-output)))
    (while separator-position
      (let* ((chunk (substring accumulated-output 0 separator-position))
             (text (funcall text-extractor chunk)))
        (funcall callback text)
        (setq accumulated-output (substring accumulated-output (+ separator-position (length "\n\n"))))
        (setq separator-position (string-match-p "\n\n" accumulated-output)))))
  accumulated-output)

(defun ai--process-sentinel (proc text-extractor callback complete-callback cancel-callback restore-callback accumulated-output)
  "Process sentinel function for handling the completion or cancellation of the curl process."
  (when (memq (process-status proc) '(exit signal))
    (unless (string-empty-p accumulated-output)
      ;; do the final generation, but if the user has hit C-g it might
      ;; not result in valid json
      (condition-case nil
          (funcall callback (funcall text-extractor accumulated-output))
        (error nil))
      )
    (funcall restore-callback)
    (if (= (process-exit-status proc) 0)
        (when complete-callback (funcall complete-callback))
      (when cancel-callback (funcall cancel-callback)))))

(defun ai--extra-text-fn (provider)
  (cond
   ((string= provider "claude")
    #'ai--claude-extract-text)
   ((string= provider "openai")
    #'ai--openai-extract-text)
   ((string= provider "groq")
    #'ai--openai-extract-text)))

(defun ai--claude-extract-text (event)
  (let ((event-parts (split-string event "\n" t)))
    (when (>= (length event-parts) 2)
      (let ((event-type (car event-parts))
            (event-data (substring (cadr event-parts) 6)))
        (if (string-prefix-p "event: content_block_delta" event-type)
            (ai--claude-content-block-delta-extract-text event-data)
          "")))))

(defun ai--claude-content-block-delta-extract-text (data)
  "Handle the content_block_delta event."
  (let* ((block-data (json-read-from-string data))
         (delta (assoc-default 'delta block-data))
         (text (assoc-default 'text delta)))
    text))

(defun ai--openai-extract-text (event)
  (if (string-prefix-p "data: " event)
      (let ((data-string (substring event 6)))
        (if (string-prefix-p "[DONE]" data-string)
            ""
          (let* ((data-string (substring event 6))
                 (data (json-read-from-string data-string))
                 (choices (assoc-default 'choices data))
                 (first-choice (elt choices 0))
                 (delta (assoc-default 'delta first-choice))
                 (content (assoc-default 'content delta)))
            (or content ""))))
    (message (format "Unparseable event: %s" event))
    ""))

(defun ai--model-provider (model)
  "Get the provider of the AI model."
  (car (split-string (symbol-name model) "/")))

(defun ai--model-name (model)
  "Get the name of the AI model."
  (cadr (split-string (symbol-name model) "/")))

(defun ai--get-api-url (provider)
  "Get the API URL based on the AI model provider."
  (cond
   ((string= provider "claude")
    "https://api.anthropic.com/v1/messages")
   ((string= provider "openai")
    "https://api.openai.com/v1/chat/completions")
   ((string= provider "groq")
    "https://api.groq.com/openai/v1/chat/completions")))

(defun ai--get-api-key (provider)
  "Get the API key based on the AI model provider."
  (let* ((env-key (cond
                  ((string= provider "claude")
                   "ANTHROPIC_API_KEY")
                  ((string= provider "openai")
                   "OPENAI_API_KEY")
                  ((string= provider "groq")
                   "GROQ_API_KEY")))
         (api-key (getenv env-key)))
    (unless api-key
      (error (format "Please set the %s environment variable" env-key)))
    api-key))

(defun ai--get-request-headers (provider api-key)
  "Get the request headers based on the AI model provider."
  (let ((headers `(("Content-Type" . "application/json"))))
    (cond
     ((string= provider "claude")
      (push `("x-api-key" . ,api-key) headers)
      (push `("anthropic-version" . "2023-06-01") headers)
      (push `("anthropic-beta" . "messages-2023-12-15") headers))
     ((string= provider "openai")
      (push `("Authorization" . ,(format "Bearer %s" api-key)) headers))
     ((string= provider "groq")
      (push `("Authorization" . ,(format "Bearer %s" api-key)) headers)))
    headers))

(defun ai--request-data-fn (provider)
  (cond
   ((string= provider "claude")
    #'ai--claude-request-data)
   ((string= provider "openai")
    #'ai--openai-request-data)
   ((string= provider "groq")
    #'ai--openai-request-data)))

(defun ai--openai-request-data (model-name dialog)
  "Create the request data for the API call using a multi-turn dialog."
  (let* ((messages (mapcar (lambda (message)
                             `((role . ,(symbol-name (car message)))
                               (content . ,(cdr message))))
                           dialog))
         (last-message (car (last dialog))))
    ;; Check if the last message is of type system
    (when (eq (car last-message) 'assistant)
      ;; Append a new user message "Continue..."
      (setq messages (append messages `(((role . "user") (content . "Seamlessly continue generating from the point it cut off."))))))
    (json-encode `(("messages" . ,messages)
                   ("model" . ,model-name)
                   ("max_tokens" . 2048)
                   ("stream" . t)))))

(defun ai--claude-request-data (model-name dialog)
  "Create the request data for the API call in the format expected by Anthropic.
The first system message in the dialog is used as a top-level system message."
  (let ((system-message nil)
        (user-messages ()))
    ;; Separate system messages and collect user messages
    (dolist (message dialog)
      (let ((role (car message))
            (content (cdr message)))
        (cond
         ((eq role 'system)
          (if system-message
              (message 'ai-request "Multiple system messages found in the dialog; only the first one is used as top level system message.")
            (setq system-message content)))
         (t
          (push `((role . ,(symbol-name role))
                  (content . ,content))
                user-messages)))))
    (json-encode `(("model" . ,model-name)
                   ("messages" . ,(nreverse user-messages))
                   ("system" . ,system-message)
                   ("max_tokens" . 2048)
                   ("stream" . t)))))

(defun ai-stream (system-prompt prompt &optional complete-callback cancel-callback)
  "Stream AI text output at the current cursor point."
  (let ((dialog `((system . ,system-prompt)
                  (user . ,prompt))))
    (ai-stream-dialog dialog complete-callback cancel-callback)))

(defun ai-stream-dialog (dialog &optional complete-callback cancel-callback)
  "Stream AI text output at the current cursor point."
  (let* ((provider (ai--model-provider ai-model))
         (model-name (ai--model-name ai-model))
         (api-url (ai--get-api-url provider))
         (api-key (ai--get-api-key provider))
         (request-headers (ai--get-request-headers provider api-key))
         (request-data-fn (ai--request-data-fn provider))
         (request-data (funcall request-data-fn model-name dialog))
         (output-buffer (current-buffer))
         (insert-position (point))
         (text-extractor (ai--extra-text-fn provider))
         (text-callback (lambda (text)
                          (with-current-buffer output-buffer
                            (goto-char insert-position)
                            (insert text)
                            (setq insert-position (point)))))
         (curl-command (ai--build-curl-command api-url "POST" request-headers request-data))
         (accumulated-output "")
         (original-quit-binding (with-current-buffer output-buffer
                                  (local-key-binding (kbd "C-g"))))
         (process (start-process-shell-command "http-client-curl" nil curl-command))
         (undo-handle (prepare-change-group output-buffer))
         (restore-callback (lambda ()
                             (with-current-buffer output-buffer
                               (local-set-key (kbd "C-g") original-quit-binding)
                               (undo-amalgamate-change-group undo-handle)
                               (accept-change-group undo-handle)))))

    (activate-change-group undo-handle)

    ;; TODO: remove debug
    ;; (prin1 request-data)

    (set-process-filter
     process (lambda (proc output)
               (setq accumulated-output
                     (ai--process-filter output text-extractor text-callback accumulated-output))))
    (set-process-sentinel
     process (lambda (proc event)
               (ai--process-sentinel
                proc text-extractor text-callback complete-callback cancel-callback restore-callback accumulated-output)))
    (set-process-query-on-exit-flag process nil)

    (with-current-buffer output-buffer
      (local-set-key (kbd "C-g")
                     (lambda ()
                       (interactive)
                       (when (process-live-p process)
                         (message "Interrupting generation")
                         (interrupt-process process)
                         (sit-for 0.1)
                         (delete-process process)
                         (when cancel-callback (funcall cancel-callback)))
                       (funcall restore-callback)
                       )))))

(defun ai-paragraph ()
  "Fetch the previous paragraph and use it as a prompt for AI streaming after inserting two newlines."
  (interactive)
  (let (para-start para-end para-text)
    (backward-paragraph)
    (setq para-start (point))
    (forward-paragraph)
    (setq para-end (point))
    (setq para-text (string-trim (buffer-substring-no-properties para-start para-end)))
    (goto-char para-end)
    (insert "\n\n")
    (ai-stream "You are a helpful assistant" para-text)))

(defun ai-prompt (prompt)
  "Ask AI a question and insert the response at the current point."
  (interactive "sPrompt: ")
  (let ((system-prompt "You are a helpful assistant."))
    (ai-stream system-prompt prompt)))

(defun ai--map-dialog-content (fn dialog)
  (mapcar (lambda (message)
            (cons (car message) (funcall fn (cdr message))))
          dialog))
