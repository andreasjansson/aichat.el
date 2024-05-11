;;; aichat.el --- Chat with language models -*- lexical-binding: t -*-

;; Copyright (C) 2023 Andreas Jansson

;; Author: Andreas Jansson <andreas@jansson.me.uk>
;; Version: 0.1.0
;; URL: https://github.com/andreasjansson/aichat.el
;; Package-Requires: ((emacs "28.0") (parsec "0.1.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides an interface for interacting with AI language models

;;; Code:

(require 'json)
(require 'dom)
(require 'eww)
(require 'parsec)
(require 'markdown-mode)

(defconst aichat-available-models
  '(replicate/meta/meta-llama-3-70b-instruct
    replicate/meta/meta-llama-3-70b
    replicate/meta/meta-llama-3-8b-instruct
    replicate/meta/meta-llama-3-8b
    replicate/snowflake/snowflake-arctic-instruct
    claude/claude-3-haiku-20240307
    claude/claude-3-opus-20240229
    openai/gpt-4-turbo
    groq/llama3-8b-8192
    groq/llama3-70b-8192
    groq/mixtral-8x7b-32768
    ollama/llama3:8b
    ollama/llama3:text
    ollama/mistral:7b
    ollama/phi3:3.8b
    ollama/wizardlm2:7b
    ollama/gemma:2b)
  "List of available models.")

(defcustom aichat-model 'claude/claude-3-haiku-20240307
  "The currently used model."
  :type `(choice ,@(mapcar (lambda (model) `(const ,model)) aichat-available-models))
  :group 'aichat)

(defcustom aichat-default-system-prompt "You are a helpful assistant."
  "Default system prompt used for AI interactions."
  :type 'string
  :group 'aichat)

(defcustom aichat-temperature 0.8
  "Sampling temperature between 0 and 1."
  :type 'float
  :group 'aichat)

(defvar aichat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<return>") #'aichat-buffer)
    (define-key map (kbd "C-; a") #'aichat-insert-assistant-tag)
    (define-key map (kbd "C-; u") #'aichat-insert-user-tag)
    (define-key map (kbd "C-; s") #'aichat-insert-system-tag)
    (define-key map (kbd "C-; f") #'aichat-context-file)
    (define-key map (kbd "C-; b") #'aichat-context-buffer)
    (define-key map (kbd "C-; h") #'aichat-context-http-url)
    (define-key map (kbd "C-; m") #'aichat-set-model)
    (define-key map (kbd "C-; c") #'aichat-copy-code)
    map)
  "Keymap for `aichat-mode'.")

;(add-to-list 'auto-mode-alist '("\\.ai\\'" . aichat-mode))

(define-derived-mode aichat-mode gfm-mode "AI"
  "Major mode for interacting with AI."
  (use-local-map aichat-mode-map)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local mode-line-misc-info '(:eval (symbol-name aichat-model))))

(defvar aichat-user-tag "## USER:")
(defvar aichat-assistant-tag "## ASSISTANT:")
(defvar aichat-system-tag "## SYSTEM:")

(defun aichat ()
  "Create a new buffer and switch to `aichat-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "*aichat*")))
    (switch-to-buffer buffer)
    (aichat-mode)
    (insert ;;"<!-- -*- mode: aichat -*- -->\n\n" ;; not working yet, need to update the parser to ignore this
            aichat-system-tag
            "\n\n" aichat-default-system-prompt "\n\n"
            aichat-user-tag
            "\n\n")
    (message (format "Using model %s" aichat-model))))

(defun aichat-insert-assistant-tag ()
  "Insert the assistant tag into the buffer."
  (interactive)
  (insert aichat-assistant-tag)
  (insert "\n\n"))

(defun aichat-insert-user-tag ()
  "Insert the user tag into the buffer."
  (interactive)
  (insert aichat-user-tag)
  (insert "\n\n"))

(defun aichat-insert-system-tag ()
  "Insert the system tag into the buffer."
  (interactive)
  (insert aichat-system-tag)
  (insert "\n\n"))

(defun aichat--maybe-insert-assistant-tag ()
  "Insert the assistant tag if it's not already present."
  (let ((found-tag nil))
    (save-excursion
      (while (and (not found-tag)
                  (re-search-backward (concat aichat-user-tag "\\|" aichat-assistant-tag "\\|" aichat-system-tag) nil t))
        (setq found-tag (match-string 0))))
    (unless (string= found-tag aichat-assistant-tag)
      (insert "\n\n" aichat-assistant-tag "\n\n"))))

(defun aichat-buffer ()
  "Send the buffer content to AI as a dialog, move to the end, insert a prefix, and insert the response at the end."
  (interactive)
  (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (dialog (aichat--parse-dialog buffer-content)))
    (unless dialog
      (error "Failed to parse dialog.  Did you forget to close a html tag?"))
    (goto-char (point-max))
    (aichat--maybe-insert-assistant-tag)
    (let ((complete-callback (lambda ()
                               (insert "\n\n" aichat-user-tag "\n\n"))))
      (aichat-stream-dialog dialog complete-callback))))

(defun aichat-context-file ()
  "Prompt the user to select a file and insert an <ai-context> at point."
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select file: " nil nil t))))
    (if (file-exists-p file)
        (insert (format "<ai-context>%s</ai-context>\n\n" file))
      (message "File does not exist!"))))

(defun aichat-context-buffer ()
  "Prompt the user to select a buffer and insert an <ai-context> at point."
  (interactive)
  (let ((buffer-name (read-buffer "Select buffer: " nil t)))
    (when buffer-name
      (let ((buffer (get-buffer buffer-name)))
        (when buffer
          (let ((path (buffer-file-name buffer)))
            (insert (format "<ai-context>%s</ai-context>\n\n" path))))))))

(defun aichat-context-http-url (url)
  "Insert an <ai-context> to the provided URL at point."
  (interactive "sUrl: ")
  (insert (format "<ai-context>%s</ai-context>\n\n" url)))

(defun aichat-copy-code ()
  "Copy the current code block under point."
  (interactive)
  (let ((code-block (aichat--get-current-code-block)))
    (if code-block
        (progn
          (kill-new code-block)
          (message (format "Copied code: %s" (aichat--truncate-with-ellipsis code-block 40))))
      (error "Point is not inside a code block"))))

(defun aichat--get-current-code-block ()
  "Return the current code block under point, or nil if not found."
  (save-excursion
    (when (re-search-backward "^```" nil t)
      (forward-line)
      (let ((start (point)))
        (when (re-search-forward "^```" nil t)
          (backward-char 4)
          (buffer-substring-no-properties start (point)))))))

(defun aichat--truncate-with-ellipsis (str max-width)
  "Truncate STR to MAX-WIDTH characters, adding an ellipsis if necessary."
  (let ((len (length str)))
    (if (<= len max-width)
        str
      (concat (substring str 0 (- max-width 3)) "..."))))

(defun aichat-set-model ()
  "Set the current AI model."
  (interactive)
  (let ((model (completing-read "Choose AI model: " aichat-available-models nil t)))
    (customize-set-variable 'aichat-model (intern model))
    (message "Model set to %s" model)))

(defun aichat--build-curl-command (url method headers data)
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

(defun aichat--process-filter (output text-extractor callback accumulated-output)
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

(defun aichat--process-sentinel (proc text-extractor callback complete-callback cancel-callback restore-callback accumulated-output)
  "Process sentinel function for handling the completion or cancellation of the curl process."
  (when (memq (process-status proc) '(exit signal))
    (unless (string-empty-p accumulated-output)
      ;; do the final generation, but if the user has hit C-g it might
      ;; not result in valid json
      (condition-case nil
          (funcall callback (funcall text-extractor accumulated-output))
        (error nil)))
    (funcall restore-callback)
    (if (= (process-exit-status proc) 0)
        (when complete-callback (funcall complete-callback))
      (when cancel-callback (funcall cancel-callback)))))

(defun aichat--extra-text-fn (provider)
  "Return the appropriate text extraction function for the given provider."
  (cond
   ((string= provider "replicate")
    #'aichat--openai-extract-text)
   ((string= provider "claude")
    #'aichat--claude-extract-text)
   ((string= provider "openai")
    #'aichat--openai-extract-text)
   ((string= provider "groq")
    #'aichat--openai-extract-text)
   ((string= provider "ollama")
    #'aichat--openai-extract-text)))

(defun aichat--claude-extract-text (event)
  "Extract the text from a Claude event."
  (let ((event-parts (split-string event "\n" t)))
    (when (>= (length event-parts) 2)
      (let ((event-type (car event-parts))
            (event-data (substring (cadr event-parts) 6)))
        (if (string-prefix-p "event: content_block_delta" event-type)
            (aichat--claude-content-block-delta-extract-text event-data)
          "")))))

(defun aichat--claude-content-block-delta-extract-text (data)
  "Handle the content_block_delta event."
  (let* ((block-data (json-read-from-string data))
         (delta (assoc-default 'delta block-data))
         (text (assoc-default 'text delta)))
    text))

(defun aichat--openai-extract-text (event)
  "Extract the text from an OpenAI event."
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

(defun aichat--model-provider (model)
  "Get the provider of the current AI model."
  (car (split-string (symbol-name model) "/")))

(defun aichat--model-name (model)
  "Get the name of the current AI model."
  (let ((parts (split-string (symbol-name model) "/")))
    (mapconcat #'identity (cdr parts) "/")))

(defun aichat--get-api-url (provider)
  "Get the API URL based on the AI model provider."
  (cond
   ((string= provider "replicate")
    "https://openai-proxy.replicate.com/v1/chat/completions")
   ((string= provider "claude")
    "https://api.anthropic.com/v1/messages")
   ((string= provider "openai")
    "https://api.openai.com/v1/chat/completions")
   ((string= provider "groq")
    "https://api.groq.com/openai/v1/chat/completions")
   ((string= provider "ollama")
    "http://localhost:11434/v1/chat/completions")))

(defun aichat--get-api-key (provider)
  "Get the API key based on the AI model provider."
  (cond
   ((string= provider "replicate")
    (aichat--must-env "REPLICATE_API_KEY"))
   ((string= provider "claude")
    (aichat--must-env "ANTHROPIC_API_KEY"))
   ((string= provider "openai")
    (aichat--must-env "OPENAI_API_KEY"))
   ((string= provider "groq")
    (aichat--must-env "GROQ_API_KEY"))
   ((string= provider "ollama")
    nil)))

(defun aichat--must-env (key)
  "Ensure the environment variable KEY is set, or raise an error."
  (let ((value (getenv key)))
    (unless value
      (error (format "Please set the %s environment variable" key)))
    value))

(defun aichat--get-request-headers (provider api-key)
  "Get the request headers based on the AI model provider."
  (let ((headers `(("Content-Type" . "application/json"))))
    (cond
     ((string= provider "replicate")
      (push `("Authorization" . ,(format "Bearer %s" api-key)) headers))
     ((string= provider "claude")
      (push `("x-api-key" . ,api-key) headers)
      (push `("anthropic-version" . "2023-06-01") headers)
      (push `("anthropic-beta" . "messages-2023-12-15") headers))
     ((string= provider "openai")
      (push `("Authorization" . ,(format "Bearer %s" api-key)) headers))
     ((string= provider "groq")
      (push `("Authorization" . ,(format "Bearer %s" api-key)) headers))
     ((string= provider "ollama")
      nil))
    headers))

(defun aichat--request-data-fn (provider)
  "Return the appropriate request data function for the given provider."
  (cond
   ((string= provider "replicate")
    #'aichat--openai-request-data)
   ((string= provider "claude")
    #'aichat--claude-request-data)
   ((string= provider "openai")
    #'aichat--openai-request-data)
   ((string= provider "groq")
    #'aichat--openai-request-data)
   ((string= provider "ollama")
    #'aichat--openai-request-data)))

(defun aichat--openai-request-data (model-name dialog)
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

(defun aichat--claude-request-data (model-name dialog)
  "Create the request data for the Claude API call using a multi-turn dialog."
  (let ((system-message nil)
        (user-messages ()))
    ;; Separate system messages and collect user messages
    (dolist (message dialog)
      (let ((role (car message))
            (content (cdr message)))
        (cond
         ((eq role 'system)
          (if system-message
              (message "Multiple system messages found in the dialog; only the first one is used as top level system message.")
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

(defun aichat-stream (system-prompt prompt &optional complete-callback cancel-callback)
  "Stream the response from the AI model for the given system prompt and user prompt."
  (let ((dialog `((system . ,system-prompt)
                  (user . ,prompt))))
    (aichat-stream-dialog dialog complete-callback cancel-callback)))

(defun aichat-stream-dialog (dialog &optional complete-callback cancel-callback)
  "Stream the response from the AI model for the given multi-turn dialog."
  (let* ((provider (aichat--model-provider aichat-model))
         (model-name (aichat--model-name aichat-model))
         (api-url (aichat--get-api-url provider))
         (api-key (aichat--get-api-key provider))
         (request-headers (aichat--get-request-headers provider api-key))
         (request-data-fn (aichat--request-data-fn provider))
         (request-data (funcall request-data-fn model-name dialog))
         (output-buffer (current-buffer))
         (insert-position (point))
         (text-extractor (aichat--extra-text-fn provider))
         (text-callback (lambda (text)
                          (with-current-buffer output-buffer
                            (goto-char insert-position)
                            (insert text)
                            (setq insert-position (point)))))
         (curl-command (aichat--build-curl-command api-url "POST" request-headers request-data))
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
    ;;(prin1 request-data)

    (set-process-filter
     process (lambda (proc output)
               (setq accumulated-output
                     (aichat--process-filter output text-extractor text-callback accumulated-output))))
    (set-process-sentinel
     process (lambda (proc event)
               (aichat--process-sentinel
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
                       (funcall restore-callback))))))

(defun aichat-paragraph ()
  "Generate a response for the current paragraph."
  (interactive)
  (let (para-start para-end para-text)
    (backward-paragraph)
    (setq para-start (point))
    (forward-paragraph)
    (setq para-end (point))
    (setq para-text (string-trim (buffer-substring-no-properties para-start para-end)))
    (goto-char para-end)
    (insert "\n\n")
    (aichat-stream aichat-default-system-prompt para-text)))

(defun aichat-prompt (prompt)
  "Ask AI a question and insert the response at the current point."
  (interactive "sPrompt: ")
  (let ((system-prompt aichat-default-system-prompt))
    (aichat-stream system-prompt prompt)))

(defun aichat--map-dialog-content (fn dialog)
  "Apply the function FN to the content of each message in the DIALOG."
  (mapcar (lambda (message)
            (cons (car message) (funcall fn (cdr message))))
          dialog))

(defun aichat--parse-dialog (s)
  "Parse the input string S into a dialog structure."
  (parsec-with-input s
    (aichat--parse-spaces)
    (let ((untagged-section (parsec-optional (aichat--parse-untagged-section)))
          (sections (parsec-many (parsec-or
                                  (aichat--parse-user-section)
                                  (aichat--parse-assistant-section)
                                  (aichat--parse-system-section)))))
      (if untagged-section
          (cons untagged-section sections)
        sections))))

(defsubst aichat--parse-spaces ()
  "Parse any number of whitespace characters."
  (parsec-many-as-string
   (parsec-re "[[:space:]\r\n]")))

(defun aichat--parse-comment ()
  "Parse an HTML comment."
  (parsec-and
   (parsec-str "<!--")
   (parsec-until-as-string
    (parsec-try
     (parsec-str "-->")))
   (aichat--parse-spaces)))

(defun aichat--parse-codeblock ()
  "Parse a Markdown code block."
  (parsec-collect-as-string
   (parsec-str "```")
   (let ((out (parsec-until-as-string
               (parsec-str "```")
               :both)))
     (concat (car out) (cdr out)))))

(defun aichat--parse-followed-by-tag (tag)
  "Check if the input is followed by the given TAG."
  (parsec-lookahead (parsec-str tag)))

(defun aichat--parse-followed-by-section ()
  "Check if the input is followed by a section tag."
  (parsec-or
   (aichat--parse-followed-by-tag aichat-user-tag)
   (aichat--parse-followed-by-tag aichat-assistant-tag)
   (aichat--parse-followed-by-tag aichat-system-tag)
   (parsec-eof)))

(defun aichat--parse-context ()
  "Parse the <ai-context> from the input."
  (parsec-str "<ai-context>")
  (aichat--parse-spaces)
  (let ((context-path (parsec-until-s
                  (parsec-and
                   (aichat--parse-spaces)
                   (parsec-str "</ai-context>")))))
    (aichat--parse-expand-context context-path)))

(defun aichat--parse-expand-context (path)
  "Expand the given context PATH into its content."
  (cond
   ((string-prefix-p "http" path)
    (aichat--text-from-url path))
   ((file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))
   (t
    (message "Invalid path or URL: %s" path)
    nil)))

(defun aichat--parse-user-content ()
  "Parse the content of a user section."
  (parsec-or
   (parsec-many-till-as-string (parsec-or
                                (parsec-optional* (aichat--parse-comment))
                                (aichat--parse-codeblock)
                                (aichat--parse-context)
                                (parsec-any-ch))
                               (aichat--parse-followed-by-section))))

(defun aichat--parse-assistant-content ()
  "Parse the content of an assistant section."
  (parsec-or
   (parsec-many-till-as-string (parsec-or
                                (aichat--parse-codeblock)
                                (parsec-any-ch))
                               (aichat--parse-followed-by-section))))

(defun aichat--parse-user-section ()
  "Parse a user section."
  (parsec-and
   (parsec-str aichat-user-tag)
   (aichat--parse-spaces)
   (cons 'user (string-trim (aichat--parse-user-content)))))

(defun aichat--parse-untagged-section ()
  "Parse an untagged section."
  (let ((content (string-trim (aichat--parse-user-content))))
    (unless (string= "" content)
      (cons 'user content))))

(defun aichat--parse-assistant-section ()
  "Parse an assistant section."
  (parsec-and
   (parsec-str aichat-assistant-tag)
   (aichat--parse-spaces)
   (cons 'assistant (string-trim (aichat--parse-assistant-content)))))

(defun aichat--parse-system-section ()
  "Parse a system section."
  (parsec-and
   (parsec-str aichat-system-tag)
   (aichat--parse-spaces)
   (cons 'system (string-trim (aichat--parse-user-content)))))

(defun aichat--text-from-url (url &optional use-highest-readability)
  "Retrieve the text content from the given URL."
  (with-current-buffer
      (url-retrieve-synchronously url t nil 10.0)
    (let ((dom (libxml-parse-html-region)))
      (when use-highest-readability
        (setq dom (eww-highest-readability dom))
        (eww-score-readability dom))
      (aichat--dom-texts-inline-aware dom))))

(defun aichat--dom-texts-inline-aware (node &optional block-separator inline-separator)
  "Extract the text content from the given DOM node, with awareness of inline and block elements."
  (let ((block-separator (or block-separator "\n"))
        (inline-separator (or inline-separator " ")))
    (mapconcat
     (lambda (elem)
       (cond
        ((stringp elem)
         (when (> (length (string-trim elem)) 0)
           elem))
        ((memq (dom-tag elem) '(head meta script style details footer)) "")
        ((memq (dom-tag elem) '(p div h1 h2 h3 h4 h5 h6 pre br hr ul ol li))
         (concat (aichat--dom-texts-inline-aware elem block-separator inline-separator)
                 block-separator))
        (t
         (aichat--dom-texts-inline-aware elem block-separator inline-separator))))
     (dom-children node)
     inline-separator)))

(provide 'aichat)

;;; aichat.el ends here
