(defcustom ai-function-prompt-template
  "Please provide only the Emacs Lisp code that implements the functionality described below in an interactive function called %s.

Make sure you follow the following constraints:
* Only output the code and nothing outside of the code! The code will be used in an automated system so it's important that you only output code, otherwise the automated system will throw a syntax error.
* Don't wrap the code in backticks, '```elisp' or anything else!
* Don't add any explanations or additional text outside of the code!
* Don't break the code up into multiple functions (the output should be a single defun)
* Use comments liberally to explain what is happening.

You have access to the following function if you think the function should use AI to implement any of it's functionality:

```
(defun ai-stream (system-prompt prompt &optional complete-callback cancel-callback)
  \"Stream AI text output at the current cursor point.

This function sends a request to an AI API to generate text based on the
provided system prompt and user prompt. The generated text is streamed and
inserted at the current cursor position in the current buffer.

Arguments:
- system-prompt: The system prompt that provides context for the conversation.
- prompt: The user prompt to which the AI should respond.
- complete-callback: (Optional) A callback function to be called when the
                     streaming is completed successfully. If not provided,
                     no action is taken on completion.
- cancel-callback: (Optional) A callback function to be called when the
                   streaming is cancelled by the user (e.g., by pressing C-g).
                   If not provided, no action is taken on cancellation.

The response from the API is streamed into the output buffer.

The output buffer is set to the current buffer when the function is called.
The insert position is updated after each insertion to ensure the text is
appended correctly.

If the streaming is completed successfully, the `complete-callback` function
is called (if provided). If the streaming is cancelled by the user (e.g., by
pressing C-g), the `cancel-callback` function is called (if provided).
\"
```

The function should do this:

%s"
  "The prompt template used when generating functions with the Claude Opus API."
  :type 'string
  :group 'ai-function)

(defcustom ai-function-edit-prompt-template
  "Here is the existing Emacs Lisp function definition for %s:\n\n%s\n\nPlease modify the function based on the following prompt, without any explanations or additional text (only output the modified code, without backticks). Use comments to explain what is happening. The modified function should do this:\n\n%s"
  "The prompt template used when editing functions with the Claude Opus API."
  :type 'string
  :group 'ai-function)

(defcustom ai-function-system-prompt
  "You are an Emacs Lisp expert who only outputs code."
  "The system prompt used when generating functions with the Claude Opus API."
  :type 'string
  :group 'ai-function)

(defvar ai-function--last-edited-function nil
  "Name of the last function edited or created with `ai-function-edit` or `ai-function`.")

(defun ai-function--delete-function (name)
  "Replace an existing function with the given NAME."
  (with-current-buffer ai-function--output-buffer
    (goto-char (point-min))
    (when (re-search-forward (format "^(defun %s" name) nil t)
      (let ((start (match-beginning 0)))
        (goto-char start)
        (forward-sexp)
        (delete-region start (point))))))

(defun ai-function--function-exists (name)
  (goto-char (point-min))
  (re-search-forward (format "^(defun %s" name) nil t))

(defun ai-function--get-function-definition (name)
  "Get the definition of the function with the given NAME."
  (with-current-buffer ai-function--output-buffer
    (goto-char (point-min))
    (when (re-search-forward (format "^(defun %s" name) nil t)
      (let ((start (match-beginning 0)))
        (goto-char start)
        (forward-sexp)
        (buffer-substring-no-properties start (point))))))

(defun ai-function--get-function-names ()
  "Get a list of function names defined in the `ai-function--output-buffer'."
  (with-current-buffer ai-function--output-buffer
    (let ((function-names '()))
      (goto-char (point-min))
      (while (re-search-forward "^(defun \\([^ ]+\\)" nil t)
        (push (match-string-no-properties 1) function-names))
      (nreverse function-names))))

(defun ai-function--evaluate-function ()
  "Evaluate the function at the current point in the `ai-function--output-buffer'."
  (with-current-buffer ai-function--output-buffer
    (save-excursion
      (forward-sexp)
      (eval-last-sexp nil))))

(defun ai-function (name prompt)
  "Generate and append a new Emacs Lisp function using AI"
  (interactive "sEnter function name: \nsEnter function prompt: ")
  (let ((output-buffer (find-file-noselect "~/.emacs.d/my-ai-functions.el")))
    (setq ai-function--output-buffer output-buffer)
    (with-current-buffer output-buffer
      (if (ai-function--function-exists name)
          (let ((should-replace-function (y-or-n-p (format "Function '%s' already exists. Replace it? " name))))
            (unless should-replace-function
              (user-error "Aborted"))
            (ai-function--delete-function name))
        (progn
          (goto-char (point-max))
          (insert "\n\n")
          (goto-char (point-max)))))
    (display-buffer output-buffer '(display-buffer-reuse-window
                                    (inhibit-same-window . t)))
    (with-current-buffer output-buffer
      (ai-stream ai-function-system-prompt (format ai-function-prompt-template function-name prompt)
                 (lambda ()
                   (ai-function--evaluate-function)
                   (setq ai-function--last-edited-function function-name)
                   (message "Function '%s' generated and evaluated" function-name))
                 (lambda ()
                   (message "Generation of function '%s' cancelled" function-name))))))

(defun ai-function-edit (name prompt)
  "Edit an existing Emacs Lisp function using AI"
  (interactive
   (let ((function-names (ai-function--get-function-names)))
     (list (completing-read "Enter function name to edit: " function-names nil t)
           (read-string "Enter edit prompt: "))))
  (let ((output-buffer (find-file-noselect "~/.emacs.d/my-ai-functions.el")))
    (setq ai-function--output-buffer output-buffer)
    (with-current-buffer output-buffer
      (if (ai-function--function-exists name)
          (let ((function-definition (ai-function--get-function-definition name)))
            (ai-function--delete-function name)
            (goto-char (point-max))
            (display-buffer output-buffer '(display-buffer-reuse-window
                                            (inhibit-same-window . t)))
            (with-current-buffer output-buffer
              (ai-stream ai-function-system-prompt (format ai-function-edit-prompt-template function-name function-definition prompt)
                         (lambda ()
                           (ai-function--evaluate-function)
                           (setq ai-function--last-edited-function function-name)
                           (message "Function '%s' edited and evaluated" function-name))
                         (lambda ()
                           (message "Editing of function '%s' cancelled" function-name)))))
        (user-error "Function '%s' does not exist" name)))
    (message "Editing function '%s'..." name)))

(defun ai-function-edit-last (prompt)
  "Edit the last created or edited function using `ai-function-edit`."
  (interactive "sEnter edit prompt: ")
  (if ai-function--last-edited-function
      (ai-function-edit ai-function--last-edited-function prompt)
    (user-error "No function has been created or edited yet")))
