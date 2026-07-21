;;; elash-ui.el --- Markdown-based single-buffer UI for elash  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Single-buffer markdown transcript.  Each fragment heading carries a
;; globally-unique ID as its last whitespace-delimited token, enabling
;; simple backward search to locate any fragment.
;;
;;; Code:

(require 'dash)
(require 's)
(require 'acp)

(declare-function elash--send-prompt "elash.el")

(defvar elash--session)

(defcustom elash-ui--kinds
  '(("read" . "👀")
    ("edit" . "✍️")
    ("delete" . "🔥")
    ("move" . "🚛")
    ("search" . "🔍")
    ("execute" . "⚡")
    ("think" . "🤔")
    ("fetch" . "📡")
    ("switch_mode" . "🔀")
    ("other" . "🛠️"))
  "Alist mapping tool-call kinds to emoji characters."
  :type '(alist :key-type (string :tag "Kind")
                :value-type (string :tag "Emoji"))
  :group 'elash-ui)

(defcustom elash-ui--statuses
  '(("pending" . "⏳")
    ("in_progress" . "⌛")
    ("completed" . "✅")
    ("failed" . "❌"))
  "Alist mapping tool-call statuses to emoji characters."
  :type '(alist :key-type (string :tag "Status")
                :value-type (string :tag "Emoji"))
  :group 'elash-ui)

(defvar-keymap elash-mode-map
  "C-c C-c" #'elash-input-submit
  "C-c C-m" #'elash-set-model
  "C-c C-n" #'elash-set-mode
  "C-c C-g" #'elash-cancel)

(defun elash-ui--match-before-prompt (limit)
  "Match everything from the start of the buffer up to the last '# User'."
  (goto-char limit)
  (when (re-search-backward "^# User")
    (set-match-data (list (point-min) (match-end 0)))
    (goto-char limit)
    t))

(define-derived-mode elash-mode markdown-ts-mode "Elash"
  "Major mode for elash agentic shell.

The buffer is a live markdown transcript:
  # User                     - your prompt (editable for the current draft)
  # Response                 - agent response section
  ## Thinking                - agent thinking block
  ## Message                 - agent message block
  ## <kind> <status> <title> - tool call

Type under the last # User heading and press \\[elash-input-submit]."
  (setq-local header-line-format '(:eval (elash-ui--heading-line)))
  (font-lock-add-keywords nil '((elash-ui--match-before-prompt 0 '(face nil read-only t) append)))
  (font-lock-add-keywords nil '(("--- [A-Za-z0-9_]+$" 0 'shadow prepend)))
  (add-hook 'kill-buffer-hook #'elash--cleanup nil t))

(defun elash-input-submit ()
  "Send the current draft user prompt to the agent."
  (interactive)
  (when (elash-ui--response-active-p)
    (user-error "Agent is still responding"))
  (let ((text (elash-ui--extract-user-prompt)))
    (unless (s-blank-p text)
      (elash-ui--begin-response-section)
      (elash--send-prompt text))))

(defun elash-ui--extract-user-prompt ()
  "Return the text of the last # User section."
  (elash-ui--at "^# User"
    (forward-line)
    (s-trim (buffer-substring-no-properties (point) (point-max)))))

(defun elash-ui--begin-response-section ()
  "Insert # Response heading."
  (goto-char (point-max))
  (insert "\n# Response"))

(defun elash-ui--finalize-response ()
  "Insert a new editable # User section after the response finishes."
  (goto-char (point-max))
  (insert "\n# User\n"))

(defun elash-ui--handle-error (err)
  "Display ACP error ERR and finalize the response."
  (message "elash error: %s" err)
  (elash-ui--finalize-response))

(defun elash-ui--ask-permission (options title)
  "Prompt user for permission with OPTIONS for TITLE.
Return ANSWER where ANSWER is the selected option-id, or nil if cancelled"
  (let* ((kind-choices '(("allow_once"    . (?a "allow once"    "Allow the tool to run once"))
                         ("allow_always"  . (?A "Always allow"  "Always allow the tool to run in this session"))
                         ("reject_once"   . (?r "reject once"   "Reject the tool once"))
                         ("reject_always" . (?R "Always Reject" "Always reject the tool in this session"))))
         (kinds (--map (map-elt it :kind) options))
         (choices (--map (cdr it) (--filter (member (car it) kinds) kind-choices)))
         (answer (condition-case nil
                     (read-multiple-choice (format "Allow %s? " title) choices)
                   (quit nil))))
    (when answer
      (let* ((chosen-kind (car (--first (equal (cdr it) answer) kind-choices))))
        (map-elt (--first (equal (map-elt it :kind) chosen-kind) options) :option-id)))))

(defun elash-ui--response-active-p ()
  "Return non-nil if the last top-level heading is # Response."
  (elash-ui--at "^# "
    (not (looking-at "# User"))))

(defmacro elash-ui--at (re &rest body)
  "Execute BODY at the beginning of the last line matching RE, or point-max if RE is nil."
  (declare (indent 1))
  `(save-excursion
     (goto-char (point-max))
     (when ,re (re-search-backward ,re nil t))
     (beginning-of-line)
     ,@body))

(defun elash-ui--render-chunk (id title content)
  "Render CONTENT chunk under ## TITLE section for fragment ID.
When HIDE is non-nil, hide the entry after inserting."
  (goto-char (point-max))
  (when (elash-ui--at id (not (looking-at (format "^## %s" title))))
    (insert (format "\n## %s --- %s\n" title id)))
  (insert content))

(defun elash-ui--tool-call-state ()
  "Return plist of :kind, :status, :title from tool call heading at point."
  (when (looking-at "^## \\(\\S-+\\) \\(\\S-+\\) \\(.*\\) --- \\(\\S-+\\)$")
    (list :kind (car (rassoc (match-string 1) elash-ui--kinds))
          :status (car (rassoc (match-string 2) elash-ui--statuses))
          :title (s-trim (match-string 3)))))

(defun elash-ui--format-tool-heading (kind status title id)
  "Return a tool call heading string for KIND, STATUS, TITLE, ID."
  (format "## %s %s %s --- %s" (map-elt elash-ui--kinds kind) (map-elt elash-ui--statuses status) title id))

(defun elash-ui--render-tool-call (id kind title status)
  "Create a new tool call block with ID, KIND, TITLE, STATUS under # Response."
  (elash-ui--at nil (insert "\n" (elash-ui--format-tool-heading kind status title id))))

(defun elash-ui--update-tool-call (id kind title status)
  "Update KIND, TITLE, STATUS in an existing tool call block with ID.
nil fields keep their previous values."
  (elash-ui--at id
    (map-let ((:kind p-kind) (:title p-title) (:status p-status)) (elash-ui--tool-call-state)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert (elash-ui--format-tool-heading (or kind p-kind) (or status p-status) (or title p-title) id)))))

(defun elash-ui--setup-buffer (id)
  "Prepare a fresh elash buffer for session with ID."
  (visual-line-mode)
  (when (string-empty-p (buffer-string)) (insert (format "# Session --- %s\n# User\n" id)))
  (goto-char (point-max)))

(defun elash-ui--heading-line ()
  "Return the heading line string."
  (if (not elash--session) " elash"
    (map-let ((:current-model model) (:current-mode mode) (:context-used used) (:context-size size)) elash--session
      (format " model: %s | mode: %s | context: %s/%s" model mode used size))))

(defun elash-ui--session-id ()
  "Return the session id in this buffer."
  (cadr (s-match "^# Session --- \\([A-Za-z0-9_]+\\)" (buffer-string))))

(provide 'elash-ui)
;;; elash-ui.el ends here
