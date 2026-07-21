;;; elash.el --- Elegant AI Shell  -*- lexical-binding: t; fill-column: 120; -*-

;; Copyright (C) 2026 Adam Niederer

;; Author: Adam Niederer
;; URL: https://github.com/adamniederer/elash
;; Version: 0.1.0
;; Package-Requires: ((emacs "32") (acp "0.12.1") (dash "2.19.1") (s "1.13.0") (f "0.21.0"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; COMMENTARY:
;;
;; An agentic shell with a focus on code quality and maintainability
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'f)
(require 'acp)
(require 'map)
(require 'elash-acp)
(require 'elash-ui)

(defvar elash--session nil
  "Plist of the current elash session state.

Keys:
:client             - ACP client
:session-id         - ACP session ID
:available-models   - List of available models
:current-model      - Currently selected model
:available-modes    - List of available agent modes
:current-mode       - Currently active mode ID
:context-used       - Context used
:context-size       - Maximum context size")

(defun elash--on-notification (notification)
  "Handle streaming ACP NOTIFICATION.
Updates fragments and renders content into the elash buffer."
  (let ((update (map-nested-elt (elash-acp--normalize notification) '(:params :update))))
    (pcase update
      ((map (:session-update "agent_message_chunk") (:message-id id) (:content (map (:text text))))
       (elash-ui--render-chunk id "Message" text))
      ((map (:session-update "agent_thought_chunk") (:message-id id) (:content (map (:text text))))
       (elash-ui--render-chunk id "Thinking" text))
      ((map (:session-update "tool_call") (:tool-call-id tool-call-id) (:kind kind) (:title title) (:status status))
       (elash-ui--render-tool-call tool-call-id kind title status))
      ((map (:session-update "current_mode_update" :mode-id id))
       (map-put! elash--session :current-mode id))
      ((map (:session-update "tool_call_update") (:tool-call-id tool-call-id) (:kind kind) (:title title) (:status status))
       (elash-ui--update-tool-call tool-call-id kind title status))
      ((map (:session-update "usage_update") (:used used) (:size size))
       (map-put! elash--session :context-used used)
       (map-put! elash--session :context-size size)))))

(defun elash--on-request (request)
  "Handle incoming ACP REQUEST from the agent."
  (acp-send-response
   :client (map-elt elash--session :client)
   :response
   (pcase (elash-acp--normalize request)
     ((map (:method "fs/read_text_file") (:id id) (:params (map :path path)))
      (apply #'acp-make-fs-read-text-file-response
             `(:request-id ,id ,@(condition-case nil `(:content ,(f-read-text path))
                                   (error `(:error ,(acp-make-error :code -32603 :message "Failed to read file.")))))))
     ((map (:method "fs/write_text_file") (:id id) (:params (map (:path path) (:content content))))
      (apply #'acp-make-fs-write-text-file-response
             `(:request-id ,id ,@(condition-case nil (f-write-text content 'utf-8 path)
                                   (error `(:error ,(acp-make-error :code -32603 :message "Failed to write file.")))))))
     ((map (:method "session/request_permission") (:id id) (:params (map (:options options) (:tool-call (map (:title title "[unknown tool]"))))))
      (apply #'acp-make-session-request-permission-response
             `(:request-id ,id ,@(if-let* ((answer (elash-ui--ask-permission options title))) `(:option-id ,answer) `(:cancelled t))))))))

(defun elash--on-err (err)
  "Handle an ACP error ERR."
  (elash-ui--handle-error err))

(defun elash--apply-config-options (options)
  "Apply config OPTIONS plist list to `elash--session'."
  (--each options
    (pcase it
      ((map (:id "model") (:options options) (:current-value current-value))
       (map-put! elash--session :available-models options)
       (map-put! elash--session :current-model current-value))
      ((map (:id "mode") (:options options) (:current-value current-value))
       (map-put! elash--session :available-modes options)
       (map-put! elash--session :current-mode current-value)))))

(defun elash--complete-option (session-key label)
  "Read a config option value from `elash--session' SESSION-KEY with LABEL prompt."
  (let* ((options (map-elt elash--session session-key))
         (name (completing-read (format "%s: " label) (--map (map-elt it :name) options) nil t)))
    (map-elt (--first (equal (map-elt it :name) name) options) :value)))

(defun elash--init-acp (command environment)
  "Initialize the ACP connection using COMMAND and ENVIRONMENT in the current buffer."
  (let ((client (acp-make-client :command (car command)
                                 :command-params (cdr command)
                                 :environment-variables environment
                                 :context-buffer (current-buffer))))
    (setq elash--session `(:client ,client))
    (acp-subscribe-to-notifications :client client :on-notification #'elash--on-notification)
    (acp-subscribe-to-requests :client client :on-request #'elash--on-request)
    (acp-send-request :client client :sync t
                      :request (acp-make-initialize-request
                                :protocol-version 1))))

(defun elash--new-session (client)
  "Send a session/new requset using CLIENT to the ACP agent."
  (let ((normalized (elash-acp--normalize (acp-send-request :client client :sync t :request (acp-make-session-new-request :cwd default-directory)))))
    (map-put! elash--session :session-id (map-elt normalized :session-id))
    (elash--apply-config-options (map-elt normalized :config-options))
    (elash-set-model "llama.cpp/qwen3.6:35ba3b-iq4_xs")))

(defun elash--resume-session (client id)
  "Send a session/resume request using CLIENT to the ACP agent for session with ID."
  (let ((normalized (elash-acp--normalize (acp-send-request :client client :sync t :request (acp-make-session-resume-request :session-id id :cwd default-directory)))))
    (map-put! elash--session :session-id id)
    (elash--apply-config-options (map-elt normalized :config-options))
    (elash-set-model "llama.cpp/qwen3.6:35ba3b-iq4_xs")))

(defun elash--send-prompt (text)
  "Send TEXT as an ACP session/prompt request."
  (acp-send-request
   :client (map-elt elash--session :client)
   :request (acp-make-session-prompt-request
             :session-id (map-elt elash--session :session-id)
             :prompt `(((type . "text") (text . ,text))))
   :on-success (lambda (&rest _) (elash-ui--finalize-response))))

(defun elash--cleanup ()
  "Shut down ACP when the buffer is killed."
  (when elash--session (acp-shutdown :client (map-elt elash--session :client)))
  (setq elash--session nil))

;;;###autoload
(defun elash-start (command name environment)
  "Start a session with the given agent configuration.

COMMAND is a list (EXECUTABLE ARGS...) for the ACP agent process.
NAME is the name of the agent harness.
ENVIRONMENT is an optional list of \"KEY=val\" strings for the agent."
  (switch-to-buffer (get-buffer-create (format "*elash: %s*" name)))
  (elash--init-acp command environment)
  (elash--new-session (map-elt elash--session :client))
  (elash-ui--setup-buffer (map-elt elash--session :session-id)))

;;;###autoload
(defun elash-resume (command name environment)
  "Resume a session from the markdown transcript in the current buffer.

COMMAND is a list (EXECUTABLE ARGS...) for the ACP agent process.
NAME is the name of the agent harness.
ENVIRONMENT is an optional list of \"KEY=val\" strings for the agent."
  (elash--init-acp command environment)
  (elash--resume-session (map-elt elash--session :client) (elash-ui--session-id))
  (elash-ui--setup-buffer (map-elt elash--session :session-id)))

(defun elash-set-model (model-id)
  "Set the ACP model for the current elash session to MODEL-ID."
  (interactive (list (elash--complete-option :available-models "Model")))
  (unless (map-elt elash--session :session-id) (user-error "Start an elash session first"))
  (acp-send-request :client (map-elt elash--session :client)
                    :request (acp-make-session-set-model-request
                              :session-id (map-elt elash--session :session-id)
                              :model-id model-id)
                    :on-success (lambda (&rest _)
                                  (map-put! elash--session :current-model model-id)
                                  (message "Model set to %s" model-id))
                    :on-failure (lambda (err _) (message "Failed to set model: %s" err))))

(defun elash-set-mode (mode-id)
  "Set the ACP mode for the current elash session to MODE-ID."
  (interactive (list (elash--complete-option :available-modes "Mode")))
  (unless (map-elt elash--session :session-id) (user-error "Start an elash session first"))
  (acp-send-request :client (map-elt elash--session :client)
                    :request (acp-make-session-set-mode-request
                              :session-id (map-elt elash--session :session-id)
                              :mode-id mode-id)
                    :on-success (lambda (&rest _)
                                  (map-put! elash--session :current-mode mode-id)
                                  (message "Mode set to %s" mode-id))
                    :on-failure (lambda (err _) (message "Failed to set mode: %s" err))))

(defun elash-cancel ()
  "Send an ACP cancellation request for the current session."
  (interactive)
  (unless (map-elt elash--session :session-id) (user-error "Start an elash session first"))
  (acp-send-request :client (map-elt elash--session :client)
                    :request (acp-make-session-cancel-notification :session-id (map-elt elash--session :session-id))
                    :on-success #'elash-ui--finalize-response
                    :on-failure (lambda (err _) (message "Failed to cancel: %s" err))))

(provide 'elash)
;;; elash.el ends here
