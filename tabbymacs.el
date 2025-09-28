;;; tabbymacs.el --- Inline AI code completions via Tabby LSP  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jędrzej Kędzierski

;; Author: Jędrzej Kędzierski <kedzierski.jedrzej@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, languages, inline completions, tabby, llm
;; URL: https://github.com/Bastillan/tabbymacs
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Emacs with Tabby - an open-source,
;; self-hosted AI coding assistant - by displaying inline completions
;; in the buffer in the form of ghost text.

;; You need to install and configure Tabby Server and Tabby Agent independently.
;; See: https://www.tabbyml.com/

;;; Code:

(require 'json)
(require 'jsonrpc)
(require 'cl-lib)
(require 'url-util)

;; ------------------------------
;; User configuration
;; ------------------------------

(defgroup tabbymacs nil
  "Inline AI code completions via Tabby LSP."
  :prefix "tabbymacs-"
  :group 'tools)

(defcustom tabbymacs-language-id-configuration
  '((c++-ts-mode . "cpp")
    (c++-mode . "cpp"))
  "Mapping of major modes to LSP language IDs.
You can extend this mapping for custom major modes."
  :type '(alist :key-type symbol :value-type string)
  :group 'tabbymacs)

(defcustom tabbymacs-send-changes-idle-time 0.5
  "Idle delay (seconds) after user input before sending didChange notification."
  :type 'number
  :group 'tabbymacs)

(defcustom tabbymacs-inline-completion-idle-time 1.0
  "Idle delay (seconds) after user input before sending inlineCompletion request."
  :type 'number
  :group 'tabbymacs)

(defcustom tabbymacs-auto-trigger t
  "If non-nil, automatically request inline completions after user input."
  :type 'boolean
  :group 'tabbymacs)

(defcustom tabbymacs-log-level :info
  "Minimum log level for Tabbymacs.
Valid values are :debug, :info, :warning, :error.
Logs below this level will be suppressed."
  :type '(choice (const :tag "Debug" :debug)
                 (const :tag "Info" :info)
                 (const :tag "Warning" :warning)
                 (const :tag "Error" :error))
  :group 'tabbymacs)

(defface tabbymacs-overlay-face
  '((t :inherit shadow))
  "Face for the inline code suggestions overlay."
  :group 'tabbymacs)

;; ------------------------------
;; Constants
;; ------------------------------

(defconst tabbymacs--triggerKind-map
  '((:invoked . 1)
    (:automatic . 2))
  "Mapping from trigger kind keywords to LSP numeric values.")

;; ------------------------------
;; Buffer-local caches & vars
;; ------------------------------

(defvar tabbymacs--debug-buffer "*tabbymacs-log*"
  "Buffer name for Tabbymacs debug logging.")

(defvar tabbymacs--connection nil
  "The JSONRPC connection to tabby-agent.")

(defvar-local tabbymacs--TextDocumentIdentifier-cache nil
  "Cached LSP TextDocumentIdentifier for the current buffer.")

(defvar-local tabbymacs--current-buffer-version 0
  "The version number of document (it increases after each change).")

(defvar-local tabbymacs--buffer-languageId-cache nil
  "Cached LSP languageId for current buffer.")

(defvar-local tabbymacs--change-idle-timer nil
  "Idle timer for batching textDocument/didChange notifications.")

(defvar-local tabbymacs--recent-changes nil
  "List of pending buffer changes for textDocument/didChange.
If Emacs gave inconsistent data it stores `:emacs-messup'.")

(defvar-local tabbymacs--completion-request-id 0
  "ID for inline completion requests.")

(defvar-local tabbymacs--inline-completion-idle-timer nil
  "Idle timer for throttling inline completion requests.")

(defvar-local tabbymacs--overlay nil
  "Overlay used to display ghost text inline completion.")

(defvar-local tabbymacs--start-point nil
  "The point where the completion started.")

(defvar-local tabbymacs--completions nil
  "The completions provided by Tabby.")

(defvar-local tabbymacs--current-completion-id nil
  "The current id of completion to be displayed.")

;; ------------------------------
;; Utility functions
;; ------------------------------

(defun tabbymacs--path-to-uri (path)
  "Convert PATH to a proper file:// URI with escaping."
  (concat "file://"
          (url-hexify-string (expand-file-name (file-local-name path))
                             url-path-allowed-chars)))

(defun tabbymacs--basename (path)
  "Return the last component of PATH, without trailing slash."
  (file-name-nondirectory (directory-file-name path)))

(defun tabbymacs--buffer-languageId ()
  "Get languageId corresponding to current buffer."
  (or tabbymacs--buffer-languageId-cache
      (setq tabbymacs--buffer-languageId-cache
            (or (cdr (assoc major-mode tabbymacs-language-id-configuration))
                (let ((name (symbol-name major-mode)))
                  (cond
                   ((string-suffix-p "-ts-mode" name)
                    (string-remove-suffix "-ts-mode" name))
                   ((string-suffix-p "-mode" name)
                    (string-remove-suffix "-mode" name))
                   (t nil))))))
  (unless tabbymacs--buffer-languageId-cache
    (tabbymacs--log :warning
                    "Unable to extract languageId for buffer `%s'. Major mode: %s"
                    (buffer-name) major-mode))
  tabbymacs--buffer-languageId-cache)

(defun tabbymacs--TextDocumentIdentifier ()
  "Return TextDocumentIdentifier object for the current buffer."
  (or tabbymacs--TextDocumentIdentifier-cache
      (setq tabbymacs--TextDocumentIdentifier-cache
            (list :uri (tabbymacs--path-to-uri buffer-file-name)))))

(defun tabbymacs--VersionedTextDocumentIdentifier ()
  "Return VersionedTextDocumentIdentifier object for the current buffer."
  (append (tabbymacs--TextDocumentIdentifier)
          (list :version tabbymacs--current-buffer-version)))

(defun tabbymacs--buffer-content ()
  "Return content of the current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun tabbymacs--TextDocumentItem ()
  "Return TextDocumentItem object for the current buffer."
  (append (tabbymacs--VersionedTextDocumentIdentifier)
          (list :languageId (tabbymacs--buffer-languageId)
                :text (tabbymacs--buffer-content))))

(defun tabbymacs--TextDocumentContentChangeEvents ()
  "Return list of TextDocumentContentChangeEvent objects for the current buffer."
  (let ((full-sync-p (eq tabbymacs--recent-changes :emacs-messup)))
    (if full-sync-p
        (vector (list :text (tabbymacs--buffer-content)))
      (cl-loop for (beg end len text) in (reverse tabbymacs--recent-changes)
               vconcat `[,(list :range `(:start ,beg :end ,end)
                                :rangeLength len
                                :text text)]))))

(defun tabbymacs--TextDocumentPositionParams ()
  "Return TextDocumentPositionParams object for the current buffer."
  (list :textDocument (tabbymacs--TextDocumentIdentifier)
        :position (tabbymacs--pos-to-lsp-position (point))))

(defun tabbymacs--InlineCompletionParams (trigger-kind)
  "Return InlineCompletionParams object.
TRIGGER-KIND should be one of :invoked or :automatic."
  (let ((kind (or (cdr (assoc trigger-kind tabbymacs--triggerKind-map))
                  1)))
    (append (tabbymacs--TextDocumentPositionParams)
            `(:context (:triggerKind ,kind)))))

(defun tabbymacs--pos-to-lsp-position (pos)
  "Convert buffer position POS to an LSP position."
  (save-excursion
    (goto-char pos)
    (list :line (1- (line-number-at-pos))
          :character (- (point) (line-beginning-position)))))

(defun tabbymacs--lsp-position-to-pos (pos)
  "Convert LSP position POS (:line and :character) to buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line (plist-get pos :line))
    (forward-char (plist-get pos :character))
    (point)))

(defun tabbymacs--log (level fmt &rest args)
  "Log a message for Tabbymacs.
LEVEL is one of :debug, :info, :warning, :error.
FMT and ARGS are like in `message'."
  (let* ((order '(:debug :info :warning :error))
         (min-idx (cl-position tabbymacs-log-level order))
         (lvl-idx (cl-position level order)))
    (when (and lvl-idx min-idx (>= lvl-idx min-idx))
      (let ((msg (apply #'format fmt args)))
        (pcase level
          (:debug
           (with-current-buffer (get-buffer-create tabbymacs--debug-buffer)
             (goto-char (point-max))
             (insert (format-time-string "[%F %T] "))
             (insert "[DEBUG] " msg "\n")))
          (:info (message "[Tabbymacs] %s" msg))
          (:warning (display-warning 'tabbymacs msg :warning))
          (:error (display-warning 'tabbymacs msg :error))
          (_ (message "[Tabbymacs][UNKNOWN] %s" msg)))))))

;; ------------------------------
;; JSONRPC Connection
;; ------------------------------

(defun tabbymacs--make-connection ()
  "Start a JSONRPC connection to tabby-agent."
  (setq tabbymacs--connection
        (make-instance
         'jsonrpc-process-connection
         :name "tabby-agent"
         :process (lambda ()
                    (make-process
                     :name "tabby-agent"
                     :command '("tabby-agent" "--stdio")
                     :connection-type 'pipe
                     :coding 'utf-8-emacs-unix
                     :stderr "*tabby-agent-stderr*"))
         :request-dispatcher
         (lambda (_conn method params)
           (tabbymacs--log :debug
                           "[tabby-agent] Request from server: %s %S"
                           method params)
           nil)
         :notification-dispatcher
         (lambda (_conn method params)
           (tabbymacs--log :debug
                           "[tabby-agent] Notification: %s %S"
                           method params)))))

(defun tabbymacs--connect ()
  "Connect to tabby-agent if not already connected."
  (unless (and tabbymacs--connection
               (jsonrpc-running-p tabbymacs--connection))
    (tabbymacs--make-connection)
    (let* ((root (expand-file-name default-directory))
           (uri (tabbymacs--path-to-uri root))
           (init-params
            `(:processId ,(emacs-pid)
                         :clientInfo (:name "emacs" :version ,(emacs-version))
                         :rootPath ,(file-local-name root)
                         :rootUri ,uri
                         :workspaceFolders
                         [(:uri ,uri
                                :name ,(tabbymacs--basename root))]
                         :capabilities
                         (:textDocument
                          (:inlineCompletion
                           (:dynamicRegistration :json-false))))))
      (jsonrpc-async-request
       tabbymacs--connection
       :initialize init-params
       :success-fn (lambda (_result)
                     (jsonrpc-notify
                      tabbymacs--connection
                      :initialized nil)
                     (tabbymacs--log :info "Tabby-agent initialized."))
       :error-fn (lambda (err)
                   (tabbymacs--log :error "Tabby-agent init failed: %S" err))))))

(defun tabbymacs--disconnect ()
  "Shutdown the connection to tabby-agent."
  (when (and tabbymacs--connection
             (jsonrpc-running-p tabbymacs--connection))
    (jsonrpc-async-request
     tabbymacs--connection
     :shutdown nil
     :success-fn (lambda (_result)
                   (jsonrpc-notify
                    tabbymacs--connection
                    :exit nil)
                   (jsonrpc-shutdown
                    tabbymacs--connection)
                   (setq tabbymacs--connection nil)
                   (tabbymacs--log :info "Stopped tabby-agent connection."))
     :error-fn (lambda (err)
                 (tabbymacs--log :error "tabby-agent shutdown failed: %S" err)))))

;; ------------------------------
;; Buffer notifications
;; ------------------------------

(defun tabbymacs--did-open ()
  "Send textDocument/didOpen notification for the current buffer."
  (when (and buffer-file-name tabbymacs--connection)
    (jsonrpc-notify
     tabbymacs--connection
     :textDocument/didOpen
     `(:textDocument ,(tabbymacs--TextDocumentItem)))))

(defun tabbymacs--did-close ()
  "Send textDocument/didClose notification for the current buffer."
  (when (and buffer-file-name tabbymacs--connection)
    (jsonrpc-notify
     tabbymacs--connection
     :textDocument/didClose
     `(:textDocument ,(tabbymacs--TextDocumentIdentifier)))))

(defun tabbymacs--did-change ()
  "Send textDocument/didChange notification for the current buffer."
  (when (and tabbymacs--recent-changes tabbymacs--connection buffer-file-name)
    (jsonrpc-notify
     tabbymacs--connection
     :textDocument/didChange
     (list :textDocument (tabbymacs--VersionedTextDocumentIdentifier)
           :contentChanges (tabbymacs--TextDocumentContentChangeEvents))))
  (setq tabbymacs--recent-changes nil))

;; ------------------------------
;; Inline completion
;; ------------------------------

(defun tabbymacs--flush-pending-changes ()
  "Ensure all textDocument/didChange notifications are sent before requests."
  (when tabbymacs--change-idle-timer
    (cancel-timer tabbymacs--change-idle-timer)
    (setq tabbymacs--change-idle-timer nil))
  (when tabbymacs--recent-changes
    (tabbymacs--did-change)))

(defun tabbymacs--auto-inline-completion ()
  "Request inline completion automatically, if enabled."
  (when (and tabbymacs-auto-trigger
             (not (minibufferp))
             (not (eq this-command 'tabbymacs--invoked-inline-completion))) ;; don't recurse
    (tabbymacs--inline-completion :automatic)))

(defun tabbymacs-invoked-inline-completion ()
  "Request inline completion manually at point."
  (interactive)
  (tabbymacs--inline-completion :invoked))

(defun tabbymacs--inline-completion (trigger-kind)
  "Request inline completion from tabby-agent at point.
TRIGGER-KIND is :invoked (manual) or :automatic (after typing)."
  (tabbymacs--flush-pending-changes)
  (when (and tabbymacs--connection buffer-file-name)
    (let ((req-id (cl-incf tabbymacs--completion-request-id))
          (buf (current-buffer))
          (start (point))
          (mod-tick (buffer-chars-modified-tick)))
      (jsonrpc-async-request
       tabbymacs--connection
       :textDocument/inlineCompletion
       (tabbymacs--InlineCompletionParams trigger-kind)
       :success-fn
       (lambda (result)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and (= req-id tabbymacs--completion-request-id)
                        (= mod-tick (buffer-chars-modified-tick)))
               (setq tabbymacs--start-point start)
               (tabbymacs--handle-inline-completion result)))))
       :error-fn
       (lambda (err)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (= req-id tabbymacs--completion-request-id)
               (tabbymacs--log :error "inlineCompletion error: %S" err)
               (tabbymacs--clear-overlay)))))))))

(defun tabbymacs--parse-items (result)
  "Parse the textDocument/inlineCompletion RESULT.
Return a list of InlineCompletionItem objects.
RESULT may be:
- an InlineCompletionList with an :items field,
- a vector or list of InlineCompletionItem,
- nil (no completions)."
  (cond
   ;; No completions
   ((null result) nil)
   ;; InlineCompletionList with :items
   ((and (listp result) (plist-get result :items))
    (let ((items (plist-get result :items)))
      (cond
       ((vectorp items) (append items nil))
       ((listp items) items)
       (t nil))))
   ;; InlineCompletionItem[] (vector)
   ((vectorp result)
    (append result nil))
   ;; InlineCompletionItem[] (list)
   ((listp result)
    result)
   ;; Fallback: unknown type
   (t
    (tabbymacs--log :warning "Unexpected inline completion result format: %S" result)
    nil)))

(defun tabbymacs--handle-inline-completion (result)
  "Handle RESULT of inline completion request."
  (let ((items (tabbymacs--parse-items result)))
    (if items
        (progn
          (tabbymacs--log :debug "Inline suggestions: %S" items)
          (tabbymacs--show-ghost-text items))
      (tabbymacs--log :debug "No inline suggestions."))))

;; ------------------------------
;; Hooks
;; ------------------------------

(defun tabbymacs--before-change (beg end)
  "Record BEG and END (as positions and markers) before a buffer change."
  (when (listp tabbymacs--recent-changes)
    (push `(,(tabbymacs--pos-to-lsp-position beg)
            ,(tabbymacs--pos-to-lsp-position end)
            (,beg . ,(copy-marker beg nil)) ;; non-inserting marker
            (,end . ,(copy-marker end t))) ;; inserting marker
          tabbymacs--recent-changes)))

(defun tabbymacs--after-change (beg end pre-change-length)
  "Record BEG, END, PRE-CHANGE-LENGTH and changed TEXT."
  (cl-incf tabbymacs--current-buffer-version)
  (tabbymacs--process-recent-change beg end pre-change-length)
  (tabbymacs--schedule-did-change))

(defun tabbymacs--process-recent-change (beg end pre-change-length)
  "Processes the most recent change record.
Use BEG END and PRE-CHANGE-LENGTH from after-change hook.
See Eglot's issues #259 and #367 for reference."
  (pcase (car-safe tabbymacs--recent-changes)
    (`(,lsp-beg, lsp-end
                 (,b-beg . ,b-beg-marker)
                 (,b-end . ,b-end-marker))
     (if (and (= b-end b-end-marker) (= b-beg b-beg-marker)
              (or (/= beg b-beg) (/= end b-end)))
         (setcar tabbymacs--recent-changes
                 `(,lsp-beg ,lsp-end ,(- b-end-marker b-beg-marker)
                            ,(buffer-substring-no-properties b-beg-marker
                                                             b-end-marker)))
       (setcar tabbymacs--recent-changes
               `(,lsp-beg ,lsp-end ,pre-change-length
                         ,(buffer-substring-no-properties beg end)))))
    (_ (setq tabbymacs--recent-changes :emacs-messup))))

(defun tabbymacs--schedule-did-change ()
  "Schedule an didChange notification after idle."
  (when tabbymacs--change-idle-timer
    (cancel-timer tabbymacs--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq tabbymacs--change-idle-timer
          (run-with-idle-timer
           tabbymacs-send-changes-idle-time nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (tabbymacs--did-change)
                 (setq tabbymacs--change-idle-timer nil))))))))

(defun tabbymacs--schedule-inline-completion ()
  "Schedule an inlineCompletion request after idle."
  (when tabbymacs--inline-completion-idle-timer
    (cancel-timer tabbymacs--inline-completion-idle-timer))
  (let ((buf (current-buffer)))
    (setq tabbymacs--inline-completion-idle-timer
          (run-with-idle-timer
           tabbymacs-inline-completion-idle-time nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (tabbymacs--auto-inline-completion))))))))

(defun tabbymacs--enable-hooks ()
  "Enable before and after change hooks for LSP didChange tracking.
Enable post-self-insert hook for inlineCompletion."
  (add-hook 'before-change-functions #'tabbymacs--before-change nil t)
  (add-hook 'after-change-functions #'tabbymacs--after-change nil t)
  (when tabbymacs-auto-trigger
      (add-hook 'post-self-insert-hook #'tabbymacs--schedule-inline-completion nil t)))

(defun tabbymacs--disable-hooks ()
  "Disable before and after change hooks as well as post-self-insert."
  (remove-hook 'before-change-functions #'tabbymacs--before-change t)
  (remove-hook 'after-change-functions #'tabbymacs--after-change t)
  (when tabbymacs-auto-trigger
    (remove-hook 'post-self-insert-hook #'tabbymacs--schedule-inline-completion t)))

;; ------------------------------
;; Ghost text
;; ------------------------------

(defun tabbymacs--clear-overlay ()
  "Remove ghost text overlay if present."
  (when (overlayp tabbymacs--overlay)
    (delete-overlay tabbymacs--overlay))
  (setq tabbymacs--overlay nil)
  (tabbymacs--deactivate-overlay-map))

(defun tabbymacs--get-overlay (beg end)
  "Build the suggestion overlay spanning from BEG to END."
  (tabbymacs--clear-overlay)
  (setq tabbymacs--overlay (make-overlay beg end nil nil t))
  (tabbymacs--activate-overlay-map)
  tabbymacs--overlay)

(defun tabbymacs--show-ghost-text (items)
  "Display ITEMS as ghost text overlay at point."
  (tabbymacs--clear-overlay)
  (setq tabbymacs--completions items
        tabbymacs--current-completion-id 0)
  (tabbymacs--show-overlay))

(defun tabbymacs--show-overlay ()
  "Make the suggestion overlay visible."
  (unless (and tabbymacs--completions
               (numberp tabbymacs--current-completion-id))
    (error "No completions to show"))
  (tabbymacs--clear-overlay)
  (let* ((suggestion (elt tabbymacs--completions tabbymacs--current-completion-id))
         (insert-text (plist-get suggestion :insertText))
         (range (plist-get suggestion :range))
         (start (when range
                  (tabbymacs--lsp-position-to-pos (plist-get range :start))))
         (end (when range
                (tabbymacs--lsp-position-to-pos (plist-get range :end))))
         (beg (or start tabbymacs--start-point))
         (end-point (or end beg))
         (propertized-text (propertize insert-text 'face 'tabbymacs-overlay-face))
         (ov (tabbymacs--get-overlay beg end-point))
         (display-str (substring insert-text 0 (- tabbymacs--start-point beg)))
         (after-str (substring propertized-text (- tabbymacs--start-point beg))))
    (put-text-property 0 (length after-str) 'cursor t after-str)
    (overlay-put ov 'display display-str)
    (overlay-put ov 'after-string after-str)
    (goto-char tabbymacs--start-point)))

(defun tabbymacs-accept-ghost-text ()
  "Accept currently shown ghost text into buffer."
  (interactive)
  (when (and (overlayp tabbymacs--overlay)
             tabbymacs--completions
             (numberp tabbymacs--current-completion-id))
    (let* ((suggestion (elt tabbymacs--completions
                            tabbymacs--current-completion-id))
           (insert-text (plist-get suggestion :insertText))
           (range (plist-get suggestion :range))
           (start (when range
                    (tabbymacs--lsp-position-to-pos (plist-get range :start))))
           (end (when range
                  (tabbymacs--lsp-position-to-pos (plist-get range :end)))))
      (tabbymacs--clear-overlay)
      (when (and start end)
        (delete-region start end)
        (goto-char start))
      (insert insert-text))))

(defun tabbymacs-cancel-ghost-text ()
  "Cancel ghost text overlay."
  (interactive)
  (let ((was-active (overlayp tabbymacs--overlay)))
    (tabbymacs--clear-overlay)
    (when was-active
      (goto-char tabbymacs--start-point))))

(defun tabbymacs-cancel-ghost-text-with-input (event)
  "Cancel ghost text overlay and replay the triggering EVENT."
  (interactive (list last-input-event))
  (tabbymacs-cancel-ghost-text)
  (setq unread-command-events (nconc unread-command-events (list event))))

(defvar tabbymacs-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'tabbymacs-accept-ghost-text)
    (define-key map (kbd "C-<return>") #'tabbymacs-accept-ghost-text)
    (define-key map (kbd "C-g") #'tabbymacs-cancel-ghost-text)
    (define-key map (kbd "<escape>") #'tabbymacs-cancel-ghost-text)
    (define-key map [t] #'tabbymacs-cancel-ghost-text-with-input)
    map)
  "Keymap active when ghost text overlay is visible.")

(defun tabbymacs--activate-overlay-map ()
  "Activate ghost text overlay keymap."
  (internal-push-keymap tabbymacs-overlay-map 'overriding-terminal-local-map))

(defun tabbymacs--deactivate-overlay-map ()
  "Deactivate ghost text overlay keymap."
  (internal-pop-keymap tabbymacs-overlay-map 'overriding-terminal-local-map))

;; ------------------------------
;; Minor mode
;; ------------------------------

(defun tabbymacs--reset-vars ()
  "Reset buffer local caches and vars."
  (setq tabbymacs--recent-changes nil
        tabbymacs--current-buffer-version 0
        tabbymacs--change-idle-timer nil
        tabbymacs--TextDocumentIdentifier-cache nil
        tabbymacs--buffer-languageId-cache nil
        tabbymacs--inline-completion-idle-timer nil
        tabbymacs--completion-request-id 0))

(defun tabbymacs--setup ()
  "Set up `tabbymacs-mode'."
  (progn
    (tabbymacs--connect)
    (tabbymacs--reset-vars)
    (tabbymacs--did-open)
    (tabbymacs--enable-hooks)))

(defun tabbymacs--cleanup ()
  "Clean up `tabbymacs-mode'."
  (progn
    (when tabbymacs--change-idle-timer
      (cancel-timer tabbymacs--change-idle-timer))
    (when tabbymacs--recent-changes
      (tabbymacs--did-change))
    (tabbymacs--disable-hooks)
    (tabbymacs--did-close)
    (tabbymacs--reset-vars)
    (unless (cl-some (lambda (buf)
                       (buffer-local-value 'tabbymacs-mode buf))
                     (buffer-list))
      (tabbymacs--disconnect))))

(defvar tabbymacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c /") #'tabbymacs-invoked-inline-completion)
    map)
  "Keymap for `tabbymacs-mode'.")

;;;###autoload
(define-minor-mode tabbymacs-mode
  "Minor mode for inline completion via Tabby LSP."
  :lighter " Tabbymacs"
  :group 'tabbymacs
  :keymap tabbymacs-mode-map
  (if tabbymacs-mode
      (tabbymacs--setup)
    (tabbymacs--cleanup)))

(provide 'tabbymacs)
;;; tabbymacs.el ends here

