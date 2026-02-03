;;; tabbymacs-test.el --- Tests for tabbymacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jędrzej Kędzierski, Warsaw University of Technology, Institute of Computer Science

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

;; Tests for tabbymacs.

;; To run the tests:
;; M-x load-file RET tabbymacs-test.el RET
;; M-x tabbymacs-test RET

;;; Code:

(require 'tabbymacs)
(require 'ert)
(require 'cl-lib)
(require 'subr-x)


(defmacro tabbymacs-test-with-temp-buffer (content &rest body)
  "Create a temporary buffer with CONTENT and execute BODY.
Buffer will have `tabbymacs-mode' enabled and a mock connection."
  (declare (indent 1))
  `(let* ((buffer (generate-new-buffer " *tabbymacs-test*"))
          (tabbymacs--connection 'mock-connection)
          (tabbymacs-language-id-configuration '((text-mode . "plaintext")))
          (tabbymacs-auto-trigger nil))
     (unwind-protect
         (with-current-buffer buffer
           (text-mode)
           (insert ,content)
           (goto-char (point-min))

           ;; Mock JSONRPC functions
           (cl-letf* (((symbol-function 'jsonrpc-running-p)
                       (lambda (&rest _) t))
                      ((symbol-function 'jsonrpc-notify)
                       (lambda (&rest _) nil))
                      ((symbol-function 'jsonrpc-async-request)
                       (lambda (&rest _) nil)))
             (tabbymacs-mode)
             ,@body))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (set-buffer-modified-p nil)
           (setq buffer-file-name nil))
         (kill-buffer buffer)))))

(ert-deftest tabbymacs-test-path-to-uri ()
  "Test path to URI conversion."
  (should (string-prefix-p "file://" (tabbymacs--path-to-uri "/home/test/file.txt")))
  (should (string-suffix-p "file.txt" (tabbymacs--path-to-uri "/home/test/file.txt")))
  (should (string-match-p "%20" (tabbymacs--path-to-uri "/path with spaces/file.txt"))))

(ert-deftest tabbymacs-test-buffer-languageId ()
  "Test language ID detection."
  (tabbymacs-test-with-temp-buffer "Test content"
    (should (equal (tabbymacs--buffer-languageId) "plaintext"))
    (emacs-lisp-mode)
    (should (equal (tabbymacs--buffer-languageId) "emacs-lisp"))
    (let ((tabbymacs-language-id-configuration '((emacs-lisp-mode . "elisp"))))
      (should (equal (tabbymacs--buffer-languageId) "emacs-lisp")) ;; cached value
      (setq tabbymacs--buffer-languageId-cache nil)
      (should (equal (tabbymacs--buffer-languageId) "elisp")))))

(ert-deftest tabbymacs-test-pos-to-lsp-position ()
  "Test Emacs position conversion to LSP format."
  (tabbymacs-test-with-temp-buffer "Line 1\nLine 2\nLine3"
    (goto-char (point-min))
    (should (equal (tabbymacs--pos-to-lsp-position (point))
                   '(:line 0 :character 0)))
    (forward-line 1)
    (forward-char 3)
    (should (equal (tabbymacs--pos-to-lsp-position (point))
                   '(:line 1 :character 3)))))

(ert-deftest tabbymacs-test-lsp-position-to-pos ()
  "Test LSP position conversion to Emacs format."
  (tabbymacs-test-with-temp-buffer "Line 1\nLine 2\nLine 3"
    (should (= (tabbymacs--lsp-position-to-pos '(:line 0 :character 0))
                (point-min)))
    (let ((expected-pos (save-excursion
                          (goto-char (point-min))
                          (forward-line 1)
                          (forward-char 3)
                          (point)))
          (actual-pos (tabbymacs--lsp-position-to-pos '(:line 1 :character 3))))
      (should (= expected-pos actual-pos)))))

(ert-deftest tabbymacs-test-TextDocumentIdentifier ()
  "Test TextDocumentIdentifier creation."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq buffer-file-name "/tmp/test.txt")
    (let ((identifier (tabbymacs--TextDocumentIdentifier)))
      (should (equal (plist-get identifier :uri)
                     "file:///tmp/test.txt")))))

(ert-deftest tabbymacs-test-VersionedDocumentIdentifier ()
  "Test VersionedTextDocumentIdentifier creation."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq buffer-file-name "/tmp/test.txt")
    (setq tabbymacs--current-buffer-version 5)
    (let ((identifier (tabbymacs--VersionedTextDocumentIdentifier)))
      (should (equal (plist-get identifier :uri)
                     "file:///tmp/test.txt"))
      (should (equal (plist-get identifier :version)
                     5)))))

(ert-deftest tabbymacs-test-TextDocumentItem ()
  "Test TextDocumentItem creation."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq buffer-file-name "/tmp/test.txt")
    (let ((item (tabbymacs--TextDocumentItem)))
      (should (equal (plist-get item :uri)
                     "file:///tmp/test.txt"))
      (should (equal (plist-get item :languageId)
                     "plaintext"))
      (should (equal (plist-get item :text)
                     "Test"))
      (should (equal (plist-get item :version)
                     0)))))

(ert-deftest tabbymacs-test-TextDocumentPositionParams ()
  "Test TextDocumentPositionParams creation."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq buffer-file-name "/tmp/test.txt")
    (goto-char 3)
    (let ((params (tabbymacs--TextDocumentPositionParams)))
      (should (equal (plist-get params :position) '(:line 0 :character 2)))
      (should (plist-get params :textDocument)))))

(ert-deftest tabbymacs-test-InlineCompletionParams-invoked ()
    "Test InlineCompletionParams creation with invoked triggerKind."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq buffer-file-name "/tmp/test.txt")
    (goto-char 3)
    (let ((params (tabbymacs--InlineCompletionParams :invoked)))
      (should (equal (plist-get params :position) '(:line 0 :character 2)))
      (should (plist-get params :textDocument))
      (should (equal (plist-get (plist-get params :context) :triggerKind) 1)))))

(ert-deftest tabbymacs-test-InlineCompletionParams-automatic ()
    "Test InlineCompletionParams creation with automatic triggerKind."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq buffer-file-name "/tmp/test.txt")
    (goto-char 3)
    (let ((params (tabbymacs--InlineCompletionParams :automatic)))
      (should (equal (plist-get params :position) '(:line 0 :character 2)))
      (should (plist-get params :textDocument))
      (should (equal (plist-get (plist-get params :context) :triggerKind) 2)))))

(ert-deftest tabbymacs-test-parse-items ()
  "Test parsing inline completion items."
  ;; Test nil result
  (should (null (tabbymacs--parse-items nil)))
  ;; Test InlineCompletionList with items vector
  (let* ((result '(:items [(:insertText "test" :range (:start (:line 0 :character 0) :end (:line 0 :character 0)))]))
         (parsed (tabbymacs--parse-items result)))
    (should parsed)
    (should (listp parsed))
    (should (= (length parsed) 1))
    (should (equal (plist-get (car parsed) :insertText) "test")))
  ;; Test direct vector of plists
  (let* ((result [(:insertText "test1") (:insertText "test2")])
         (parsed (tabbymacs--parse-items result)))
    (should parsed)
    (should (listp parsed))
    (should (= (length parsed) 2))
    (should (equal (plist-get (car parsed) :insertText) "test1")))
  ;; Test direct list of plists
  (let* ((result '((:insertText "test1") (:insertText "test2")))
         (parsed (tabbymacs--parse-items result)))
    (should parsed)
    (should (listp parsed))
    (should (eq parsed result)))
  ;; Test unknown format
  (let ((result "string"))
    (should (null (tabbymacs--parse-items result)))))

(ert-deftest tabbymacs-test-ghost-text-overlay ()
  "Test ghost text overlay creation and deletion."
  (tabbymacs-test-with-temp-buffer "def main():"
    (goto-char (point-max))
    (setq tabbymacs--completions
          (list (list :insertText "\n    print('Hello')")))
    (setq tabbymacs--current-completion-id 0)
    (setq tabbymacs--start-point (point))

    (tabbymacs--show-overlay)
    (should (overlayp tabbymacs--overlay))
    (should (equal (overlay-get tabbymacs--overlay 'after-string) "\n    print('Hello')"))

    (tabbymacs--clear-overlay)
    (should (null tabbymacs--overlay))))

(ert-deftest tabbymacs-test-accept-ghost-text ()
  "Test accepting ghost text."
  (tabbymacs-test-with-temp-buffer "def main():"
    (goto-char (point-max))
    (let ((completion (list :insertText "\n    print('Hello')"
                            :range (list :start (tabbymacs--pos-to-lsp-position (point))
                                         :end (tabbymacs--pos-to-lsp-position (point))))))
      (setq tabbymacs--completions (list completion))
      (setq tabbymacs--current-completion-id 0)
      (setq tabbymacs--start-point (point))

      (tabbymacs--show-overlay)
      (tabbymacs-accept-ghost-text)
      (should (null tabbymacs--overlay))
      (should (string-match-p "print('Hello')" (buffer-string))))))

(ert-deftest tabbymacs-test-cancel-ghost-text ()
  "Test canceling ghost text."
  (tabbymacs-test-with-temp-buffer "def main():"
    (goto-char (point-max))
    (let ((completion (list :insertText "\n    print('Hello')"
                            :range (list :start (tabbymacs--pos-to-lsp-position (point))
                                         :end (tabbymacs--pos-to-lsp-position (point))))))
      (setq tabbymacs--completions (list completion))
      (setq tabbymacs--current-completion-id 0)
      (setq tabbymacs--start-point (point))

      (tabbymacs--show-overlay)
      (tabbymacs-cancel-ghost-text)
      (should (null tabbymacs--overlay))
      (should (= (point) tabbymacs--start-point))
      (should-not (string-match-p "print('Hello')" (buffer-string))))))

(ert-deftest tabbymacs-test-buffer-content ()
  "Test budder content extraction."
  (tabbymacs-test-with-temp-buffer "Line1\nLine2\Line3"
    (should (equal (tabbymacs--buffer-content) "Line1\nLine2\Line3"))))

(ert-deftest tabbymacs-test-basename ()
  "Test basename extraction."
  (should (equal (tabbymacs--basename "test.py") "test.py"))
  (should (equal (tabbymacs--basename "/home/user/test.py") "test.py"))
  (should (equal (tabbymacs--basename "/home/user/") "user")))

(ert-deftest tabbymacs-test-change-tracking ()
  "Test buffer change tracking."
  (tabbymacs-test-with-temp-buffer "Initial"
    (goto-char (point-max))
    (should (null tabbymacs--recent-changes))
    (insert " added")
    (should tabbymacs--recent-changes)))

(ert-deftest tabbymacs-test-flush-pending-changes ()
  "Test flushing pending changes."
  (tabbymacs-test-with-temp-buffer "Test"
    (setq tabbymacs--recent-changes '((:test "change")))
    (cl-letf (((symbol-function 'tabbymacs--did-change)
               (lambda ()
                 (setq tabbymacs--recent-changes nil))))
      (tabbymacs--flush-pending-changes)
      (should (null tabbymacs--recent-changes)))))

(ert-deftest tabbymacs-test-schedule-did-change ()
  "Test scheduling did-change notifications."
  (tabbymacs-test-with-temp-buffer "Test"
    (let ((tabbymacs-send-changes-idle-time 0.1)
          (timer-created nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (seconds _repeat function)
                   (setq timer-created (list seconds function))
                   'mock-timer))
                ((symbol-function 'cancel-timer) #'ignore))
        (setq tabbymacs--recent-changes '((:test "change")))
        (tabbymacs--schedule-did-change)
        (should timer-created)
        (should (equal (car timer-created) 0.1))
        (should (functionp (cadr timer-created)))
        (should (eq tabbymacs--change-idle-timer 'mock-timer))))))

(ert-deftest tabbymacs-test-schedule-inline-completion ()
  "Test scheduling inlineCompletion request."
  (tabbymacs-test-with-temp-buffer "Test"
    (let ((tabbymacs-inline-completion-idle-time 0.1)
          (timer-created nil)
          (completion-requested nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (seconds _repeat function)
                   (setq timer-created (list seconds function))
                   'mock-timer))
                ((symbol-function 'cancel-timer) #'ignore)
                ((symbol-function 'tabbymacs--auto-inline-completion)
                 (lambda ()
                   (setq completion-requested t))))
        (tabbymacs--schedule-inline-completion)
        (should timer-created)
        (should (equal (car timer-created) 0.1))
        (should (functionp (cadr timer-created)))
        (should (eq tabbymacs--inline-completion-idle-timer 'mock-timer))
        (funcall (cadr timer-created))
        (should completion-requested)))))

(defun tabbymacs-test ()
  "Test tabbymacs."
  (interactive)
  (ert-run-tests-interactively "tabbymacs-test-"))

(provide 'tabbymacs-test)

;;; tabbymacs-test.el ends here
