;;; lean4-tactic.el --- Tactic automation for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2024 George Seelinger. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at:
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This library provides tactic automation commands for `lean4-mode'.
;; Currently it supports replacing `simp' with the output of `simp?',
;; i.e., replacing a bare `simp' with a more precise `simp only [...]'
;; call using the suggestion provided by the Lean language server.
;;
;; Two commands are provided:
;;   `lean4-apply-simp-suggestion'              -- at point
;;   `lean4-apply-simp-suggestion-whole-buffer' -- whole buffer

;;; Code:

(require 'cl-lib)
(require 'lean4-util)
(require 'lean4-info)

(defun lean4--find-simp-on-line ()
  "Return (BEG . END) of the first plain `simp' on the current line, or nil.
A \"plain\" simp is one not already followed by `?', `_', or ` only'."
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position))
          result)
      (while (and (not result)
                  (re-search-forward "\\bsimp\\b" eol t))
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (unless (lean4-in-comment-p)
            (save-excursion
              (goto-char end)
              (unless (or (eq (char-after) ??)
                          (eq (char-after) ?_)
                          (looking-at-p "\\s-+only\\b"))
                (setq result (cons beg end)))))))
      result)))

(defun lean4--try-this-at-line (line)
  "Return (REPLACEMENT START END) for a `Try this' diagnostic at LINE (0-indexed).
START and END are buffer positions for the replacement region.
Returns nil if no suggestion is available yet."
  (cl-loop for diag in (lean4-info--diagnostics)
           for msg = (let ((m (cl-getf diag :message)))
                       (cond ((stringp m) m)
                             ((consp m) (or (nth 2 m) ""))
                             (t "")))
           when (and (string-match "\\`Try this: \\(.*\\)" msg)
                     (equal (lean4-info--diagnostic-start diag) line))
           return (let* ((replacement (match-string 1 msg))
                         (range (cl-getf diag :range))
                         (start-pos (and range (cl-getf range :start)))
                         (end-pos (and range (cl-getf range :end)))
                         (start-char (and start-pos (cl-getf start-pos :character)))
                         (end-char (and end-pos (cl-getf end-pos :character))))
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line line)
                      (beginning-of-line)
                      (let ((bol (point)))
                        (cond
                         ;; Use the exact character range from the diagnostic.
                         ;; Note: LSP uses UTF-16 character offsets; for typical
                         ;; ASCII Lean source this equals the byte offset.
                         ((and start-char end-char)
                          (list replacement
                                (+ bol start-char)
                                (+ bol end-char)))
                         ;; Fallback: locate `simp?' on the line and include
                         ;; any immediately following bracketed arguments.
                         ((re-search-forward "\\bsimp[?]" (line-end-position) t)
                          (let* ((match-start (match-beginning 0))
                                 (after-keyword (match-end 0))
                                 (arg-end
                                  (save-excursion
                                    (goto-char after-keyword)
                                    (skip-chars-forward " \t")
                                    (if (memq (char-after) '(?\[ ?\())
                                        (ignore-errors
                                          (forward-list)
                                          (point))
                                      after-keyword))))
                            (list replacement match-start
                                  (or arg-end after-keyword))))))))))

(defun lean4--wait-and-apply-try-this (buf line retries)
  "In BUF, wait for a `Try this' diagnostic at LINE (0-indexed) and apply it.
Retries up to RETRIES times with a 0.5 s delay between attempts."
  (if (not (buffer-live-p buf))
      (message "lean4: Buffer was killed; aborting simp replacement")
    (with-current-buffer buf
      (let ((info (lean4--try-this-at-line line)))
        (if info
            (let ((replacement (nth 0 info))
                  (start (nth 1 info))
                  (end (nth 2 info)))
              (if (and start end)
                  (progn
                    (delete-region start end)
                    (goto-char start)
                    (insert replacement)
                    (message "lean4: Applied: %s" replacement))
                (message "lean4: Found suggestion but could not locate range on line %d"
                         (1+ line))))
          (if (> retries 0)
              (run-with-timer 0.5 nil
                              #'lean4--wait-and-apply-try-this
                              buf line (1- retries))
            (message "lean4: Timed out waiting for `Try this' on line %d"
                     (1+ line))))))))

(defun lean4--wait-and-apply-all-try-this (buf lines retries)
  "In BUF, wait for `Try this' diagnostics for all LINES (0-indexed) and apply them.
Retries up to RETRIES times (0.5 s apart) for any lines not yet resolved."
  (if (not (buffer-live-p buf))
      (message "lean4: Buffer was killed; aborting simp replacement")
    (with-current-buffer buf
      (let (resolved remaining)
        (dolist (line lines)
          (let ((info (lean4--try-this-at-line line)))
            (if (and info (nth 1 info) (nth 2 info))
                (push (cons line info) resolved)
              (push line remaining))))
        ;; Apply resolved replacements from last to first line so that
        ;; earlier buffer positions are not invalidated by later edits.
        (when resolved
          (dolist (r (sort resolved (lambda (a b) (> (car a) (car b)))))
            (let* ((info (cdr r))
                   (replacement (nth 0 info))
                   (start (nth 1 info))
                   (end (nth 2 info)))
              (delete-region start end)
              (goto-char start)
              (insert replacement))))
        (if remaining
            (if (> retries 0)
                (run-with-timer 0.5 nil
                                #'lean4--wait-and-apply-all-try-this
                                buf remaining (1- retries))
              (message "lean4: Timed out waiting for `Try this' for %d simp(s)"
                       (length remaining)))
          (message "lean4: Applied `Try this' suggestion(s) for %d simp(s)"
                   (length lines)))))))

;;;###autoload
(defun lean4-apply-simp-suggestion ()
  "Replace the `simp' tactic on the current line with the output of `simp?'.
This temporarily replaces `simp' with `simp?' and then automatically
applies the `Try this' suggestion from the Lean language server,
resulting in a more precise `simp only [...]' call."
  (interactive)
  (let ((match (lean4--find-simp-on-line)))
    (if match
        (let* ((beg (car match))
               (end (cdr match))
               (line (1- (line-number-at-pos beg t)))
               (buf (current-buffer)))
          (delete-region beg end)
          (goto-char beg)
          (insert "simp?")
          (lean4--wait-and-apply-try-this buf line 20))
      (message "lean4: No plain `simp' found on the current line"))))

;;;###autoload
(defun lean4-apply-simp-suggestion-whole-buffer ()
  "Replace all plain `simp' tactics in the buffer with their `simp?' suggestions.
For each `simp' found (excluding `simp only', `simp?', and `simp_*' variants),
this replaces it with `simp?' and waits for the Lean language server's
`Try this' suggestion, typically yielding a `simp only [...]' call.
Processes the buffer from end to beginning so position shifts do not
affect earlier occurrences."
  (interactive)
  (let (lines
        (buf (current-buffer)))
    ;; Walk backward so replacements do not shift the positions of earlier matches.
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "\\bsimp\\b" nil t)
        (unless (lean4-in-comment-p)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (save-excursion
              (goto-char end)
              (unless (or (eq (char-after) ??)
                          (eq (char-after) ?_)
                          (looking-at-p "\\s-+only\\b"))
                (let ((line (1- (line-number-at-pos beg t))))
                  (delete-region beg end)
                  (goto-char beg)
                  (insert "simp?")
                  (push line lines))))))))
    (if (null lines)
        (message "lean4: No plain `simp' found in buffer")
      (message "lean4: Replaced %d `simp' with `simp?'; waiting for suggestions..."
               (length lines))
      (lean4--wait-and-apply-all-try-this buf lines 30))))

(provide 'lean4-tactic)
;;; lean4-tactic.el ends here
