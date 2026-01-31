;;; lean4-util.el --- Utilities for lean4-mode -*- lexical-binding: t -*-

;; Copyright (c) 2014 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Author: Soonho Kong
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

;; This library provides utilities for `lean4-mode'.

;;; Code:

(require 'cl-lib)
(require 'lean4-settings)

(defun lean4-line-offset (&optional pos)
  "Return the byte-offset of POS or current position.
Counts from the beginning of the line."
  (interactive)
  (let* ((pos (or pos (point)))
         (bol-pos
          (save-excursion
            (goto-char pos)
            (beginning-of-line)
            (point))))
    (- pos bol-pos)))

(defun lean4-pos-at-line-col (l c)
  "Return the point of the given line L and column C."
  ;; http://emacs.stackexchange.com/a/8083
  (save-excursion
    (goto-char (point-min))
    (forward-line (- l 1))
    (move-to-column c)
    (point)))

(defun lean4-whitespace-cleanup ()
  "Delete trailing whitespace if `lean4-delete-trailing-whitespace' is t."
  (when lean4-delete-trailing-whitespace
    (delete-trailing-whitespace)))

(defun lean4-in-comment-p ()
  "Return t if a current point is inside of comment block.  Return nil otherwise."
  (nth 4 (syntax-ppss)))

;; The following function is a slightly modified version of
;; f--collect-entries written by Johan Andersson
;; The URL is at https://github.com/rejeep/f.el/blob/master/f.el#L416-L435
(defun lean4--collect-entries (path recursive)
  "Find all files in PATH.  If RECURSIVE, then descend into subfolders.
This is a modified version of `f--collect-entries' that waits for 0.0001s before
descending into subfolders.  This allows `wait-timeout' function to check the
timer and kill the execution of this function."
  (let (result
        (entries
         (-reject
          (lambda (file)
            (or
             (equal (f-filename file) ".")
             (equal (f-filename file) "..")))
          (directory-files path t))))
    ;; The following line is the only modification that I made
    ;; It waits 0.0001 second for an event. This wait allows
    ;; wait-timeout function to check the timer and kill the execution
    ;; of this function.
    (sit-for 0.0001)
    (cond (recursive
           (mapc
            (lambda (entry)
              (if (f-file? entry)
                  (setq result (cons entry result))
                (when (f-directory? entry)
                  (setq result (cons entry result))
                  (setq result (append result (lean4--collect-entries entry recursive))))))
            entries))
          (t (setq result entries)))
    result))

(defmacro lean4-with-uri-buffers (server uri &rest body)
  (declare (indent 2)
           (debug (form form &rest form)))
  (let ((path-var (make-symbol "path")))
    `(let ((,path-var (eglot-uri-to-path ,uri)))
       (dolist (buf (eglot--managed-buffers ,server))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (when (and buffer-file-name
                        (or (ignore-errors (file-equal-p buffer-file-name ,path-var))
                            (string= (expand-file-name buffer-file-name)
                                     (expand-file-name ,path-var))))
               ,@body)))))))

(provide 'lean4-util)
;;; lean4-util.el ends here
