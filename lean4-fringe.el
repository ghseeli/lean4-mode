;;; lean4-fringe.el --- Show Lean processing progress in the editor fringe -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Microsoft Corporation. All rights reserved.
;; Released under Apache 2.0 license as described in the file LICENSE.
;;
;; Authors: Gabriel Ebner, Sebastian Ullrich
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
;;
;; Show Lean processing progress in the editor fringe
;;
;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lean4-settings)
(require 'lean4-util)

(defface lean4-fringe-face
  nil
  "Face to highlight Lean file progress."
  :group 'lean4)

(if (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'lean4-fringe-fringe-bitmap
      (vector) 16 8))

(if (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'lean4-fringe-goals-accomplished-bitmap
      (vector #x00 #x00 #x00 #x01
              #x02 #x04 #x28 #x10
              #xA0 #x40 #x00 #x00
              #x00 #x00 #x00 #x00)
      16 8))

(defface lean4-fringe-fringe-processing-face
  '((((class color) (background light))
     :background "chocolate1")
    (((class color) (background dark))
     :background "navajo white")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file processing progress."
  :group 'lean)

(defface lean4-fringe-fringe-fatal-error-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t :inverse-video t))
  "Face to highlight the fringe of Lean file fatal errors."
  :group 'lean)

(defface lean4-fringe-fringe-goals-accomplished-face
  '((((class color) (background light))
     :foreground "#3063b5")
    (((class color) (background dark))
     :foreground "#3794ff")
    (t :foreground "blue"))
  "Face to highlight the fringe checkmark for sorry-free declarations."
  :group 'lean)

(defun lean4-fringe-fringe-face (lean-file-progress-processing-info)
  (let ((kind (cl-getf lean-file-progress-processing-info :kind)))
    (cond
     ((eq kind 1) 'lean4-fringe-fringe-processing-face)
     (t 'lean4-fringe-fringe-fatal-error-face))))

(defvar-local lean4-fringe-data nil)

(defun lean4-fringe-update-progress-overlays ()
  "Update processing bars in the current buffer."
  (dolist (ov (flatten-tree (overlay-lists)))
    (when (eq (overlay-get ov 'face) 'lean4-fringe-face)
      (delete-overlay ov)))
  (when lean4-show-file-progress
    (seq-doseq (item lean4-fringe-data)
      (let* ((reg (eglot-range-region (cl-getf item :range)))
             (ov (make-overlay (car reg) (cdr reg))))
        (overlay-put ov 'face 'lean4-fringe-face)
        (overlay-put ov 'line-prefix
                     (propertize " " 'display
                                 `(left-fringe lean4-fringe-fringe-bitmap ,(lean4-fringe-fringe-face item))))
        (overlay-put ov 'help-echo (format "processing..."))))))

(defvar-local lean4-fringe-delay-timer nil)

(defun lean4-fringe-update (server processing uri)
  (lean4-with-uri-buffers server uri
    (setq lean4-fringe-data processing)
    (unless (and lean4-fringe-delay-timer
                 (memq lean4-fringe-delay-timer timer-list))
      (setq lean4-fringe-delay-timer
            (run-at-time 0.3 nil
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (lean4-fringe-update-progress-overlays)
                               (setq lean4-fringe-delay-timer nil))))
                         (current-buffer))))))

(defvar-local lean4-fringe-goals-accomplished-data nil)

(defconst lean4-fringe-lean-tag-goals-accomplished 2
  "Value of the GoalsAccomplished tag in Lean diagnostics leanTags.")

(defun lean4-fringe-update-goals-accomplished-overlays ()
  "Update goals accomplished checkmarks in the current buffer."
  (dolist (ov (flatten-tree (overlay-lists)))
    (when (eq (overlay-get ov 'lean4-type) 'goals-accomplished)
      (delete-overlay ov)))
  (when lean4-show-goals-accomplished
    (seq-doseq (diag lean4-fringe-goals-accomplished-data)
      (let* ((range (cl-getf diag :range))
             (start (cl-getf range :start))
             (line (cl-getf start :line)))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line line)
            (let ((ov (make-overlay (point) (min (1+ (point)) (point-max)))))
              (overlay-put ov 'lean4-type 'goals-accomplished)
              (overlay-put ov 'before-string
                           (propertize " " 'display
                                       '(left-fringe lean4-fringe-goals-accomplished-bitmap
                                                     lean4-fringe-fringe-goals-accomplished-face)))
              (overlay-put ov 'help-echo "goals accomplished ✓"))))))))

(defvar-local lean4-fringe-goals-accomplished-delay-timer nil)

(defun lean4-fringe-update-goals-accomplished (server diagnostics uri)
  "Filter and store goals-accomplished DIAGNOSTICS, then schedule overlay update.
SERVER is the eglot server, URI is the file URI.
The overlay update is debounced to avoid excessive redisplay."
  (lean4-with-uri-buffers server uri
    (setq lean4-fringe-goals-accomplished-data
          (seq-filter
           (lambda (diag)
             (let ((tags (cl-getf diag :leanTags)))
               (and tags (seq-contains-p tags lean4-fringe-lean-tag-goals-accomplished))))
           diagnostics))
    (unless (and lean4-fringe-goals-accomplished-delay-timer
                 (memq lean4-fringe-goals-accomplished-delay-timer timer-list))
      (setq lean4-fringe-goals-accomplished-delay-timer
            (run-at-time 0.3 nil
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (lean4-fringe-update-goals-accomplished-overlays)
                               (setq lean4-fringe-goals-accomplished-delay-timer nil))))
                         (current-buffer))))))

(provide 'lean4-fringe)
;;; lean4-fringe.el ends here
