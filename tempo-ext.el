;;; tempo-ext.el --- extensions to tempo
;; 
;; Copyright 2009-2012 Florian Kaufmann <sensorflo@gmail.com>
;;
;; Author: Florian Kaufmann <sensorflo@gmail.com>
;; URL: https://github.com/sensorflo/tempo-ext.git
;; Created: 2009
;; 
;; This file is not part of GNU Emacs.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;; Commentary: 
;;
;; Extension to the tempo library. Is in early stages; it's currently just a
;; playground for myself.
;;
;;; Code: 
(require 'tempo)
(require 'doremi)
(require 'doremi-mac)

(defvar tempo-ext-undo-state nil)

;;- first remove any apperance for preceding for
;; - after first try cycle through different versions. Restore beginning point by doing some kind
;;   of 'save-point'. Maybe undohandling supports going back to a specific point.
;;- (newline-unless-blank)
(defun tempo-entry(&optional rubout-regexp)
  (interactive)
  (when rubout-regexp
    (let ((saved-point (point))) 
      (when (re-search-backward (concat rubout-regexp "\\=") (line-beginning-position) t)
        (delete-region (point) saved-point)))))

(defun my-tempo-handler (element)
  ;; on-region:
  ;; Due to Emacs Lisp's 'indefinite scope', the locale variable
  ;; on-region bound in tempo-insert-template can be used here.
  (cond

   ((eq element '>n) (indent-according-to-mode) "\n")

   ;; region or blank-line
   ((eq element 'r-or-blank-line>)
    (if on-region
	'r>
      (indent-according-to-mode)	; p
      (tempo-insert-mark (point-marker)) ; >
      'n))

   ;; line wise start
   ((memq element '(lws line-wise-start))
    (cond
     (on-region
      (goto-char tempo-region-start)
      (beginning-of-line)
      (while (and (looking-at "\\s-*$") (< (point) tempo-region-stop))
	(forward-line))
      (set-mark tempo-region-stop)
      (mark-whole-lines)
      'save-region)
     ((save-excursion (beginning-of-line) (not (looking-at "\\s-*$")))
      (end-of-line)
      (setq tempo-insertion-start (point))
      'n>)
     (t "")))

   ;; save region
   ((memq element '(sr save-region))
    (when on-region
      (set-marker tempo-region-start (min (mark) (point)))
      (set-marker tempo-region-stop (max (mark) (point)))
      (goto-char tempo-region-start))
    "")))

(add-to-list 'tempo-user-elements 'my-tempo-handler)

(defmacro tempo-ext-define-group (cmd-name enum &optional doc-string)
  (let* ((expaned-enum (mapcar (lambda (x) (intern (concat "tempo-template-" cmd-name x))) enum))
	 (foo `(quote ,expaned-enum)))
    `(defun ,(intern (concat "tempo-template-doremi-" cmd-name)) ()
       ,(or doc-string (concat "Cycle among inserting" cmd-name))
       (interactive)
       (setq tempo-ext-undo-state (undo-get-state))
       (tempo-ext-doremi-setter-fn (car ,foo))
       (doremi (quote tempo-ext-doremi-setter-fn) (car ,foo) nil nil ,foo t)
       (setq tempo-ext-undo-state nil))))

(defun tempo-ext-doremi-setter-fn (tempo-fn)
  (tempo-ext-undo-revert-to-state tempo-ext-undo-state)
  (funcall tempo-fn)
  tempo-fn)

(defun tempo-ext-undo-get-state ()
  "Return a handler for the current state to which we might want to undo.
The returned handler can then be passed to `tempo-ext-undo-revert-to-state'."
  (unless (eq buffer-undo-list t)
    buffer-undo-list))

(defun tempo-ext-undo-revert-to-state (state)
  "Revert to the state STATE earlier grabbed with `tempo-ext-undo-get-state'.
This undoing is not itself undoable (aka redoable)."
  (unless (eq buffer-undo-list t)
    (let ((new-undo-list (cons (car state) (cdr state))))
      ;; Truncate the undo log at `state'.
      (when state
        (setcar state nil) (setcdr state nil))
      (unless (eq last-command 'undo) (undo-start))
      ;; Make sure there's no confusion.
      (when (and state (not (eq state (last pending-undo-list))))
        (error "Undoing to some unrelated state"))
      ;; Undo it all.
      (while (not (memq pending-undo-list '(nil t))) (undo-more 1))
      ;; Reset the modified cons cell to its original content.
      (when state
        (setcar state (car new-undo-list))
        (setcdr state (cdr new-undo-list)))
      ;; Revert the undo info to what it was when we grabbed the state.
      (setq buffer-undo-list state))))


(provide 'tempo-ext)

;;; tempo-ext.el ends here
