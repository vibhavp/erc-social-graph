;;; erc-social-graph.el --- A social network graph module for ERC.

;;; Copyright (C) 2014 Vibhav Pant <vibhavp@gmail.com>

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'erc)

(defun sgraph-update-hash ()
  "Update graph"
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (when (string=  (substring text 0 1) "<")
      (let ((sender (substring text (+ 1 (string-match "<" text))
				(string-match ">" text))))
	(setq text (substring text (+ 2 (string-match ">" text))))
	(maphash (lambda (nick value)
		   (let ((result (ignore-errors (string-match nick text))))
		     (when result
		       (let* ((key (concat sender "-" nick))
			      (times (gethash key graph nil)))
			 (unless (string= sender nick)
			   (if (not times)
			       (puthash key 1 graph)
			     (puthash key (+ times 1) graph)))))))
		 erc-channel-users)))))    

(defun sgraph-create ()
  "Greate graph"
  (setq-default graph (make-hash-table :test 'equal))
  (make-variable-buffer-local 'graph))

;;;###autoload(autoload 'erc-social-graph-mode "erc-social-graph" nil t)
(define-erc-module social-graph nil
  "Social network gaphs similar to piespy"
  ;; Enable
  ((add-hook 'erc-join-hook 'sgraph-create)
   (add-hook 'erc-insert-post-hook 'sgraph-update-hash))
  ;; Disable
  ((makunbound graph)
   (remove-hook 'erc-insert-post-hook 'sgraph-update-hash)
   (remove-hook 'erc-join-hook 'sgraph-create)))
