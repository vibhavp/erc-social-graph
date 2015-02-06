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

(defcustom sgraph-update-function 'sgraph-update-graph
  "The function used for updating the graph hash table. Takes the message
 sender as an argument."
  :type 'function
  :group 'erc-social-graph)

(defvar sgraph-table (make-hash-table :test 'equal))

(defun sgraph-update-graph (sender text)
  "Update the graph"
  (maphash (lambda (nick value)
	     (let ((result (ignore-errors (string-match
					   (format "\\<%s\\>\\([^[:alpha:]]\\|$\\)"
						   (downcase nick))
					   (downcase text)))))
	       (when result
		 (let* ((key (concat (downcase sender) "-" (downcase nick)))
			(times (gethash key (gethash (buffer-name) sgraph-table) nil)))
		   (if (eq times nil)
		       (puthash key 1 (gethash (buffer-name) sgraph-table))
		     (puthash key (+ times 1) (gethash (buffer-name) sgraph-table)))))))
	   erc-channel-users)) 

(defun sgraph-update ()
  "Parse the text in the current buffer, and pass it to sgraph-update-function"
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))    
    (when (string=  (substring text 0 1) "<")
      (let ((sender (substring text (+ 1 (string-match "<" text))
				(string-match ">" text))))
	(setq text (substring text (+ 2 (string-match ">" text))))
	(funcall sgraph-update-function sender text)))))  

(defun sgraph-create ()
  "Create empty graph for current-buffer"
  (puthash (buffer-name) (make-hash-table :test 'equal) sgraph-table))

(defun sgraph-clear ()
  "Clear graph for current region"
  )

(defun sgraph-draw (channel)
  "Draw a graph for the given channel"
  (interactive (list (completing-read "Draw graph for channel: "
				      (mapcar 'buffer-name (erc-buffer-list)))))
  (let ((buffer (format "sgraph-%s.dot" channel)))
    (get-buffer-create buffer)
    (set-buffer buffer)
    (insert "digraph {\n")
    (let ((channel-graph (gethash channel sgraph-table)))
      (maphash (lambda (link value)
		 (insert (format "\"%s\" -> \"%s\" [penwidth = %d]; \n"
				 (substring link 0 (string-match "-" link))
				 (substring link (+ 1 (string-match "-" link)))
				 (if (> value 4)
				     6
				   value))))
	       channel-graph))
    (insert "}")
    (switch-to-buffer buffer)))

(define-erc-module social-graph sgraph
  "Social network gaphs similar to piespy"
  ;; Enable
  ((add-hook 'erc-join-hook 'sgraph-create)
   (add-hook 'erc-insert-post-hook 'sgraph-update))
  ;; Disable
  ((remove-hook 'erc-insert-post-hook 'sgraph-update)
   (remove-hook 'erc-join-hook 'sgraph-create)))

(provide 'erc-social-graph)
