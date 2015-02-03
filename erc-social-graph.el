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
