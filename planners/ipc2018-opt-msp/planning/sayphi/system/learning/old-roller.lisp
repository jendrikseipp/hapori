;; ROLLER on SAYPHI Planner 
;; Replaying Tilde Decision Tree of Helpful Context.
;; Td. 11.02.2008
;; _________________________________________________________________________________________


(defvar *roller-tree* nil) ;;This will point to the firt bnode
(defvar *trace-roller* nil) ;;Tracing the decission tree selections


(defun roller-meta-predicate (meta-predicate predicate-type)
  (cond ((eq predicate-type 'target-goal)
	 (cons (intern (format nil "TARGET_GOAL_~a" (car meta-predicate))) (cdr meta-predicate)))
	((eq predicate-type 'candidate)
	 (cons (intern (format nil "CANDIDATE_~a" (car meta-predicate)))
		 (cons nil (cdr meta-predicate))))))
	 

(defun roller-node-context (node)
  (let ((node-context nil))
    (dolist (i-child (snode-children node))
      (push (roller-meta-predicate (snode-plan-action i-child) 'candidate) node-context))
    (dolist (i-goal (pp-state (target-goals node) 'list) node-context)
      (push (roller-meta-predicate i-goal 'target-goal) node-context))))


(defun load-roller-tree ()
  (setf *roller-tree* (btree-load-from-tilde-file "system/learning/siblings-episodes.out"))
  (setf (btree-pointer *roller-tree*) 
	(btree-first-node *roller-tree*))
  )


;; It returns the action name
(defun ask-roller-for-action (node)
  (setf (btree-pointer *roller-tree*) (btree-first-node *roller-tree*))
  (setf (btree-current-context *roller-tree*) (roller-node-context node))
  (car (solve-btree-context 0)))


;; This returns in a list the action an the numbers of examples that match the leaf
(defun solve-btree-context (tree-depth)
  (let ((roller-points (btree-pointer *roller-tree*)))
    (cond ((rtest-p (bnode-element roller-points))
	   (if (roller-match)
	       (progn
		 (when *trace-roller* (format t "~%~a[R]> ~a ? YES" (say-indent tree-depth) (bnode-element roller-points)))
		 (setf (btree-pointer *roller-tree*) (bnode-yes-node roller-points)))
	       (progn
		 (when *trace-roller* (format t "~%~a[R]> ~a ? NO" (say-indent tree-depth) (bnode-element roller-points)))
		 (setf (btree-pointer *roller-tree*) (bnode-no-node roller-points))))
	   (solve-btree-context (1+ tree-depth)))
	  ((rleaf-p (bnode-element roller-points))
	   (when *trace-roller* (format t "~%~a[R]> ~a" (say-indent tree-depth) (bnode-element roller-points)))
	   (car (rleaf-decissions (bnode-element roller-points))))
	   )))


(defun roller-match ()
  (let ((roller-points (btree-pointer *roller-tree*)))
    (when (find (rtest-test (bnode-element roller-points)) 
		(btree-current-context *roller-tree*) :key #'car)
      t
;;       hacer algo con los argumentos
    )))

	  

	  
;; This will decide which is the next node base on the advise
;; and some sorting criteria   
(defun roller-next-node (action-name node visited &key (select-option :default)
			                               (heuristic-fn nil))
  (let ((candidates (remove-if #'(lambda (i-child)
			     (or (not (eq action-name (car (gaction-planaction (snode-applied-action i-child)))))
				 (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state)))
			 (snode-children node))))
    (cond ((eq select-option :default)
	   (car candidates))
	  ((eq select-option :best-candidates)
	   (dolist (i-candidate candidates)
	     (store-h-extras i-candidate heuristic-fn))
	   (setf candidates (stable-sort candidates #'< :key #'snode-h-value))
	   (car candidates))
	  (t (car candidates))
	  )
    
	 ))
			     


(defun roller-climbing (init-node h-function cost-fn search-options &optional  
		      (problem *current-problem*))
  (declare (ignore cost-fn))
  (let ((runtype (search-option-value :roller-run search-options))
	(helpful (search-option-value :helpful search-options))
	(visited (make-hash-table))
	(node init-node)
	(next-node nil) (action-name nil))
    (load-roller-tree)
    (store-h-extras node h-function)
    
    (do* ((stop-this (stop-search node problem) (stop-search node problem)))
	 (stop-this (when-stop (cdr stop-this) node problem))
      (expand-state node :helpful helpful)
      (push node (gethash (snode-hash-code node) visited))
      
      (setf action-name (ask-roller-for-action node))
      (setf next-node nil)
      
      (unless (null action-name)
	(setf next-node (roller-next-node action-name node visited :select-option runtype
					                           :heuristic-fn h-function))
	(when next-node (store-h-extras next-node h-function))
	(when (> *say-output* 1) (format t "{~% ROLLER:>> Selecting ~a}" action-name)))
      
      (unless next-node
	(dolist (i-child (snode-children node))
	  (cond ((not (find i-child (gethash (snode-hash-code i-child) visited) :test #'equal-state))
		 (store-h-extras i-child h-function))
		(t 
		 (setf (snode-closed i-child) t))))
	
	(setf next-node (hc-next-node node)))
	    
      (cond ((snode-p next-node)
	     (setf node next-node))
	    (t
	     (restore-nonhelpful node h-function)
	     (when (> *say-output* 1) (format t "{~% Restoring NON-Helpful Actions!!}"))
	     (setf node (hc-next-node node))))
      (when (snode-p node)
	(print-search-node node nil))
      ;; 	 (break)
      )))
