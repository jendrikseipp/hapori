(in-package "COMMON-LISP-USER")

;; (load-ma-sayphi)
;; (try-all-agentifications "satellite" "pfile*.pddl" :domain-file "satellite.pddl")
(defun try-all-agentifications (domain problems-regexp
				&key (domain-file "domain.pddl")
				  (domain-dir (concatenate 'string *domains-dir* domain "/"))
				  (probsets-dir (concatenate 'string domain-dir "probsets/"))
				  (timeout 100) (goal-selections '(subsets)))
  (let* ((domain-def (cdr (read-all-file (concatenate 'string domain-dir domain-file))))
	 (types-def (process-types (find-argument domain-def :types)))
	 (analyzed-types nil))
    (dolist (type-def types-def)
      (unless (member (car type-def) analyzed-types)
	(push (car type-def) analyzed-types)
	(if (not (eq (car type-def) 'object))
	    ;; when it is a intermediate type, I call with all its subtypes
	    (try-agentification (cons (car type-def) (sub-types type-def types-def))
				domain domain-file problems-regexp domain-dir probsets-dir timeout goal-selections)))
      (dolist (subtype (cdr type-def))
	(unless (member subtype analyzed-types)
	  (push subtype analyzed-types)
	  ;; when it is a intermediate type, I call with all its subtypes
	  (if (assoc subtype types-def)
	      (try-agentification (cons subtype (sub-types (list subtype) types-def))
				  domain domain-file problems-regexp domain-dir probsets-dir timeout goal-selections)
	      (try-agentification (list subtype) domain domain-file problems-regexp domain-dir probsets-dir timeout goal-selections)))))))

(defun try-agentification (agent-types domain domain-file problems-regexp domain-dir probsets-dir timeout goal-selections)
  (format t "~2%Trying agentification with type ~a" agent-types)
  ;;   (setf *merged-domain* (format nil "~a-merged-domain.pddl" (car agent-types)))
  (run-ma-experiments domain problems-regexp agent-types nil nil
		      :domain-file domain-file :domain-dir domain-dir :probsets-dir probsets-dir
		      :goal-selections goal-selections :output-file-prefix (format nil "~a-" agent-types)
		      :sort-agents '(mingoals) :parallel-plan-p t :use-macros-p t :generate-subproblems-only-p t
		      :ma-pddl-p t :solve-for-agentification-p t
		      :run-original-centralized-p nil :run-mapr-p nil :run-cmap-p nil :solve-for-merging-p nil
		      :algorithms '(lama-unit-cost) :replanning-algorithms '(lama-unit-cost) :timeout timeout))
