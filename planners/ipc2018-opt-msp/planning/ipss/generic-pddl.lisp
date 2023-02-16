(in-package "COMMON-LISP-USER")
#+(or allegro sbcl clisp) (asdf:oos 'asdf:load-op "cl-ppcre")
;;  (require 'cl-ppcre)

;;; **************************************************************************************
;;;              Writing PDDL problem and domain files
;;; **************************************************************************************

(defun write-domain (domain-def domain-dir domain-file)
  (write-domain-pddl-file (cadar domain-def)
			  (find-argument domain-def :requirements)
			  (find-argument domain-def :types)
			  (find-argument domain-def :predicates)
			  (pddl-domain-functions domain-def)
			  (give-me-all-actions domain-def)
			  (concatenate 'string domain-dir domain-file)
			  (find-argument domain-def :constants)))

(defun write-problem (problem-def probsets-dir problem-file)
  (write-pddl-file (cadar problem-def)
		   (car (find-argument problem-def :domain))
		   (find-argument problem-def :objects)
		   (find-argument problem-def :init)
		   (give-me-all-goals problem-def)
		   (concatenate 'string probsets-dir problem-file)
		   (find-argument problem-def :metric)))

(defun copy-problem (problem-file new-problem-file probsets-dir)
  (let ((command (concatenate 'string "cd ; cd " probsets-dir "; cp -p " problem-file " " new-problem-file)))
    (execute-shell-command command)))
;;   (write-problem (cdr (read-all-file (concatenate 'string probsets-dir problem-file)))
;; 		 probsets-dir
;; 		 new-problem-file))

(defun write-domain-pddl-file (name requirements types predicates functions actions file &optional constants)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "(define (domain ~(~a~))" name)
    (if requirements (format stream "~%  ~(~s~)" `(:requirements ,@requirements)))
    (if types (format stream "~%  ~(~s~)" `(:types ,@types)))
    (if constants (format stream "~%  ~(~s~)" `(:constants ,@constants)))
    (if predicates (format stream "~%  ~(~s~)" `(:predicates ,@predicates)))
    (if functions (format stream "~%  ~(~s~)" `(:functions ,@functions)))
    (dolist (action actions)
      (if (nth 3 action)
	  (format stream "~2%  ~(~s~)" action)
	  (format stream "~2%  (:action ~(~s~)~%   :parameters ()~%   :precondition ~(~s~)~%    :effect ~(~s~))" (nth 1 action) (nth 5 action) (nth 7 action))))
    ;; because I do not want parameters to be saved as nil
    ;;    (pp-list actions 2 stream t nil)
    (format stream "~%)"))
  file)

(defun write-pddl-file (name domain-name objects state goals file &optional metric constraints comment)
  (with-open-file (stream file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (if comment (format stream "~a~2%" comment))
    (format stream "(define (problem ~(~a~)) (:domain ~(~a~))" name domain-name)
    (format stream "~%  (:objects")
    (do ((instances objects (cdr instances)))
	((null instances))
      (cond ((eq (car instances) '-)
	     (format stream " - ~(~a~)~%~12T" (cadr instances))
	     (setq instances (cdr instances)))
	    (t (format stream " ~(~a~)" (car instances)))))
    (format stream ")")
    (format stream "~%  (:init")
    (pp-list state 7 stream t)
    (format stream ")")
    (format stream "~%  (:goal (and ")
    (pp-list (cond ((eq (car goals) 'and)
		    (cdr goals))
		   ((listp (car goals)) goals)
		   (t (list goals)))
	     11 stream t)
    (format stream "))")
    (if metric
	(format stream "~%  ~s" (cons :metric metric)))
    (if constraints
	(format stream "~%  ~s" (list :constraints constraints)))
    (format stream ")")))

;;; **************************************************************************************
;;;              Getting information from PDDL definitions
;;; **************************************************************************************


;; property can be either: problem, :domain, :objects, :init or :goal
(defun pddl-problem-property (property pddl-problem-description)
  (funcall (if (member property '(problem :domain :goal)) #'cadr #'cdr)
	   (assoc property (cdr pddl-problem-description))))

;; More or less the same as before. I will merge them.
(defun find-argument (domain argument)
  (cdr (assoc argument domain)))

(defun find-all-argument (domain argument)
  (remove-if-not #'(lambda (definition)
		     (eq (car definition) argument))
		 domain))

(defun give-me-all-goals (problem-def)
  (let ((goals-in-problem (find-argument problem-def :goal)))
    (if (eq (caar goals-in-problem) 'and)
	(cdar goals-in-problem)
	goals-in-problem)))

(defun give-me-all-actions (domain-def)
  (append (find-all-argument domain-def :action)
	  (find-all-argument domain-def :durative-action)))

(defun find-action-def (action-name actions only-standard-actions-p)
  (if only-standard-actions-p
      (find action-name actions :key #'pddl-action-name)
      (some #'(lambda (name)
		(find name actions :key #'pddl-action-name))
	    `(,action-name ,(label-action action-name 'not-achieves) ,(label-action action-name 'not-deletes)))))

(defun label-action (action-name label)
  (intern (format nil "~:@(~a-~a~)" label action-name)))

(defun pddl-domain-predicates (domain-def)
  (find-argument domain-def :predicates))

(defun pddl-predicate-names (domain-def)
  (mapcar #'car (find-argument domain-def :predicates)))

;; I clean functions definition of type information
(defun pddl-domain-functions (domain-def)
  (let ((functions (find-argument domain-def :functions)))
    (if (member '- functions)
	(do ((fs functions (cdr fs))
	     (new-fs nil))
	    ((null fs) (reverse new-fs))
	  (cond ((eq '- (cadr fs))
		 (push (car fs) new-fs)
		 (setq fs (cddr fs)))
		(t (push (car fs) new-fs))))
	functions)))

(defun pddl-function-names (domain-def)
  (mapcar #'car (pddl-domain-functions domain-def)))

;;; **************************************************************************************
;;;              Processing instances, parameters, types
;;; **************************************************************************************

;; returns a list of sublists of the form ((type1 instance1 ... instanceN) ... (typeM instance1 ... instanceR))
(defun process-instances (dirty-instances)
  (do ((instances (cleanup-params dirty-instances))
       (current-instances nil)
       (final-instances nil)
       (instance nil)
       (type nil)
       (x 0 (1+ x)))
      ((> x (length instances)) final-instances)
    (setq instance (nth x instances))
    (cond ((or (eq instance '-) (null instance))
	   (when (or instance current-instances)
	     (setq type (if (null instance)
			    'object
			    (nth (incf x) instances)))
	     (if (assoc type final-instances)
		 (dolist (current-instance current-instances)
		   (push current-instance (cdr (assoc type final-instances))))
		 (push (cons type (reverse current-instances))
		       final-instances))
	     (setq current-instances nil)))
	  (t (push instance current-instances)))))

(defun cleanup-params (parameters)
  (do* ((params parameters (cdddr params))
	(new-params nil))
       ((null params) (nreverse new-params))
    (cond ((eq (cadr params) '-)
	   (push (car params) new-params)
	   (push (cadr params) new-params)
	   (push (caddr params) new-params))
	  (t (do* ((rest-params params (cdr rest-params))
		   (vars nil))
		  ((or (null rest-params) (eq (cadr rest-params) '-))
		   (cond ((null rest-params)
			  (dolist (var (reverse vars))
			    (push var new-params))
			  (setq params nil))
			 (t (dolist (var (reverse (cons (car rest-params) vars)))
			      (push var new-params)
			      (push '- new-params)
			      (push (caddr rest-params) new-params))
			    (setq params rest-params))))
	       (push (car rest-params) vars))))))

(defun flatten-instances (instances-list)
  (let ((instances nil))
    (dolist (type-instances instances-list)
      (if (cdr type-instances)
	  (setq instances (append instances (cdr type-instances) (list '- (car type-instances))))))
    instances))

;; I assume both sets have been processed with process-instances, and they could later by handled by flatten-instances
(defun merge-instances (instances old-instances)
  (dolist (instance-def instances old-instances)
    (if (assoc (car instance-def) old-instances)
	(setf (cdr (assoc (car instance-def) old-instances))
	      (union (cdr (assoc (car instance-def) old-instances))
		     (cdr instance-def) :test #'eq))
	(push instance-def old-instances))))

;; It takes as input the types directly from the domain file and returns a list of sublists
;; ((object supertype1 ...) (supertype1 subtype1 ...) ...)
(defun process-types (types)
  (let ((type-hierarchy (process-instances types))
	(object-def nil))
    (cond ((not type-hierarchy)
	   (setq type-hierarchy (list (list 'object))))
	  ((not (eq (caar type-hierarchy) 'object))
	   (setq object-def (assoc 'object type-hierarchy))
	   (setq type-hierarchy (cons object-def (remove object-def type-hierarchy :test #'equal))))
	  (t nil))
    (dolist (type-def (cdr type-hierarchy))
      (if (not (direct-super-type (car type-def) type-hierarchy))
	  (setf (cdar type-hierarchy)
		(cons (car type-def) (cdar type-hierarchy)))))
    type-hierarchy))

;; types-def should be the output of process-types
(defun direct-super-type (type type-defs)
  (caar (member type type-defs :test #'(lambda (the-type type-def) (member the-type (cdr type-def))))))

;; types-def should be the output of process-types
(defun super-types (type types-def)
  (if (eq type 'object)
	(list 'object)
	(let ((direct (direct-super-type type types-def)))
	  (if (eq direct 'object)
	      (list direct)
	      (cons direct (super-types direct types-def))))))

;; same as above but receives a list of types as input, instead of only one type
;; types-def should be the output of process-types
(defun super-types-of-set (types types-def)
  (if types
      (union types (reduce #'union (mapcar #'(lambda (atype) (super-types atype types-def)) types)))))

;; Computes the set of types that are subtypes of types in types-def
;; Assumes types-def has been computed with process-types
(defun sub-types (types types-def)
  (do* ((the-types types-def (if changep types-def (cdr the-types)))
	(changep nil nil)
	(new-sub-types types))
       ((null the-types) (set-difference new-sub-types types))
    (when (and (member (caar the-types) new-sub-types)
	       (not (subsetp (cdar the-types) new-sub-types)))
      (setq new-sub-types (union (cdar the-types) new-sub-types))
      (setq changep t))))

;; Computes the leaf types of a hierarchy of types
(defun leaf-types (types-def)
  (set-difference (reduce #'append types-def :key #'cdr) (mapcar #'car types-def)))

;; Given two types and a set of types, it returns the closest super-type of both
(defun find-common-super-type (type1 type2 types)
  (do* ((super-types1 (cons type1 (super-types type1 types)))
	(super-types2 (cons type2 (super-types type2 types)))
	(stypes1 super-types1 (cdr stypes1))
	(stypes2 super-types2 (cdr stypes2))
	(common-supertype nil))
       ((or common-supertype (null stypes1) (null stypes2))
	common-supertype)
    (setq common-supertype (car (or (member (car stypes1) stypes2)
				    (member (car stypes2) stypes1))))))

;; Given two types, where I assume one is a subtype of the other and a set of types, it returns the most specific type
(defun find-common-subtype (type1 type2 types)
  (cond ((eq type1 type2) type1)
	((member type2 (super-types type1 types)) type1)
	((member type1 (super-types type2 types)) type2)
	(t nil)))

;; I assume object def is the first one (according to what process-types returns)
(defun flatten-types (types)
  (flatten-instances (append (cdr types) (list (car types)))))

(defun merge-types (types1 types2)
  (merge-instances types1 types2))

;;; **************************************************************************************
;;;              Managing solutions
;;; **************************************************************************************

;; It takes a solution in whatever format and returns a list of actions
;; solution can be either:
;;  - simple solution list: ((unstack a b) (putdown a))
;;  - parallel solution list (number represents time step): ((1 (unstack a b)) (2 (putdown a)))
;;  - solution struct
;;  - sayphi solution
(defun give-me-solution-list (solution)
  (cond ((listp solution) solution)
	((solution-p solution) (solution-path solution))
	(t (pp-solution-sayphi solution))))

;; It prints the solution
;; If ipc-p=T, it prints it in the IPC format. Otherwise as a simple list of actions
;; if print-info-p=T, it will print global info on the plan
;; domain and problem related args are needed for obtaining action costs
;; if unit-cost-p=T, it assumes unit cost, so there is no need to supply all these files
;; if unit-cost-p=NIL, the code assumes that domain, domain-file and problem-file are given
;; It does not handle sayphi solutions that include lookahead nodes. Use say-pp-solution instead
;; solution can be either:
;;  - simple solution list: ((unstack a b) (putdown a))
;;  - parallel solution list (number represents time step): ((1 (unstack a b)) (2 (putdown a)))
;;      I inherit this representation from Susana's code
;;  - solution struct
;;  - sayphi solution
;; If plan-file=T, it will print the plan in the standard output
(defun pp-solution (solution &key (ipc-p nil) (unit-cost-p t) (print-info-p nil)
			       (domain "") (domain-file "domain.pddl")
			       (domain-dir (concatenate 'string *domains-dir* domain "/"))
			       (problem-file "")
			       (probsets-dir (concatenate 'string domain-dir "probsets/"))
			       (plan-file (concatenate 'string domain-dir "result/plan.sol")))
  (ensure-directories-exist (concatenate 'string domain-dir "result/"))
  (let* ((plan-list (give-me-solution-list solution))
	 (parallel-plan-p (numberp (caar plan-list)))
	 (sequential-plan (if parallel-plan-p (mapcar #'cadr plan-list) plan-list))
	 (plan (if unit-cost-p
		   (mapcar #'(lambda (action) (list action 1)) plan-list)
		   (give-me-actions-costs sequential-plan
					  :domain domain :domain-file domain-file :domain-dir domain-dir
					  :problem-file problem-file :probsets-dir probsets-dir)))
	 (plan-cost 0)
	 (*print-pretty* nil))
    (if parallel-plan-p
	(setq plan (mapcar #'(lambda (time-action action) `(,(car action) ,(cadr action) ,(car time-action)))
			   plan-list plan)))
    (if (eq plan-file t)
	(setq plan-cost (print-plan plan t ipc-p print-info-p parallel-plan-p))
	(with-open-file (stream plan-file :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (setq plan-cost (print-plan plan stream ipc-p print-info-p parallel-plan-p))))
    (if (solution-p solution)
	(setf (solution-total-cost solution) plan-cost))
    solution))

;; plan is a list that can take the following formats:
;;   - sequential plan: (((unstack a b) 1) ((putdown a) 1))
;;   - parallel plan: (((unstack a b) 1 1) ((putdown a) 1 2)) where
;;       first number is cost, second number is time step
(defun print-plan (plan stream ipc-p print-info-p parallel-plan-p)
  (let ((plan-length 0)
	(plan-cost 0)
	(makespan 0))
    (do* ((actions plan (cdr actions))
	  (action (car actions) (car actions))
	  (action-cost (cadr action) (cadr action))
	  (planaction-counter 0 (1+ planaction-counter)))
	 ((null actions))
      (incf plan-length)
      (if (numberp action-cost) (incf plan-cost action-cost))
      (setq makespan (if (and parallel-plan-p (numberp (caddr action))) (caddr action) plan-length))
      (if ipc-p (format stream "~d: " (if (and parallel-plan-p (numberp makespan)) makespan planaction-counter)))
      (format stream "~a" (car action))
      (if (and ipc-p (numberp action-cost))
	  (if (integerp action-cost)
	      (format stream " [~d]" action-cost)
	      (format stream " [~,2f]" action-cost)))
      (terpri stream))
    (if print-info-p
	(if (integerp plan-cost)
	    (format stream ";; Cost: ~d, Length: ~d, Makespan: ~d" plan-cost plan-length makespan)
	    (format stream ";; Cost: ~,2f, Length: ~d, Makespan: ~d" plan-cost plan-length makespan)))
    plan-cost))

;; Reimplementation from pale/planner-client.lisp
(defun get-solution-from-planner (planner &optional (from-file-p t) (solution-file "solution.txt"))
  (if from-file-p
      (get-solution-from-file planner solution-file)
      (case planner
	(ipss (car (pp-solution :format 'list :include-pp-solution-p t)))
	(sayphi (pp-solution-sayphi))
	(metric-ff (car (solutions-ff :ff-results-file solution-file)))
	(t nil))))

(defun get-solution-from-file (planner solution-file)
  (let ((solution nil)
	(*readtable* (copy-readtable nil)))
    (with-open-file (istream solution-file :direction :input)
      (set-macro-character #\: #'skip-it)
      (setq solution (read istream nil 'eof)))
    (if (or (not solution) (eq solution 'eof))
	nil
	(if (or (eq planner 'sayphi) (eq planner 'ipss) (listp solution))
	    (if (listp (car solution))
		solution
		(with-open-file (istream solution-file :direction :input)
		  (setq solution nil)
		  (do* ((action (read istream nil 'eof)
				(read istream nil 'eof)))
		       ((eq action 'eof) (reverse solution))
		    (push action solution))))
	    (pddl-solution-to-lisp solution-file)))))

(defun solution-from-file (solution-file)
  (let ((solution nil)
	(*readtable* (copy-readtable nil)))
    (with-open-file (istream solution-file :direction :input)
      (set-macro-character #\: #'skip-it)
      (setq solution (read istream)))
    (if (listp solution)
	solution
	(pddl-solution-to-lisp solution-file))))

(defun skip-it (stream char)
  char)

(defun pddl-solution-to-lisp (solution-file)
  (let ((matching-closure-actions (cl-ppcre:create-scanner "([0-9])+: ([-_ ()0-9a-zA-Z]+)" :case-insensitive-mode t))
	(*readtable* (copy-readtable nil))
	(solution nil))
    (set-macro-character #\: #'skip-it)
    (with-open-file (istream solution-file :direction :input)
      (do* ((line (read-line istream nil 'eof)
		  (read-line istream nil 'eof)))
	   ((eq line 'eof))
;; 	(format t "~%Line: ~a" line)
	(multiple-value-bind (result array)
	    (cl-ppcre:scan-to-strings matching-closure-actions line)
	  (when array
	    (do* ((action (aref array 1))
		  (action-parameter nil)
		  (start 0)
		  (action-list nil))
		 ((eq action-parameter 'eof)
		  (if (listp (car action-list))
		      (setq solution (append action-list solution))
		      (push (nreverse action-list) solution)))
	      (multiple-value-setq (action-parameter start)
		(read-from-string action nil 'eof :start start))
	      (if (and action-parameter (not (eq action-parameter 'eof)))
		  (push action-parameter action-list)))))))
    (nreverse solution)))

(defun give-me-files (dir files-regexp)
  (mapcar #'(lambda (pathname)
	      (if (pathname-type pathname)
		  (concatenate 'string (pathname-name pathname) "." (pathname-type pathname))
		  (pathname-name pathname)))
	  (directory (format nil "~a~a" dir files-regexp))))

;; It takes as input a solution, and a planning task (domain and problem files) and outputs a solution with
;; actions costs
(defun give-me-actions-costs (solution &key (domain "") (domain-file "domain.pddl")
					 (domain-dir (concatenate 'string *domains-dir* domain "/"))
					 (problem-file "")
					 (probsets-dir (concatenate 'string domain-dir "probsets/")))
  (let* ((domain-def (cdr (read-all-file (concatenate 'string domain-dir domain-file))))
	 (problem-def (cdr (read-all-file (concatenate 'string probsets-dir problem-file))))
	 (state (find-argument problem-def :init))
	 (metric (or (cadr (find-argument problem-def :metric)) '(total-cost)))
	 (actions-costs (give-me-cost-effects (give-me-all-actions domain-def)))
	 (new-plan nil))
    (mapcar #'(lambda (plan-step)
		(let* ((action (if (snode-p plan-step)
				   (snode-plan-action plan-step)
				   plan-step))
		       (action-name (car action))
		       (parameter-cost (cdr (assoc action-name actions-costs)))
		       (parameters (car parameter-cost))
		       (substitution (get-substitution (cdr action) parameters)))
		  (some #'(lambda (cost)
			    (if (equal metric (cadr cost))
				(list action
				      (case (car cost)
					;; I am not sure what to do in the case of assign
					((assign increase) (compute-cost (caddr cost) state))
					(decrease (- (compute-cost (caddr cost) state)))
					(t 1)))))
			(sublis substitution (cadr parameter-cost)))))
	    (if (solution-p solution) (solution-path solution) solution))))

;; It returns a list of sublists (action-name action-parameters cost-effects-list)
(defun give-me-cost-effects (actions)
  (mapcar #'(lambda (action)
	      (list (pddl-action-name action)
		    (cleanup-params (pddl-action-parameters action))
		    (or (remove-if-not #'(lambda (effect) (member (car effect) '(increase decrease assign)))
				       (pddl-action-effects action))
			'((increase (total-cost) 1)))))
	  actions))

;; It inputs the cost computation as defined in the domain with the corresponding substitution from the plan and a state
;; and returns the value of the cost. It works with complex cost formulae as:
;;     (+ (* (total-cost) (total-fuel pl1)) (road-length l1 l2))
(defun compute-cost (cost-def state)
  (if (numberp cost-def)
      cost-def
      (if (listp cost-def)
	  (or (some #'(lambda (literal)
			(if (and (eq (car literal) '=)
				 (equal (cadr literal) cost-def))
			    (caddr literal)))
		    state)
	      (funcall (car cost-def)
		       (compute-cost (cadr cost-def) state)
		       (compute-cost (caddr cost-def) state))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Solution validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A simple solution validation tool (for now, just strips plans)
(defun validate-sol (&key (solution *say-solution*) (domain nil) (domain-file nil)
		       (domain-dir (format nil "~a~a/" *domains-dir* domain))
		       (problem-file nil) (probsets-dir (format nil "~aprobsets/" domain-dir)) (sayphi-p nil))
  (when sayphi-p
    (if (and domain domain-file)
	(say-domain domain domain-file nil))
    (if problem-file
	(prob (concatenate 'string probsets-dir problem-file) nil)))
  (do* ((domain-def (cdr (read-all-file (concatenate 'string domain-dir domain-file))))
	(predicates (pddl-predicate-names domain-def))
	(problem-def (cdr (read-all-file (concatenate 'string probsets-dir problem-file))))
	(functions (pddl-function-names domain-def))
	(state (find-argument problem-def :init))
	(plan (if (solution-p solution) (solution-path solution) solution) (cdr plan))
	(plan-step (car plan) (car plan))
	(all-goals-true-p nil)
	(goals (give-me-all-goals problem-def))
	(invalid nil))
       ((or (null plan) invalid)
	(setq all-goals-true-p (every #'(lambda (goal) (member goal state :test #'equal)) goals))
	(if (not all-goals-true-p)
	    (format t "~2%Invalid plan; it doesn't achieve top level goals:~%  ~a"
		    (remove-if #'(lambda (goal) (member goal state :test #'equal)) goals)))
	(and (not invalid) all-goals-true-p))
    (let* ((action (if (snode-p plan-step)
		       (snode-plan-action plan-step)
		       plan-step))
	   (action-name (car action))
	   (action-def (find-action-def action-name (give-me-all-actions domain-def) t))
	   (parameters (cleanup-params (pddl-action-parameters action-def)))
	   (substitution (get-substitution (cdr action) parameters))
	   adds dels costs)
      (dolist (effect (pddl-action-effects action-def))
	(cond ((member (car effect) predicates) (push (sublis substitution effect) adds))
	      ((eq (car effect) 'not) (push (sublis substitution (cadr effect)) dels))
	      (t (push (sublis substitution effect) costs))))
      (cond ((pddl-preconditions-matching-p action-def state substitution predicates functions)
	     (if *trace-ma-sayphi* (format t "~2%Action: ~a"  action))
	     (setq state (update-state adds dels costs state)))
	    (t (report-matching-failure action-def state predicates functions substitution)
	       (setq invalid t))))))

;; Sayphi version. Outdated
;; (defun validate-sol (&key (solution *say-solution*) (domain nil) (domain-file nil) (problem-file nil) (probsets-dir nil))
;;   (if (and domain domain-file)
;;       (say-domain domain domain-file nil))
;;   (if problem-file
;;       (prob (concatenate 'string probsets-dir problem-file) nil))
;;   (do* ((state (problem-lit-init-state *current-problem*))
;; 	(plan (if (solution-p solution) (solution-path solution) solution) (cdr plan))
;; 	(plan-step (car plan) (car plan))
;; 	(negated-preds (dom-negative-preds *pspace*))
;; 	(special-preds (dom-special-preds *pspace*))
;; 	(all-goals-true-p nil)
;; 	(invalid nil))
;;        ((or (null plan) invalid)
;; 	(setq all-goals-true-p (every #'(lambda (goal) (member goal state :test #'equal))
;; 				      (problem-lit-goals *current-problem*)))
;; 	(if (not all-goals-true-p)
;; 	    (format t "~2%Invalid plan: it doesn't achieve top level goals ~a"
;; 		    (remove-if #'(lambda (goal) (member goal state :test #'equal))
;; 			       (problem-lit-goals *current-problem*))))
;; 	(and (not invalid) all-goals-true-p))
;;     (let* ((action (if (snode-p plan-step)
;; 		       (snode-plan-action plan-step)
;; 		       plan-step))
;; 	   (action-name (car action))
;; 	   (action-struct (find action-name (dom-actions *pspace*) :key #'action-name))
;; 	   (parameters (action-parameters action-struct))
;; 	   (substitution (mapcar #'cons parameters (cdr action))))
;;       (cond ((preconditions-met-p action-struct state substitution negated-preds special-preds)
;; 	     (if *trace-ma-sayphi*
;; 		 (format t "~2%Action: ~a"  action))
;; 	     (setq state (update-state (sublis substitution (action-adds action-struct))
;; 				       (sublis substitution (action-dels action-struct))
;; 				       (sublis substitution (action-costs action-struct))
;; 				       state t negated-preds)))
;; 	    (t (format t "~2%Invalid action: ~a~%in state: ~a" action state)
;; 	       (format t "~% precond unmet: ~a"
;; 		       (find-if-not #'(lambda (precond)
;; 					(precondition-met-p precond state negated-preds special-preds))
;; 				    (sublis substitution (action-preconditions action-struct))))
;; 	       (setq invalid t))))))

;; (defun preconditions-met-p (action-struct state substitution
;; 			    &optional (negated-preds (dom-negative-preds *pspace*))
;; 			      (special-preds (dom-special-preds *pspace*)))
;;   (every #'(lambda (precond)
;; 	     (precondition-met-p precond state negated-preds special-preds))
;; 	 (sublis substitution (action-preconditions action-struct))))
;; 
;; (defun precondition-met-p (precond state negated-preds special-preds)
;;   (let ((negated-pred (negated-pred (car precond) negated-preds)))
;;     (if negated-pred
;; 	(not (member (cons negated-pred (cdr precond)) state :test #'equal))
;; 	(if (eq (car precond) '=)
;; 	    (eq (cadr precond) (caddr precond))
;; 	    (if (negated-equality (car precond) special-preds)
;; 		(not (eq (cadr precond) (caddr precond)))
;; 		(member precond state :test #'equal))))))

;; (defun negated-equality (symbol special-preds)
;;   (let ((symbol-string (format nil "~a" (symbol-name symbol))))
;;     (and (eq (elt symbol-string 0) #\_)
;; 	 (eq (elt symbol-string 1) #\=)
;; 	 (member (intern (string-trim '(#\_ #\=) symbol-string)) special-preds))))

;; It computes the new state after executing an action
(defun update-state (adds dels costs state)
  (let ((previous-cost-def nil)
	(previous-cost 0)
	(found nil)
	(new-state state)
	(negated-pred nil))
    (dolist (del dels)
      (setq new-state (remove del new-state :test #'equal)))
    (dolist (add adds)
      (pushnew add new-state :test #'equal))
    (dolist (cost costs)
      (setq previous-cost-def (find-if #'(lambda (literal)
					   (and (eq (car literal) '=)
						(equal (cadr literal) (cadr cost))))
				       new-state))
      (setq previous-cost (if previous-cost-def (caddr previous-cost-def)))
      (case (car cost)
	(increase (setq new-state
			(cons `(= ,(cadr cost) ,(+ previous-cost (compute-cost (caddr cost) new-state)))
			      (remove previous-cost-def new-state :test #'equal))))
	(decrease (setq new-state
			(cons `(= ,(cadr cost) ,(- previous-cost (compute-cost (caddr cost) new-state)))
			      (remove previous-cost-def new-state :test #'equal))))
	(assign (setq new-state
		      (cons `(= ,(cadr cost) ,(compute-cost (caddr cost) new-state))
			    (remove previous-cost-def new-state :test #'equal))))
	(otherwise nil)))
    (when (or *trace-ma-sayphi* (and (boundp '*trace-execution-p*) *trace-execution-p*))
      (format t "~%  new state literals")
      (pp-list (set-difference new-state state :test #'equal)))
    new-state))

;; (defun update-state (adds dels costs state &optional sayphi-p negated-preds)
;;   (let ((previous-cost-def nil)
;; 	(previous-cost 0)
;; 	(found nil)
;; 	(new-state state)
;; 	(negated-pred nil))
;;     (dolist (del dels)
;;       (cond (sayphi-p
;; 	     (setq negated-pred (negated-pred (car del) negated-preds))
;; 	     (if negated-pred
;; 		 (pushnew (cons negated-pred (cdr del)) new-state :test #'equal)
;; 		 (setq new-state (remove del new-state :test #'equal))))
;; 	    (t (setq new-state (remove del new-state :test #'equal)))))
;;     (dolist (add adds)
;;       (cond (sayphi-p
;; 	     (setq negated-pred (negated-pred (car add) negated-preds))
;; 	     (if negated-pred
;; 		 (setq new-state (remove (cons negated-pred (cdr add)) new-state :test #'equal))
;; 		 (pushnew add new-state :test #'equal)))
;; 	    (t (pushnew add new-state :test #'equal))))
;;     (dolist (cost costs)
;;       (setq previous-cost-def (find-if #'(lambda (literal)
;; 					   (and (eq (car literal) '=)
;; 						(equal (cadr literal) (cadr cost))))
;; 				       new-state))
;;       (setq previous-cost (if previous-cost-def (caddr previous-cost-def)))
;;       (case (car cost)
;; 	(increase (setq new-state
;; 			(cons `(= ,(cadr cost) ,(+ previous-cost (compute-cost (caddr cost) new-state)))
;; 			      (remove previous-cost-def new-state :test #'equal))))
;; 	(decrease (setq new-state
;; 			(cons `(= ,(cadr cost) ,(- previous-cost (compute-cost (caddr cost) new-state)))
;; 			      (remove previous-cost-def new-state :test #'equal))))
;; 	(assign (setq new-state
;; 		      (cons `(= ,(cadr cost) ,(compute-cost (caddr cost) new-state))
;; 			    (remove previous-cost-def new-state :test #'equal))))
;; 	(otherwise nil)))
;;     (when (or *trace-ma-sayphi* *trace-execution-p*)
;;       (format t "~%  new state literals")
;;       (pp-list (set-difference new-state state :test #'equal)))
;;     new-state))

	 
;;; **************************************************************************************
;;;              Managing states
;;; **************************************************************************************

(defun get-lits-from-state (predicates &key (state nil) (node *last-node*) (drive-with-args-p nil))
  (declare (special *last-node*))
  (mapcan #'(lambda (literal)
	      (if (eq (car literal) '=)
		  (if (member (caadr literal) predicates)
		      (if drive-with-args-p
			  (list literal)
			  (list (caadr literal) (caddr literal))))
		  (if (member (car literal) predicates)
		      (list literal))))
	  (if node
	      (if (snode-p node)
		  (pp-state (snode-state node) 'list))
	      state)))

;;; **************************************************************************************
;;;              Matching actions preconds in PDDL
;;; **************************************************************************************

;; action-def is a PDDL definition of an action
;; state is a list of literals and function assignments
;; substitution is an alist of: (var1 . instance1)*
;; predicates is a list of predicates names
;; functions is a list of functions names
(defun pddl-preconditions-matching-p (action-def state substitution predicates functions)
  (match-preconds (sublis substitution (pddl-action-preconditions action-def))
		  state predicates functions))

;; I assume args are the instances of the action parameters
;; it returns two values: a substitution and the list of corresponding types in the same order as the parameters
(defun get-substitution (args params)
  (do* ((instances args (cdr instances))
	(parameters params (cdddr parameters))
	(substitution nil)
	(types nil))
       ((or (null instances) (null parameters))
	(values substitution types))
    (push (caddr parameters) types)
    (push (cons (car parameters) (car instances)) substitution)))

;; it returns T, NIL, or a numeric value (in case of functions)
(defun match-preconds (conditions state predicates functions)
  (case (car conditions)
    (and (every #'(lambda (condition) (match-preconds condition state predicates functions)) (cdr conditions)))
    (or (some #'(lambda (condition) (match-preconds condition state predicates functions)) (cdr conditions)))
    (not (not (match-preconds (cadr conditions) state predicates functions)))
    ((<= >= < > =) (funcall (car conditions)
			    (pddl-function-value (cadr conditions) state functions)
			    (pddl-function-value (caddr conditions) state functions)))
    ((forall exists) t) ;; I do not handle them yet
    (otherwise ;; individual literal (predicate or function)
     (cond ((or (and (eq (car conditions) 'at)
		     (or (eq (cadr conditions) 'start)
			 (eq (cadr conditions) 'end)))
		(and (eq (car conditions) 'over)
		     (eq (cadr conditions) 'all)))
	    (match-preconds (caddr conditions) state predicates functions))
	   ((member (car conditions) predicates)
	    (member conditions state :test #'equal))
	   (t (pddl-function-value conditions state functions))))))

;; in case of error, it returns the most-positive-fixnum
(defun pddl-function-value (exp state functions)
  (cond ((numberp exp) exp)
	((atom exp) most-positive-fixnum)
	((member (car exp) '(+ * - /))
	 (funcall (car exp) (pddl-function-value (cadr exp) state functions)
		  (pddl-function-value (caddr exp) state functions)))
	((member (car exp) functions)
	 (let ((function-value (some #'(lambda (literal)
					 (and (eq (car literal) '=)
					      (equal (cadr literal) exp)
					      (caddr literal)))
				     state)))
	   (or function-value most-positive-fixnum)))
	(t most-positive-fixnum)))

(defun pddl-action-name (action-def)
  (cadr action-def))

(defun pddl-action-parameters (action-def)
  (cadr (member :parameters action-def)))

(defun pddl-action-duration (action-def)
  (cadr (member :duration action-def)))

(defun pddl-action-preconditions (action-def)
  (or (cadr (member :precondition action-def))
      (cadr (member :condition action-def))))

(defun pddl-action-effects (action-def)
  (let ((effects (cadr (member :effect action-def))))
    (if (eq (car effects) 'and)
	(cdr effects)
	(list effects))))
