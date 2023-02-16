(in-package "COMMON-LISP-USER")

;; TODO: si falla el planner (por memoria) y queda tiempo, llamar al dominio/problema/algoritmo del padre

(defvar *max-depth* 3)
(defvar *estimation-timeout* 10.0)
(defvar *tracep* nil)
(defvar *last-node* nil)
(defvar *root-node* nil)
(defvar *node-id* 0)
(defvar *domains-dir* (concatenate 'string *my-planning-path* "sayphi/domains/"))
(defvar *path-baggy* (concatenate 'string *my-planning-path* "bagging/translate/"))
(defvar *path-rida* (concatenate 'string *my-planning-path* "RIDA/"))
(defvar *parse-symba-estimation* (concatenate 'string *path-symba* "parse-symba-estimation.sh "))
(defvar *path-rida-sampling* (concatenate 'string *my-planning-path* ""))
(defvar *path-remove-conditional-effects* (concatenate 'string *my-planning-path* "conditionalEffect/"))
(defvar *ms-operators* nil)
(defvar *heuristics* (list 'lmcut 'hmax 'blind 'potentials
			   'initial_state_potential 'all_states_potential 'sample_based_potentials 'diverse_potentials
			   'operatorcounting 'operatorcounting-lmc 'operatorcounting-pho 'operatorcounting-lmc-seq
			   'cegar 'ipdb 'gapdb 'cpdbs 'merge_and_shrink 'zopdbs)
  "List of all usable heuristics")
(defvar *planners* (list 'fd-opt 'rida 'symba) "List of all usable planners")
(defvar *translators* (list 'fd-jordan 'symba-jordan 'fd-nojordan 'symba-nojordan) "List of usable translators")
(defvar *pre-processors* (list 'vidal-alvaro 'fd) "List of all usable pre-processors")
(defvar *fd-estimation-file* (concatenate 'string *my-path* "estimation.lisp"))
;; (defvar *fd-estimation-file* (concatenate 'string *path-fd* "estimation.lisp"))
(defvar *results-file* "")
(defvar *rida-heuristics-file-postfix* "heuristics.sh")
(defvar *solution-file-postfix* "plan.sol")
(defvar *alarms-file* "")
(defvar *run-baggy-p* nil)
(defvar *can-bag-p* t)
(defvar *original-problem-file* nil)
(defvar *result-dir* "result/")
(defvar *expanded* 0)
(defvar *generated* 0)
(defvar *evaluated* 0)
(defvar *memory-bound* (* 7.8 (expt 2 20)) "7.8Gb in Kb") ;; in .init.lisp or exe generator

;; I will probably have to store more info coming from RIDA as max-f-level, branching factor, ...
(defstruct (meta-node (:print-function meta-node-print))
  (id 0)
  (state nil)
  (domain nil)
  (domain-file nil)
  (problem-file nil)
  (domain-dir nil)
  (probsets-dir nil)
  (depth nil)
  (substitution nil)
  (evaluation most-positive-fixnum)
  (baggable-p t)
  (evaluation-info nil)
  (evaluation-time 0)
  (sampling-time nil)
  (rida-heuristics nil)
  (sampling-plan-p nil)
  (plan-file nil)
  (parent nil)
  (children nil))

;; domains which should work (bagging-all or original) and optimal track: tetris, floortile, nomistery, hiking
;; bagging does not work on a bagged domain
;; try only bagging-all or not bagging
;; create a function that plans: meta-search and then calls rida again to plan.

;; plan-file: path to the file where to write the result plan
;; evaluation-fn: rida-estimation-1 (time), rida-estimation-2 (f level), rida-estimation-3 (f level in time and memory),
;;                rida-estimation-4 (same as before, but better node checks if the avg-time is less than the other one
;;                AND the number of nodes in the closed list of the highest common f-level is less than the other),
;;                rida-estimation-5 (f level time), rida-estimation-6 (f level memory)
;;                symba-estimation, lama-estimation (satisficing), ff-estimation (satisficing), ask (user),
;;                fd-estimation (optimal FD), mixed-estimation (max f-level when using different planners)
;; backtrackingp: T will explore all branches up to max-depth. NIL will only explore best successor
;; ms-operators-keywords: list of names of meta-search ops
;; ms-operators-translators: list of names of translators
;; ms-operators-pre-processors: list of names of pre-processing techniques
;; heuristics: list of names of heuristics
;; combine-heuristics-p: if T, it will generate the set of all combinations of heuristics
;; use-heuristics-p: if T, it will include the decision on which heuristic to use in the meta-search
;; random-ordering-operators-p: if T, it will order the meta-search-operators randomly
;; pruning-ops-fn: a function that prunes nodes from search tree
;; timeout: full timeout (meta-search + search)
;; meta-search-timeout: max time to the whole meta-search
;; search-timeout: time to plan after meta-search
;; estimation-timeout: time for each node in the meta-search
;; fixed-timeout-p: if T, the search-timeout will be fixed and not computed from the remaining time after meta-search
;; meta-search-timeout: timeout for meta-search (*estimation-timeout* is for each estimation)
;; increasing-bags-p: T it will increase set of types to be bagged
;; split-bagging-p: T it will add meta-search operators for each type
;; planners: to be used in meta-search (rida, fd-opt, symba)
;; meta-search-algorithm: ms-enforced-hill-climbing, ms-hill-climbing, ms-generate-and-test, ms-random-walk
;; translate-back-p: if NIL, it will not translate the solution back. Just for debugging or running code "fast"
;; init-state: initial state of the search. If we want to start the search with vidal-alvaro preprocessor, define it as
;;             (original nojordan vidal-alvaro #'original-script)
;;             You can use also bagging as: (bagging all jordan vidal-alvaro #'baggy-script)
;; run-planner-p: if T, it will run the selected nodes at the end. Otherwise, it will just generate nodes and files
;; validate-sol-p: if T, it will validate the solution
;; report-statistics-p: if T, it will write report file

;; examples:
;; (meta-search-planner "tetris" "p02-6.pddl" :evaluation-fn 'rida-estimation-2 :meta-search-timeout 50)
;; (meta-search-planner "tetris" "p02-6.pddl" :evaluation-fn 'lama-estimation :meta-search-timeout 50 :planners '(lama-opt))
;; (meta-search-planner "tetris14" "p02-6.pddl" :evaluation-fn 'fd-estimation :ms-operators-keywords '(bagging original)
;;                      :heuristics '(lmcut ipdb) :use-heuristics-p t :timeout 1800 :estimation-timeout 60 :planners '(fd-opt))
;; (meta-search-planner "tetris14" "p02-6.pddl" :evaluation-fn 'mixed-estimation :ms-operators-keywords '(bagging original)
;;                      :heuristics '(lmcut ipdb) :use-heuristics-p t :timeout 1800 :estimation-timeout 60
;;                      :planners '(fd-opt symba))
(defun meta-search-planner (domain problem-file
			    &key (plan-file "plan.sol")
			      (domain-file "domain.pddl")
			      (domain-dir (concatenate 'string *domains-dir* domain "/"))
			      (probsets-dir (concatenate 'string domain-dir "probsets/"))
			      ;; (output-file (concatenate 'string domain-dir *result-dir* "results-" (pathname-name problem-file) ".txt"))
			      (output-file (concatenate 'string domain-dir *result-dir* "results.out"))
			      (alarms-file (concatenate 'string domain-dir *result-dir* "alarms-"
							(pathname-name problem-file) ".txt"))
			      (finish-fn #'done) (evaluation-fn 'rida-estimation-4)
			      (meta-search-algorithm #'ms-enforced-hill-climbing)
			      (max-depth *max-depth*)
			      (backtrackingp nil)
			      (estimated-number-bagged-types 2)
			      (ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse
							   'inverse 'alphabetical-random 'random))
			      ;; the order is relevant
			      (ms-operators-translators *translators*)
			      (ms-operators-pre-processors *pre-processors*)
			      (heuristics *heuristics*)
			      (combine-heuristics-p nil)
			      (max-combined-heuristics 2) ;; max number of combined heuristics if combine-heuristics-p=T
			      (use-heuristics-p nil)
			      (random-ordering-operators-p nil)
			      (pruning-ops-fn #'applicable-ms-operator-p)
			      (timeout 1800)
			      (translation-back-timeout 10)
			      (search-timeout (- timeout translation-back-timeout))
			      (fixed-timeout-p nil)
			      (meta-search-timeout (/ search-timeout 2.0))
			      (estimation-timeout nil)
			      ;; should it be vidal-alvaro instead of fd?
			      (init-state `(fd-opt original fd-nojordan fd ,#'original-script))
			      (increasing-bags-p nil) (split-bagging-p nil)
			      (planners *planners*) (translate-back-p t)
			      (run-planner-p t)
			      (run-original-planner-p nil)
			      (validate-sol-p nil)
			      (report-statistics-p nil))
  ;; DB: we will most probably have to change this or always call with the right values
  (if (not ms-operators-pre-processors) (setq ms-operators-pre-processors (list 'fd)))
  (if (not ms-operators-translators) (setq ms-operators-translators (list 'fd-nojordan)))
  (setf *alarms-file* alarms-file)
  (setf *max-depth* max-depth)
  (ensure-directories-exist (concatenate 'string domain-dir *result-dir*))
  (when (>= meta-search-timeout timeout)
    (format t "~2%Supplied meta-search-timeout (~,2f) greater than timeout (~,2f).~%Setting meta-search-timeout to half of timeout: ~,2f"
	    meta-search-timeout timeout (/ timeout 2))
    (setq meta-search-timeout (/ (- timeout translation-back-timeout) 2.0)))
  (when (>= estimation-timeout meta-search-timeout)
    (format t "~2%Supplied estimation-timeout (~,2f) greater than meta-search-timeout (~,2f).~%Setting estimation-timeout to 1/3 of meta-search-timeout: ~,2f"
	    estimation-timeout meta-search-timeout (/ meta-search-timeout 3.0))
    (setq estimation-timeout (/ meta-search-timeout 3.0)))
  (format t "~2%Starting meta-search with total timeout ~,2f, meta-search-timeout ~,2f, estimation-timeout, ~,2f, search-timeout ~,2f, max-depth ~,2f"
	  timeout meta-search-timeout estimation-timeout search-timeout max-depth)
  ;; error control
  (cond	((null planners)
	 (format t "~2%Planner not specified. Setting it to RIDA*")
	 (setq planners (list 'rida))
	 (setq evaluation-fn 'rida-estimation-4))
	((or (and (equal planners (list 'symba)) (not (eq evaluation-fn 'symba-estimation)))
	     (and (not (member 'symba planners)) (eq evaluation-fn 'symba-estimation)))
	 (format t "~2%Planner ~a and estimation ~a do not match.~%Converting everything to Symba"
		 planners evaluation-fn)
	 (setq planners (list 'symba))
	 (setq evaluation-fn 'symba-estimation))
	((and (> (length planners) 1) (not (eq evaluation-fn 'mixed-estimation)))
	 (format t "~2%Planner ~a and estimation ~a do not match.~%Converting everything to mixed-estimation"
		 planners evaluation-fn)
	 (setq evaluation-fn 'mixed-estimation))
	((and (= (length planners) 1) (eq evaluation-fn 'mixed-estimation))
	 (format t "~2%Planner ~a and estimation ~a do not match.~%Converting everything to ~a"
		 planners evaluation-fn
		 (case (car planners)
		   (fd-opt (setq evaluation-fn 'fd-estimation))
		   (symba (setq evaluation-fn 'symba-estimation))
		   (rida (setq evaluation-fn 'rida-estimation-4))
		   (otherwise nil))))
	(t nil))
  (let* ((removed-p (remove-conditional-effects-if-any domain-dir probsets-dir domain-file problem-file))
	 (init-time (get-internal-real-time))
	 (ms-time 0)
 	 (solution-time 0)
	 (solution-meta-search nil)
	 (ms-operators (generate-meta-search-operators ms-operators-keywords ms-operators-translators
						       ms-operators-pre-processors heuristics planners
						       use-heuristics-p random-ordering-operators-p
						       combine-heuristics-p max-combined-heuristics))
	 (output-file (if (eq meta-search-algorithm #'ms-generate-and-test)
			  (concatenate 'string domain-dir *result-dir* "results-" (pathname-name problem-file) ".out")
			  (concatenate 'string domain-dir *result-dir* "results.out")))
	 (meta-search-solution-nodes nil))
    (setq estimation-timeout (or estimation-timeout
				 (/ meta-search-timeout
				    (+ (length ms-operators)
				       (* 4 estimated-number-bagged-types)))))
    (setf *estimation-timeout* estimation-timeout)
    (setf *original-problem-file* problem-file)
    (setq meta-search-solution-nodes
	  (meta-search domain problem-file :init-state init-state
		       :finish-fn finish-fn :evaluation-fn evaluation-fn
		       :meta-search-algorithm meta-search-algorithm
		       :domain-file domain-file :domain-dir domain-dir :probsets-dir probsets-dir
		       :backtrackingp backtrackingp :ms-operators ms-operators
		       :pruning-ops-fn pruning-ops-fn
		       :meta-search-timeout meta-search-timeout
		       :search-timeout (- timeout meta-search-timeout)
		       :increasing-bags-p increasing-bags-p :split-bagging-p split-bagging-p
		       :alarms-file alarms-file))
    (dolist (meta-search-solution-node meta-search-solution-nodes)
      (let* ((new-domain-file (meta-node-domain-file meta-search-solution-node))
	     (new-problem-file (meta-node-problem-file meta-search-solution-node))
	     (plan-p (meta-node-sampling-plan-p meta-search-solution-node))
	     (plan nil)
	     (remaining-time 0)
	     (solution-original nil)
	     (solution-node nil)
	     (validated nil)
	     (validated-VAL nil)
	     (state (meta-node-state meta-search-solution-node))
	     (planner (car (intersection (car state) planners)))
	     (selected-heuristics (intersection (car state) *heuristics*))
	     (pre-processor (intersection (car state) *pre-processors*))
	     (translator (intersection (car state) *translators*))
             (internal-plan-file (concatenate 'string domain-dir *result-dir*
					      (generate-node-name meta-search-solution-node) "-" *solution-file-postfix*))
	     ;;(internal-plan-file (meta-node-plan-file meta-search-solution-node))
	     (rida-solving-heuristics-file (meta-node-rida-heuristics meta-search-solution-node)))
	(setq ms-time (elapsed-time init-time 'real-time))
	(setq remaining-time (if (or fixed-timeout-p (eq meta-search-algorithm #'ms-generate-and-test))
				 (- timeout translation-back-timeout)
				 (- timeout ms-time translation-back-timeout)))
	(format t "~2%Result of meta-search (in ~,2f seconds):" ms-time)
        (format t "~2% Selected node ~a: ~,2f"
			     meta-search-solution-node (meta-node-evaluation meta-search-solution-node))
	(format t "~%  domain: ~a~%  problem: ~a" new-domain-file new-problem-file)
	
	;; ms-operator it for RIDA*
	(setq solution-meta-search
	      (cond (plan-p (setq plan (get-solution-from-file planner (meta-node-plan-file meta-search-solution-node)))
			    (when translate-back-p
			      ;; Raquel, lo he quitado porque no hace nada
			      ;; (meta-node-state meta-search-solution-node)
			      (translate-solution-meta-search meta-search-solution-node)
      			      (setq internal-plan-file (meta-node-plan-file meta-search-solution-node))
			      (if (and internal-plan-file (probe-file internal-plan-file))
				  (setq plan (get-solution-from-file planner internal-plan-file))
				  (setq plan-p nil)))
			    (setq ms-time (elapsed-time init-time 'real-time))
			    (cond (plan-p
				   (make-solution :found T
						  :total-time ms-time
						  :length (length plan)
						  :path plan
						  :num-nodes 0
						  :total-cost (length plan)))
				  (t (make-solution :found nil
						    :total-time ms-time
						    :length 0
						    :path nil
						    :num-nodes 0
						    :total-cost 0))))
		    ((not run-planner-p)
		     (make-solution :found nil
				    :total-time (elapsed-time init-time)
				    :length 0
				    :path nil
				    :num-nodes 0
				    :total-cost 0))
		    ((eq planner 'rida)
		     ;; RF the original domain and problem are required in rida function to translate back the solution
		     (rida meta-search-solution-node domain domain-file problem-file new-domain-file new-problem-file
			   probsets-dir domain-dir remaining-time))
		    (t (setq solution-node
			     (solve-problem-after-meta-search meta-search-solution-node planners
							      remaining-time domain probsets-dir domain-dir
							      use-heuristics-p init-time fixed-timeout-p
							      (eq meta-search-algorithm #'ms-generate-and-test)
							      timeout translation-back-timeout))
		       (setq solution-meta-search (car solution-node))
		       (setq meta-search-solution-node (cdr solution-node))
		       (setq internal-plan-file (concatenate 'string domain-dir *result-dir*
							     (generate-node-name meta-search-solution-node)
							     "-" *solution-file-postfix*))
		       (cond ((and (solution-p solution-meta-search)
				   (solution-found solution-meta-search) (solution-path solution-meta-search)
				   (probe-file internal-plan-file)) ;; I do not know why Raquel has added this
			      ;; 			      (some #'(lambda (ms-op) (member 'bagging ms-op))
			      ;; 							  state))
			      (setf (meta-node-plan-file meta-search-solution-node) internal-plan-file)
			      (if translate-back-p
				  (translate-solution-meta-search meta-search-solution-node))
			      ;;(setq internal-plan-file (translate-solution-meta-search meta-search-solution-node))
			      ;;(setf (meta-node-plan-file meta-search-solution-node) internal-plan-file)
			      (setq internal-plan-file (meta-node-plan-file meta-search-solution-node))
			      (cond ((and internal-plan-file (probe-file internal-plan-file))
				     (setq plan (get-solution-from-file planner internal-plan-file))
				     (make-solution :found T
						    :total-time (elapsed-time init-time)
						    :length (length plan)
						    :path plan
						    :num-nodes 0
						    :total-cost (length plan)))
				    (t (setq plan-p nil)
				       (make-solution :found nil
						      :total-time (elapsed-time init-time)
						      :length 0
						      :path nil
						      :num-nodes 0
						      :total-cost 0))))
			     (t (make-solution :found nil
					       :total-time (elapsed-time init-time)
					       :length 0
					       :path nil
					       :num-nodes 0
					       :total-cost 0))))))
	(if (and removed-p (solution-p solution-meta-search) (solution-path solution-meta-search))
	    (setf (solution-path solution-meta-search)
		  (translate-solution-conditional-effects (solution-path solution-meta-search))))
	(if removed-p
	    (restore-original-domain-problem-conditional-effects domain-dir domain-file probsets-dir problem-file))
	(setf (solution-total-time solution-meta-search) (elapsed-time init-time 'real-time))
	(format t "~2%Solution (total time ~,2f seconds):~%~a"
		(setq solution-time (solution-total-time solution-meta-search)) solution-meta-search)
	(when run-original-planner-p
	  ;; DB: I do not think we should do this
	  ;; 	    (cond ((eq (caar (meta-node-state meta-search-solution-node)) 'original)
	  ;; 		   (setq solution-original solution-meta-search))
	  ;; 		  (t 
	  ;; for comparison
	  (format t "~2%Calling ~a with~%  domain: ~a~%  problem: ~a~%Solution:~%" planner domain-file problem-file)
	  (setq solution-original
		(if (eq planner 'rida)
		    (rida *root-node* domain domain-file problem-file domain-file problem-file
			  probsets-dir domain-dir timeout)
		    (the-ring planner timeout domain domain-file problem-file :probsets-dir probsets-dir
			      :domain-dir domain-dir)))
	  (print solution-original))

	;; prints the solution in the required plan-file. As a side effect, it computes the cost well
	(if (and solution-meta-search (solution-found solution-meta-search))
	    (pp-solution solution-meta-search :ipc-p t :unit-cost-p nil
			 :domain domain :domain-file domain-file
			 :domain-dir domain-dir :problem-file problem-file
			 :probsets-dir probsets-dir :plan-file plan-file))
	(setq validated (if (and solution-meta-search (solution-found solution-meta-search) validate-sol-p)
			    (ignore-errors (validate-sol :solution solution-meta-search
							 :domain-file domain-file :problem-file problem-file
							 :domain-dir domain-dir :probsets-dir probsets-dir)
					   :not-validated)))
	(setq validated-val (if (and solution-meta-search (solution-found solution-meta-search) validate-sol-p)
				(validate-VAL domain-dir domain-file probsets-dir problem-file plan-file)))

	(if (and report-statistics-p (not (eq meta-search-algorithm #'ms-generate-and-test)))
	    (report-statistics meta-search-solution-node output-file problem-file new-domain-file new-problem-file
			       solution-meta-search ms-time solution-time use-heuristics-p selected-heuristics planner
			       rida-solving-heuristics-file translator pre-processor validated validated-VAL))))
    solution-meta-search))
;; 	(if removed-p (setq solution-meta-search (translate-back-conditional-effects plan-file solution-meta-search
;; 										     domain-dir domain-file probsets-dir problem-file)))

(defun meta-search (domain problem-file
		    &key (init-state '(fd-opt original fd-nojordan fd #'null))
		      (finish-fn #'done) (evaluation-fn 'rida-estimation-4)
		      (domain-file "domain.pddl")
		      (domain-dir (concatenate 'string *domains-dir* domain "/"))
		      (probsets-dir (concatenate 'string domain-dir "probsets/"))
		      (backtrackingp nil) (pruning-ops-fn #'applicable-ms-operator-p)
		      (meta-search-algorithm #'ms-enforced-hill-climbing)
		      (ms-operators nil)
		      (meta-search-timeout (/ 1800 2)) (search-timeout (/ 1800 2))
		      (increasing-bags-p nil) (split-bagging-p nil)
		      (results-file (case evaluation-fn
				      (symba-estimation "symba-estimation.lisp")
				      (fd-estimation "fd-estimation.lisp")
				      (mixed-estimation "mixed-estimation.lisp")
				      (otherwise "RIDA_predictions.lisp")))
		      (alarms-file (concatenate 'string domain-dir *result-dir* "alarms-"
						(pathname-name problem-file) ".txt")))
  (setf *run-baggy-p* (not split-bagging-p))
  (setf *can-bag-p* t)
  (setf *ms-operators* ms-operators)
  (setf *expanded* 0)
  (setf *generated* 0)
  (setf *evaluated* 0)
  (setf *node-id* -1)
  ;;  (setf *results-file*  (concatenate 'string domain-dir "result/" results-file))
  (setf *results-file*  results-file)
  (let* ((initial-time (get-internal-real-time))
	 (initial-node (create-initial-node init-state evaluation-fn (not (eq meta-search-algorithm #'ms-random-walk))
					    domain problem-file domain-file domain-dir
					    probsets-dir search-timeout increasing-bags-p initial-time))
	 (solution (funcall meta-search-algorithm initial-node finish-fn evaluation-fn
			    domain problem-file domain-file domain-dir probsets-dir
			    backtrackingp increasing-bags-p initial-time meta-search-timeout search-timeout
			    pruning-ops-fn)))
    (format t "~%Meta-search time: ~,2f" (elapsed-time initial-time 'real-time))
    (if (eq meta-search-algorithm #'ms-generate-and-test)
	*generated*
	(list solution))))

(defun generate-meta-search-operators (ms-operators-keywords ms-operators-translators
				       ms-operators-pre-processors heuristics planners
				       use-heuristics-p random-ordering-operators-p combine-heuristics-p
				       max-combined-heuristics)
  (let ((meta-search-operators
	 (mapcan #'(lambda (op)
		     (generate-complete-ms-op op ms-operators-translators ms-operators-pre-processors heuristics
					      planners use-heuristics-p combine-heuristics-p max-combined-heuristics))
		 (append (if (member 'bagging ms-operators-keywords)
			     (list (list 'bagging 'all #'baggy-script)))
			 (if (member 'original ms-operators-keywords)
			     (list (list 'original #'original-script)))
			 (if (member 'alphabetical-inverse ms-operators-keywords)
			     (list (list 'alphabetical 'inverse #'inverse-alphabetical-script)))
			 (if (member 'inverse ms-operators-keywords)
			     (list (list 'inverse #'inverse-script)))
			 (if (member 'alphabetical-random ms-operators-keywords)
			     (list (list 'alphabetical 'random #'random-alphabetical-script)))
			 (if (member 'random ms-operators-keywords)
			     (list (list 'random #'random-script)))))))
    (if random-ordering-operators-p
	(shuffle meta-search-operators :test #'equal)
	meta-search-operators)))

(defun generate-complete-ms-op (op ms-operators-translators ms-operators-pre-processors heuristics planners
				use-heuristics-p combine-heuristics-p max-combined-heuristics)
  (let ((result nil)
	(ms-op nil))
    (dolist (planner (reverse planners))
      (dolist (ms-operators-translator (reverse ms-operators-translators))
	(if (or (not (eq planner 'symba)) (eq ms-operators-translator 'symba-nojordan))
	    (dolist (ms-operators-pre-processor (reverse ms-operators-pre-processors))
	      (cond ((and (eq planner 'symba) (not (eq ms-operators-pre-processor 'vidal-alvaro))))
		    ((and use-heuristics-p (not (eq planner 'symba)))
		     (dolist (heuristic-combination (heuristics-combinations (reverse heuristics) combine-heuristics-p
									     max-combined-heuristics))
		       (setq ms-op (generate-ms-op op ms-operators-translator ms-operators-pre-processor
						   heuristic-combination planner))
		       (if ms-op (push ms-op result))))
		    (t (setq ms-op (generate-ms-op op ms-operators-translator ms-operators-pre-processor nil planner))
		       (if ms-op (push ms-op result))))))))
    result))

(defun heuristics-combinations (heuristics combine-heuristics-p max-combined-heuristics)
  (if combine-heuristics-p
      (sort (remove-if #'(lambda (combination)
			   (or (null combination)
			       (> (length combination) max-combined-heuristics)))
		       (power-set heuristics))
	    #'(lambda (x y) (<= (length x) (length y))))
      (mapcar #'list heuristics)))

;; it returns the power set of the input list
(defun power-set (list)
  (if list
      (mapcan #'(lambda (x) (list (cons (car list) x) x))
	      (power-set (cdr list)))
      (list nil)))

(defun generate-ms-op (op ms-operators-translator ms-operators-pre-processor heuristics planner)
  (if (not (and (not (member 'bagging op))
		(or (eq ms-operators-translator 'fd-jordan)
		    (eq ms-operators-translator 'symba-jordan))))
      (if heuristics
	  (append (list planner) (butlast op) (list ms-operators-translator ms-operators-pre-processor) heuristics (last op))
	  (append (list planner) (butlast op) (list ms-operators-translator ms-operators-pre-processor) (last op)))))

;; It tries to solve the problem with the best configuration derived by MS
;; If it fails with the current MS node, it tries with the ancestor node configuration
(defun solve-problem-after-meta-search (meta-search-solution-node planners remaining-time domain probsets-dir domain-dir
					use-heuristics-p init-time fixed-timeout-p
					generate-and-test-p timeout translation-back-timeout)
  (do* ((solution (make-solution))
	(continue-p t (and (not (solution-found solution)) node (meta-node-parent node)))
	(node meta-search-solution-node (if continue-p (meta-node-parent node) node))
	(state (meta-node-state node) (if (and continue-p node) (meta-node-state node) state))
	(domain-file (meta-node-domain-file node) (if (and continue-p node) (meta-node-domain-file node) domain-file))
	(problem-file (meta-node-problem-file node) (if (and continue-p node) (meta-node-problem-file node) problem-file))
	(planner (car (intersection (car state) planners))
		 (if (and continue-p node) (car (intersection (car state) planners)) planner))
	(selected-heuristics (intersection (car state) *heuristics*)
			     (if (and continue-p node) (intersection (car state) *heuristics*) selected-heuristics))
	(pre-processor (intersection (car state) *pre-processors*)
		       (if (and continue-p node) (intersection (car state) *pre-processors*) pre-processor))
	(internal-plan-file (concatenate 'string domain-dir *result-dir* (generate-node-name node)
					 "-" *solution-file-postfix*)
			    (if (and continue-p node)
				(concatenate 'string domain-dir *result-dir* (generate-node-name node)
					     "-" *solution-file-postfix*)
				internal-plan-file))
	(total-time 0)
	(solving-time remaining-time))
       ((or (and (solution-found solution) (solution-path solution)) (<= solving-time 0) (not node))
	(cons solution node))
    (setq solution (the-ring planner solving-time
			     domain domain-file problem-file
			     :probsets-dir probsets-dir
			     :domain-directory  domain-dir
			     :plan-file internal-plan-file
			     :search-options (generate-search-options (and use-heuristics-p selected-heuristics)
								      pre-processor
								      (/ solving-time 2))))
    (setq total-time (elapsed-time init-time 'real-time))
    (setq solving-time (if (or fixed-timeout-p generate-and-test-p)
			   (- timeout translation-back-timeout)
			   (- timeout total-time translation-back-timeout)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Meta Search Algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ms-hill-climbing (node finish-fn evaluation-fn domain problem-file domain-file
			      domain-dir probsets-dir backtrackingp increasing-bags-p
			      initial-time meta-search-timeout search-timeout pruning-ops-fn)
  (if *tracep* (format t "~2%Expanding ~a (after ~,2f seconds)" node (elapsed-time initial-time 'real-time)))
  (setf *last-node* node)
  (incf *expanded*)
  (cond ((and (functionp finish-fn)
	      (funcall finish-fn node initial-time meta-search-timeout))
	 node)
	(t (let* ((successors (sort-successors (generate-successors node evaluation-fn increasing-bags-p t
								    search-timeout pruning-ops-fn initial-time)
					       evaluation-fn search-timeout))
		  (solution (cond ((and (= (length successors) 1)
					(meta-node-sampling-plan-p (car successors)))
				   (car successors))
				  ;; best successor is better than parent
				  ((and successors
					(eq (car successors)
					    (car (sort-successors (list node (car successors)) evaluation-fn
								  search-timeout))))
				   (if backtrackingp
				       (car (sort-successors
					     (mapcar #'(lambda (successor)
							 (ms-hill-climbing successor finish-fn evaluation-fn
									   domain problem-file domain-file
									   domain-dir probsets-dir
									   backtrackingp increasing-bags-p
									   initial-time meta-search-timeout
									   search-timeout pruning-ops-fn))
						     successors)
					     evaluation-fn search-timeout))
				       (ms-hill-climbing (car successors) finish-fn evaluation-fn
							 domain problem-file domain-file domain-dir probsets-dir 
							 backtrackingp increasing-bags-p
							 initial-time meta-search-timeout search-timeout pruning-ops-fn)))
				  (t node))))
	     (incf *generated* (length successors))
	     solution))))
;; 	     (if solution (cons node solution))))))

(defun ms-enforced-hill-climbing (node finish-fn evaluation-fn domain problem-file domain-file
				       domain-dir probsets-dir backtrackingp increasing-bags-p
				       initial-time meta-search-timeout search-timeout pruning-ops-fn)
  (if *tracep* (format t "~2%Expanding ~a (after ~,2f seconds)" node (elapsed-time initial-time 'real-time)))
  (setf *last-node* node)
  (incf *expanded*)
  (cond ((and (functionp finish-fn)
	      (funcall finish-fn node initial-time meta-search-timeout))
	 node)
	(t (do* ((successors (generate-successors node evaluation-fn increasing-bags-p nil search-timeout
						  pruning-ops-fn initial-time)
			     (cdr successors))
		 (evaluation 0)
		 (better-node nil))
	       ((or (null successors)
		    better-node
		    (and (functionp finish-fn)
			 (funcall finish-fn (car successors) initial-time meta-search-timeout)))
		(incf *generated* (length successors))
		(or better-node
		    (if (eq evaluation-fn 'rida-estimation-1)
			(car (sort-successors (cons node successors) evaluation-fn search-timeout))
			node)))
	     (setq evaluation (evaluate-node (car successors) evaluation-fn search-timeout initial-time))
	     (when  *tracep*
	       (format t "~2% Evaluation of node ~a: ~,2f" (car successors) evaluation)
	       (format t "~2% Evaluated at time: ~,2f" (meta-node-evaluation-time (car successors)))) 

	       
	     (setq better-node
		   (cond ((meta-node-sampling-plan-p (car successors))
			  (car successors))
			 ;; best successor is better than parent
			 ((better-node-p (car successors) node evaluation-fn search-timeout 0)
			  (when (member evaluation-fn '(rida-estimation-2 rida-estimation-3 rida-estimation-4
							rida-estimation-5 rida-estimation-6))
			    (if (> evaluation 10000)
				(print-alarm "~2% ~a. Evaluation of ~a (~,2f) too high"
					     (pathname-name problem-file)
					     (generate-node-name (car successors)) evaluation))
			    (if (and (eq evaluation-fn 'rida-estimation-4)
				     (< (meta-node-evaluation (car successors))
					(meta-node-evaluation node)))
				(print-alarm "~2% ~a. Node ~a selected over its parent ~a ~%due to better Pat's heuristic"
					     (pathname-name problem-file)
					     (car successors) node))
			    (if (and (> (meta-node-evaluation node) 0)
				     (> evaluation 0)
				     (or (> (/ evaluation (meta-node-evaluation node)) 10)
					 (> (/ (meta-node-evaluation node) evaluation) 10)))
				(print-alarm "~2% ~a. Difference of evaluation of node ~a (~,2f) wrt its parent ~a (~,2f) too high"
					     (pathname-name problem-file)
					     (generate-node-name (car successors)) evaluation
					     (generate-node-name node) (meta-node-evaluation node))))
			  (ms-enforced-hill-climbing (car successors) finish-fn evaluation-fn
						     domain problem-file domain-file domain-dir probsets-dir 
						     backtrackingp increasing-bags-p
						     initial-time meta-search-timeout search-timeout pruning-ops-fn))
			 (t nil)))
	     (if *tracep* (format t "~2% Better node: ~a" better-node))))))

(defun ms-random-walk (node finish-fn evaluation-fn domain problem-file domain-file
		       domain-dir probsets-dir backtrackingp increasing-bags-p
		       initial-time meta-search-timeout search-timeout pruning-ops-fn)
  (if *tracep* (format t "~2%Expanding ~a (after ~,2f seconds)" node (elapsed-time initial-time 'real-time)))
  (setf *last-node* node)
  (incf *expanded*)
  (cond ((and (functionp finish-fn)
	      (funcall finish-fn node initial-time meta-search-timeout))
	 (evaluate-node node evaluation-fn search-timeout initial-time)
	 node)
	(t (let* ((successors (generate-successors node evaluation-fn increasing-bags-p nil
						   search-timeout pruning-ops-fn initial-time))
		  (random-successor (choose-one successors)))
	     (incf *generated* (length successors))
	     (if *tracep* (format t "~2% Random node: ~a" random-successor))
	     (cond (random-successor
		    (ms-random-walk random-successor finish-fn evaluation-fn
				    domain problem-file domain-file domain-dir probsets-dir 
				    backtrackingp increasing-bags-p
				    initial-time meta-search-timeout search-timeout pruning-ops-fn))
		   (t (evaluate-node node evaluation-fn search-timeout initial-time)
		      node))))))

(defun ms-generate-and-test (node finish-fn evaluation-fn domain problem-file domain-file
			     domain-dir probsets-dir backtrackingp increasing-bags-p
			     initial-time meta-search-timeout search-timeout pruning-ops-fn)
  (if *tracep* (format t "~2%Expanding ~a (after ~,2f seconds)" node (elapsed-time initial-time 'real-time)))
  (do ((open (list node)))
      ((null open) node)
    (dolist (successor (generate-successors (pop open) evaluation-fn increasing-bags-p nil search-timeout
					    pruning-ops-fn initial-time))
      (push successor open)
      (setf *last-node* successor)
      (incf *generated*)
      (incf *expanded*)
      (evaluate-node successor evaluation-fn search-timeout initial-time)
      (if *tracep* (format t "~2% Evaluation of node ~a: ~,2f" successor (meta-node-evaluation successor))))))
  
(defun generate-successors (node evaluation-fn increasing-bags-p evaluate-node-p search-timeout
			    pruning-ops-fn initial-time)
  (if *tracep* (format t "~2%Generating successors of node ~a" node))
  (let ((successors nil)
	(init-time (get-internal-real-time))
	(domain (meta-node-domain node))
	(domain-file (meta-node-domain-file node))
	(problem-file (meta-node-problem-file node))
	(domain-dir (meta-node-domain-dir node))
	(probsets-dir (meta-node-probsets-dir node))
	(state (meta-node-state node)))
    (dolist (ms-operator *ms-operators*)
      ;;       (if (not (or (assoc (car ms-operator) state)
      ;; 		   (and (eq (car ms-operator) 'bagging)
      ;; 			(some #'(lambda (previous-ms-operator) (member 'bagging previous-ms-operator)) state))))
      (if (funcall pruning-ops-fn ms-operator state)
	  (push ms-operator successors)))
    (do* ((ops successors (cdr ops))
	  (successor (car ops) (car ops))
	  (first-baggy-child nil)
	  (result-nodes nil)
	  (plan-node nil)
	  (child nil))
	 ((or (null ops) plan-node)
	  (if plan-node (list plan-node) result-nodes))
      ;;       (if *tracep* (format t "~%  using operator ~a" successor))
      (setq child (create-node successor node domain domain-file domain-dir problem-file probsets-dir
			       evaluation-fn evaluate-node-p increasing-bags-p search-timeout first-baggy-child initial-time))
      (if (and (member 'bagging successor)
	       (not first-baggy-child))
	  (setq first-baggy-child child))
      (when *tracep*
	(format t "~2%Created child in ~,2f seconds. Time spent by meta-search ~,2f seconds"
		(elapsed-time init-time 'real-time) (elapsed-time initial-time 'real-time))
	(setq init-time (get-internal-real-time)))
      (when (and (member evaluation-fn '(rida-estimation-1 rida-estimation-2 rida-estimation-3 rida-estimation-4
					 rida-estimation-5 rida-estimation-6 symba-estimation fd-estimation mixed-estimation))
		 evaluate-node-p
		 (meta-node-sampling-plan-p child))
	(if *tracep* (format t "~%SOLVED by " child))
	(setq plan-node child))
      (push child (meta-node-children node))
      (if *tracep* (format t "~2%  Child ~a" child))
      (push child result-nodes))))
;; 					     :state (cons (cons (car successor)
;; 								  (funcall (cdr successor) domain problem-file domain-file
;; 									   new-domain-file new-problem-file))
;; 							    (meta-node-state node))

;; Pruning rules. A node is pruned if either:
;; a) it uses original with the same translator and pre-processor than an ancestor
;; b) it uses bagging and it was already detected that bagging cannot be performed
;; c) it uses bagging and it was used before
;; d) it uses any random operator and an ancestor already used any random option
;; e) it is the same operator applied in an ancestor
(defun applicable-ms-operator-p (ms-operator state)
  (not (or (if (member 'original ms-operator)
	       (or (same-preprocessor-translator-p ms-operator (car state))
		   (and (member 'original (car state))
			(repeated-original-p ms-operator (cdr state))))
	       (member ms-operator state :test #'equal))
	   (and (member 'bagging ms-operator)
		(or (not *can-bag-p*)
		    (some #'(lambda (op) (member 'bagging op)) state)))
	   (some #'(lambda (op) (member 'random op)) state))))

(defun applicable-ms-operator-not-original-p (ms-operator state)
  (not (or (member ms-operator state :test #'equal)
	   (and (member 'bagging ms-operator)
		(or (not *can-bag-p*)
		    (some #'(lambda (op) (member 'bagging op)) state)))
	   (some #'(lambda (op) (member 'random op)) state))))

;; (defun applicable-ms-operator-p (ms-operator state)
;;   (and (not (member ms-operator state :test #'equal))
;;        (not (and (member 'bagging ms-operator)
;; 		 (or (not *can-bag-p*)
;; 		     (some #'(lambda (op) (member 'bagging op)) state))))
;;        (notany #'(lambda (op) (member 'random op)) state)))
;;   (and (not (assoc (car ms-operator) state))

(defun same-preprocessor-translator-p (ms-operator previous-op)
  (let ((translator-ms-operator (or (member 'fd-jordan ms-operator) (member 'fd-nojordan ms-operator)
				    (member 'symba-jordan ms-operator) (member 'symba-nojordan ms-operator)))
	(translator-previous-op (or (member 'fd-jordan previous-op) (member 'fd-nojordan previous-op)
				    (member 'symba-jordan previous-op) (member 'symba-nojordan previous-op))))
    (and (eq (car translator-ms-operator) (car translator-previous-op))
	 (eq (cadr translator-ms-operator) (cadr translator-previous-op)))))

(defun repeated-original-p (ms-operator state)
  (if state
      (if (member 'original (car state))
	  (if (same-preprocessor-translator-p ms-operator (car state))
	      t
	      (repeated-original-p ms-operator (cdr state))))))

(defun create-node (state node domain domain-file domain-dir problem-file probsets-dir evaluation-fn evaluate-node-p
		    increasing-bags-p search-timeout first-baggy-child initial-time)
  (let* ((child (make-meta-node :id (incf *node-id*)
				:parent node
				:domain domain
				:domain-dir domain-dir
				:probsets-dir probsets-dir
				:state (if node
					   (cons state (meta-node-state node))
					   (list state))
				:evaluation (if (member evaluation-fn '(rida-estimation-2 rida-estimation-3
									rida-estimation-4 rida-estimation-5
									rida-estimation-6 ask
									symba-estimation fd-estimation
									mixed-estimation))
						0
						most-positive-fixnum)
				:depth (if node
					   (1+ (meta-node-depth node))
					   0)))
	 (baggingp (member 'bagging state))
	 (call-generator-p (not (and first-baggy-child baggingp)))
	 (new-domain-file (if (and node (> (length (meta-node-state node)) 1))
			      (format nil "~a-~d-~a" (from-list-to-string (butlast state))
				      *node-id* domain-file)
			      (format nil "~a-~d-~a-~a" (from-list-to-string (butlast state))
				      *node-id* (pathname-name problem-file) domain-file)))
	 (new-problem-file (format nil "~a-~d-~a" (from-list-to-string (butlast state))
				   *node-id* problem-file))
	 (result (if call-generator-p
		     (funcall (car (last state)) domain problem-file domain-file domain-dir probsets-dir
			      new-domain-file new-problem-file))))
    (when (and (not *run-baggy-p*) *can-bag-p* baggingp (member 'all state))
      (setf *run-baggy-p* t)
      (update-operators domain domain-file domain-dir increasing-bags-p))
    (cond (call-generator-p
	   (setf (meta-node-domain-file child) (nth 0 result))
	   (setf (meta-node-problem-file child) (nth 1 result))
	   (setf (meta-node-substitution child) (nth 2 result))
	   (setf (meta-node-baggable-p child) (nth 3 result)))
	  (t (copy-bagging-information first-baggy-child child domain problem-file
				       domain-dir probsets-dir new-domain-file new-problem-file)))
    (when (and evaluate-node-p (or *can-bag-p* (not baggingp)))
      (evaluate-node child evaluation-fn search-timeout initial-time)
      (when *tracep*
	  (format t "~2% Evaluation of node ~a: ~,2f (can-bug-p=~a)" child (meta-node-evaluation child) *can-bag-p*)
          (format t "~2% Evaluated at time: ~,2f" (meta-node-evaluation-time child))))
    child))

(defun create-initial-node (init-state evaluation-fn evaluate-node-p domain problem-file domain-file domain-dir probsets-dir
			    search-timeout increasing-bags-p initial-time)
  (setf *root-node*
	(create-node init-state nil domain domain-file domain-dir problem-file probsets-dir
		     evaluation-fn evaluate-node-p increasing-bags-p search-timeout nil initial-time)))

(eval-when (load eval compile)
  (defmacro get-assoc-value (assoc-value)
    `(if (listp (cdr ,assoc-value))
	 (cadr ,assoc-value)
	 (cdr ,assoc-value)))
  
  (defmacro get-last (key evaluation-info)
    `(find ,key ,evaluation-info :key #'car :from-end t))

  (defmacro get-hbf (evaluation-info)
    `(or (get-assoc-value (assoc 'hbf ,evaluation-info))
	 (let ((hbf-f (get-assoc-value (get-last 'hbf-f ,evaluation-info)))
	       (hbf-g nil))
	   (cond ((and (numberp hbf-f) (> hbf-f 1))
		  hbf-f)
		 (t (setq hbf-g (get-assoc-value (get-last 'hbf-g ,evaluation-info)))
		    (if (and (numberp hbf-g) (> hbf-g 1))
			hbf-g))))))

  (defmacro get-last-f-bound (evaluation-info)
    `(get-assoc-value (get-last 'f_bound ,evaluation-info)))

  (defmacro get-time-per-node (evaluation-info)
    `(or (get-assoc-value (get-last 'avg_total_time_costs_per_expanded_node ,evaluation-info))
	 (get-assoc-value (get-last 'avg_total_time_costs_per_generated_node ,evaluation-info))))

  (defmacro get-init-mem (evaluation-info)
    `(get-assoc-value (assoc 'InitialMemory ,evaluation-info)))

  (defmacro get-final-mem (evaluation-info)
    `(or (get-assoc-value (assoc 'FinalStageMemory ,evaluation-info))
	 (get-assoc-value (get-last 'Memory ,evaluation-info))))

  (defmacro get-f-boundary (evaluation-info)
    `(get-assoc-value (assoc 'next_f_boundary ,evaluation-info)))

  ;; I do not check get-last of ClosedListNodes because the function that uses this macro does a reverse before calling it
  (defmacro get-closed (evaluation-info)
    `(or (get-assoc-value (assoc 'closedlistentries ,evaluation-info))
	 (get-assoc-value (assoc 'ClosedListNodes ,evaluation-info))))

  (defmacro get-final-closed (evaluation-info)
    `(or (get-assoc-value (assoc 'FinalStageClosedListEntries ,evaluation-info))
	 (get-assoc-value (get-last 'ClosedListNodes ,evaluation-info))))

  (defmacro get-next-f-level-time (evaluation-info)
    `(get-assoc-value (assoc 'next_f_level_time ,evaluation-info)))

  (defmacro get-time-to-solve (evaluation-info)
    `(get-assoc-value (assoc 'time-to-solve ,evaluation-info)))
  )

;; evaluates a node according to evaluation-fn and stores the evaluation (a number) in the meta-node-evaluation attribute
(defun evaluate-node (node evaluation-fn search-timeout initial-time)
  (incf *evaluated*)
  (setf (meta-node-evaluation-time node) (elapsed-time initial-time 'real-time))
  (setf (meta-node-evaluation node)
	(case evaluation-fn
	  (rida-estimation-1 (if (meta-node-baggable-p node)
				 (funcall (eval '(function rida-estimation)) node)
				 most-positive-fixnum))
	  (rida-estimation-2
	   (cond ((meta-node-baggable-p node)
		  (funcall (eval '(function rida-estimation)) node)
		  (let* ((evaluation-info (meta-node-evaluation-info node))
			 (avg_eval_per_node (get-time-per-node evaluation-info))
			 (hbf (get-hbf evaluation-info)))
		    (if (or (not avg_eval_per_node) (not hbf))
			0
			(if (or (<= hbf 1) (zerop avg_eval_per_node))
			    0
			    (/ (log (/ search-timeout avg_eval_per_node))
			       (log hbf))))))
		 (t 0)))
	  ((rida-estimation-3 rida-estimation-4 rida-estimation-5 rida-estimation-6 fd-estimation)
	     (cond ((meta-node-baggable-p node)
		    (if (eq evaluation-fn 'fd-estimation)
			(fd-estimation node 'fd-opt)
			(funcall (eval '(function rida-estimation)) node))
		    (compute-estimation node evaluation-fn search-timeout))
		   (t 0)))
	  (fbound-estimation
	     (cond ((meta-node-baggable-p node)
		    (fd-estimation node 'fd-opt)
		    (or (get-last-f-bound (meta-node-evaluation-info node))
			0))
		   (t 0)))
	  (ask (format t "~% Value of node ~a? " node)
	       (read))
	  (lama-estimation ;; runs a satisficing planner
	   (cond ((meta-node-baggable-p node)
		  (fd-estimation node 'lama-first)
		  (or (get-time-to-solve (meta-node-evaluation-info node))
		      most-positive-fixnum))
		 (t most-positive-fixnum)))
	  (symba-estimation ;; runs symba that can return either a solution or an explored f-level
	   (if (meta-node-baggable-p node)
	       (compute-symba-estimation node)
	       0))
	  (mixed-estimation ;; the estimation is derived from the planner to be used
	   (evaluate-node node
			  (case (car (intersection (car (meta-node-state node)) *planners*))
			    (fd-opt 'fbound-estimation)
			    (symba 'symba-estimation)
			    (rida 'rida-estimation-4)
			    (otherwise 'rida-estimation-4))
			  search-timeout initial-time))
	  (t (if (meta-node-baggable-p node)
		 (funcall (eval `(function ,evaluation-fn)) node)
		 most-positive-fixnum)))))

(defun compute-estimation (node evaluation-fn search-timeout)
  (let* ((evaluation-info (meta-node-evaluation-info node))
	 (compute-time-p (and evaluation-info
			      (not (numberp evaluation-info))
			      (not (eq evaluation-fn 'rida-estimation-6))))
	 (compute-memory-p (and evaluation-info
			      (not (numberp evaluation-info))
			      (not (eq evaluation-fn 'rida-estimation-5))))
	 ;; only compare the highest common f-level expanded
	 (avg_eval_per_node (and compute-time-p (get-time-per-node evaluation-info)))
	 ;; (#nodes-closed-levelX - #nodes-closed-levelX-1)/(#nodes-closed-levelX-1 - #nodes-closed-levelX-2)
	 (hbf (if (or compute-time-p compute-memory-p) (get-hbf evaluation-info)))
	 (init-memory (and compute-memory-p (get-init-mem evaluation-info)))
	 ;; use the last midstage memory instead of final
	 (end-memory (and compute-memory-p (get-final-mem evaluation-info)))
	 ;; use closedlistentries of last flevel
	 (num-nodes-closed (and compute-memory-p (get-final-closed evaluation-info)))
	 (hbf-p (and hbf (> hbf 1)))
	 (data-for-time-p (and compute-time-p hbf-p avg_eval_per_node (not (zerop avg_eval_per_node))))
	 (f-time (if data-for-time-p
		     (/ (log (/ search-timeout avg_eval_per_node))
			(log hbf))))
	 (data-for-memory-p (and compute-memory-p hbf-p init-memory end-memory num-nodes-closed
				 (> end-memory init-memory)))
	 (f-memory (if data-for-memory-p
		       (- (/ (log (+ 1 (/ (* num-nodes-closed
					     (- *memory-bound* init-memory)
					     (- hbf 1))
					  (- end-memory init-memory))))
			     (log hbf))
			  1))))
    (if (and end-memory init-memory (= end-memory init-memory))
	(print-alarm "~2%Init memory equal to end-memory: ~,2f" init-memory))
    (if (not hbf-p)
	(print-alarm "~2%Incorrect HBF value: ~,2f" hbf))
    (if (not avg_eval_per_node)
	(print-alarm "~2%Incorrect avg_eval_per_node value: ~,2f" avg_eval_per_node))
    (if *tracep*
	(format t "~2%File name: ~a~%Contents of results file: ~a~%Evaluation data: f-time (~,2f), f-memory (~,2f)"
		(concatenate 'string (generate-node-name node) "-" *results-file*)
		evaluation-info f-time f-memory))
    (if data-for-time-p
	(if data-for-memory-p
	    (min f-time f-memory)
	    f-time)
	(if data-for-memory-p
	    f-memory
	    0))))

;; Now it just returns the last expanded f-level
(defun compute-symba-estimation (node)
  (let ((evaluation (fd-estimation node 'symba)))
    (cond ((null evaluation) 0)
	  ((listp evaluation)
	   (or (caddr (find 'step evaluation :key #'car :from-end t)) 0))
	  ((numberp evaluation) evaluation)
	  (t 0))))

;; Now it just returns the last expanded f-level
;; I have to change it for the new version of Pat
;; (defun compute-fd-opt-estimation (node)
;;   (let ((evaluation (fd-estimation node 'fd-opt)))
;;     (cond ((listp evaluation)
;; 	   (if (eq (caar (last evaluation)) 'f)
;; 	       (cadar (last evaluation))
;; 	       (cadr (find 'f_bound evaluation :key #'car :from-end t))))
;; 	  ((numberp evaluation) evaluation)
;; 	  (t 0))))

(defun sort-successors (nodes evaluation-fn search-timeout)
  (if (> (length nodes) 1)
      (if (eq evaluation-fn 'rida-estimation-1)
	  (sort-by-rida nodes evaluation-fn search-timeout)
	  (sort nodes #'(lambda (node1 node2) (better-node-p node1 node2 evaluation-fn search-timeout nil))))
      nodes))

(defun better-node-p (node1 node2 evaluation-fn search-timeout max-f-boundary)
  (case evaluation-fn
    ((rida-estimation-4 fd-estimation) (better-statistics node1 node2))
    ((rida-estimation-2 rida-estimation-3 rida-estimation-5 rida-estimation-6 ask
			symba-estimation mixed-estimation)
     (> (meta-node-evaluation node1) (meta-node-evaluation node2)))
    ((lama-estimation ff-estimation)
     (let ((eval-node1 (meta-node-evaluation node1))
	   (eval-node2 (meta-node-evaluation node2)))
       (and (or (> eval-node1 1.0)
		(> eval-node2 1.0))
	    ;; I assume that it has to be at least a 10% better than the other one in time
	    (< eval-node1 (* 0.9 eval-node2)))))
    ;; 2. Compute the normalized next_f_level_time for each combination comb_i:
    ;;     normalized_next_f_level_time_{comb_i}  = next_f_level_time_{comb_i} * HBF_{comb_i}^(MAX - next_f_boundary_{comb_i})
    ;; 
    ;; 3. Choose combination with less normalized_next_f_level_time.
    (rida-estimation-1
     (let ((next_f_level_time-1 (get-next-f-level-time (meta-node-evaluation-info node1)))
	   (next_f_level_time-2 (get-next-f-level-time (meta-node-evaluation-info node2)))
	   (hbf1 (get-hbf (meta-node-evaluation-info node1)))
	   (hbf2 (get-hbf (meta-node-evaluation-info node2)))
	   (next_f_boundary-1 (get-f-boundary (meta-node-evaluation-info node1)))
	   (next_f_boundary-2 (get-f-boundary (meta-node-evaluation-info node2))))
       (cond ((or (not next_f_level_time-1) (not hbf1) (not next_f_boundary-1))
	      (if (and next_f_level_time-2 hbf2 next_f_boundary-2) nil t))
	     ((or (not next_f_level_time-2) (not hbf2) (not next_f_boundary-2))
	      t)
	     (t (< (* next_f_level_time-1 (expt hbf1 (- max-f-boundary next_f_boundary-1)))
		    (* next_f_level_time-2 (expt hbf2 (- max-f-boundary next_f_boundary-2))))))))
    (otherwise t)))
;;     (rida-estimation-2
;;      ;; 	 argmax f = log(TimeRemaining / avg_eval_per_node) / log(HBF)
;;      ;; DB I think this is the same as min log_HBF avg_eval_per_node
;;      (let ((avg_eval_per_node-1 (cdr (assoc 'avg_total_time_costs_per_generated_node (meta-node-evaluation-info node1))))
;; 	   (avg_eval_per_node-2 (cdr (assoc 'avg_total_time_costs_per_generated_node (meta-node-evaluation-info node2))))
;; 	   (hbf1 (cdr (assoc 'HBF (meta-node-evaluation-info node1))))
;; 	   (hbf2 (cdr (assoc 'HBF (meta-node-evaluation-info node2)))))
;; 
;; ;;        (if *tracep* (format t "~% Evaluations ~,2f ~,2f" (/ (log (/ search-timeout avg_eval_per_node-1)) (log hbf1))  (/ (log (/ search-timeout avg_eval_per_node-2))  (log hbf2))))
;; 
;;        (cond ((or (not avg_eval_per_node-1) (not hbf1))
;; 	      (if (and avg_eval_per_node-2 hbf2) nil t))
;; 	     ((or (not avg_eval_per_node-2) (not hbf2))
;; 	      t)
;; 	     (t (>= (/ (log (/ search-timeout avg_eval_per_node-1))
;; 		       (log hbf1))
;; 		    (/ (log (/ search-timeout avg_eval_per_node-2))
;; 		       (log hbf2)))))))

(defun better-statistics (node1 node2)
  (let* ((evaluation-info-1 (meta-node-evaluation-info node1))
	 (evaluation-info-2 (meta-node-evaluation-info node2))
	 (avg_eval_per_node-1 (get-time-per-node evaluation-info-1))
	 (avg_eval_per_node-2 (get-time-per-node evaluation-info-2))
	 (flevels-1 (mapcan #'f-level evaluation-info-1))
	 (flevels-2 (mapcan #'f-level evaluation-info-2))
	 (common-f-levels (intersection flevels-1 flevels-2))
	 (max-common-f-level (if common-f-levels (apply #'max common-f-levels)))
	 (last-f-level-1 (if flevels-1 (apply #'max flevels-1)))
	 (last-f-level-2 (if flevels-2 (apply #'max flevels-2)))
	 (num-nodes-closed-1 (num-nodes-closed max-common-f-level last-f-level-1 evaluation-info-1))
	 (num-nodes-closed-2 (num-nodes-closed max-common-f-level last-f-level-2 evaluation-info-2))
	 (better-n1n2-reason1 (and evaluation-info-1 evaluation-info-2 max-common-f-level
				   (numberp avg_eval_per_node-1) (numberp avg_eval_per_node-2)
				   (numberp num-nodes-closed-1) (numberp num-nodes-closed-2)
				   (< avg_eval_per_node-1 avg_eval_per_node-2)
				   (< num-nodes-closed-1 num-nodes-closed-2)))
	 (better-n2n1-reason1 (and evaluation-info-1 evaluation-info-2 max-common-f-level
				   (numberp avg_eval_per_node-1) (numberp avg_eval_per_node-2)
				   (numberp num-nodes-closed-1) (numberp num-nodes-closed-2)
				   (< avg_eval_per_node-2 avg_eval_per_node-1)
				   (< num-nodes-closed-2 num-nodes-closed-1)))
	 (better-n1n2-reason2 (and evaluation-info-1 evaluation-info-2 (not max-common-f-level)
				   last-f-level-1 last-f-level-2
				   (> last-f-level-1 last-f-level-2)))
	 (better-n2n1-reason2 (and evaluation-info-1 evaluation-info-2 (not max-common-f-level)
				   last-f-level-1 last-f-level-2
				   (> last-f-level-2 last-f-level-1)))
	 (better-n1n2-reason3 (> (meta-node-evaluation node1) (meta-node-evaluation node2))))
    (if *tracep*
	(cond (better-n1n2-reason1 (format t "~2%~a~% chosen over~% ~a~% for reason1" node1 node2))
	      (better-n1n2-reason2 (format t "~2%~a~% chosen over~% ~a~% for reason2" node1 node2))
	      ((and better-n1n2-reason3 (not better-n2n1-reason1) (not better-n2n1-reason2))     
	       (format t "~2%~a~% chosen over~% ~a~% for reason3" node1 node2))
	      ((and evaluation-info-1 (not evaluation-info-2))
	       (format t "~2%~a~% chosen over~% ~a~% because lack of evaluation-info-2" node1 node2))
	      (t (format t "~2%~a~% not chosen over~% ~a" node1 node2))))
    (if (and evaluation-info-1 evaluation-info-2)
	(or better-n1n2-reason1
	    better-n1n2-reason2
	    (and better-n1n2-reason3 (not better-n2n1-reason1) (not better-n2n1-reason2)))
	(and evaluation-info-1 (not evaluation-info-2)))))

(defun f-level (info)
  (if (eq (car info) 'f_bound)
      (list (cadr info))))

(defun num-nodes-closed (max-common-f-level last-f-level evaluation-info)
  (if max-common-f-level
      (or (get-closed (cdr (member (list 'f_bound max-common-f-level)
				   evaluation-info
				   :test #'equal)))
	  ;; in case there is no data on the last flevel, we use the previous one
	  (get-closed (cdr (member (list 'f_bound max-common-f-level)
				   (reverse evaluation-info)
				   :test #'equal))))))

;; DB. TODO. I might have to ms-operator the next_f_boundary for another value
(defun sort-by-rida (nodes evaluation-fn search-timeout)
  (let ((max-f-boundary (if (eq evaluation-fn 'rida-estimation-1)
			    (apply #'max (mapcar #'(lambda (node)
						     (or (get-f-boundary (meta-node-evaluation-info node)) 0))
						 nodes)))))
    (sort nodes
	  #'(lambda (node1 node2)
	      (better-node-p node1 node2 evaluation-fn search-timeout max-f-boundary)))))

(defun update-operators (domain domain-file domain-dir increasing-bags-p)
  (let* ((domain-def (cdr (read-all-file (concatenate 'string domain-dir domain-file))))
	 (types-def (process-types (find-argument domain-def :types)))
	 (baggy-types (read-all-file (concatenate 'string domain-dir "bagged-types.lisp")))
	 (all-types (if baggy-types (sub-types '(object) types-def)))
	 (dont-types nil)
	 (new-ms-operators nil))
    (when (> (length baggy-types) 1)
      (setq baggy-types (mapcar #'car (sort baggy-types #'> :key #'cdr)))
      (dolist (type baggy-types)
	(setq dont-types (if increasing-bags-p
			     (remove type all-types)
			     (cons type (set-difference all-types baggy-types))))
	(if *tracep* (format t "~2%  Generating meta-search operators for not bagging ~a" dont-types))
	(dolist (version (generate-versions (list 'bagging
						  (if increasing-bags-p
						      type
						      (intern (format nil "~:@(not-~a~)" type)))
						  (create-baggy-type-function dont-types))))
	  (push version new-ms-operators)))
      (setq *ms-operators* (append *ms-operators* new-ms-operators)))
    (if *tracep* (format t "~2%  New meta-search operators: ~a" *ms-operators*))))

;; I will assume that read-rida-results returns a cons of a number and a list of heuristics selected by rida
;; TBD: which preprocess and translate to use if we have arrived at a given node that uses different preprocess and
;; translate?
;; ./rida-sampling.sh ../RIDA/domains-test-raquel/Tetris/domain.pddl
;; ../RIDA/domains-test-raquel/Tetris/p01-6.pddl  ../RIDA ./mi-mappings.txt
;; ./results rida-solving.sh -nojordan -alvaro

;; ./rida-sampling <domain-file> <problem-file>  <rida-path>
;; <mappings-input-file> <results-path>
;; <file-rida-solving-output-file-name> <translate-option (optional,
;; values: -jordan, -nojordan, default: -nojordan)> <preprocess-option
;; optional, values: -alvaro -noalvaro, default:-alvaro)>"

;; DB we would probably have to give heuristics as input
(defun rida-estimation (node)
  (let* ((domain (meta-node-domain node))
	 (domain-file (meta-node-domain-file node))
	 (problem-file (meta-node-problem-file node))
	 (domain-dir (meta-node-domain-dir node))
	 (probsets-dir (meta-node-probsets-dir node))
	 (domain-path (concatenate 'string domain-dir domain-file))
	 (problem-path (concatenate 'string probsets-dir problem-file))
	 (last-state (car (meta-node-state node)))
	 (rida-solving-heuristics-file (concatenate 'string (generate-node-name node) "-" *rida-heuristics-file-postfix*))
 	 (rida-plan-file (concatenate 'string (generate-node-name node) "-" *solution-file-postfix*))
  	 (rida-results-file (concatenate 'string (generate-node-name node) "-" *results-file*))
	 (result-dir *result-dir*)
	 (initial-time (get-internal-real-time))
	 (command  (concatenate 'string "bash "
			       ;; (format nil "ulimit -v ~a; " *memory-bound*)
			       ;; rida sampling script
				*path-rida-sampling* "rida-sampling.sh "
				;; domain file 
				domain-path " "
				;; problem file
				problem-path " "
			        ;; rida-path	
			        *path-rida* " "
                                ;; mappings-input-file
				domain-dir "mappings-" (pathname-name problem-file) ".txt "
				;; results path
				domain-dir result-dir " "
				;; rida-predictions-file (file name, no path)
				rida-results-file " "
				;; rida-solving-selected-heuristics-file (file name, no path)
				rida-solving-heuristics-file " "
				;; rida-plan-file (file name, no path)
				rida-plan-file " "
				;; rida-time-bound
				(write-to-string (floor *estimation-timeout*)) " "
				;; translate option 
				(cond ((member 'fd-jordan last-state) " -fd-jordan")
				      ((member 'symba-jordan last-state) " -symba-jordan")
				      ((member 'symba-nojordan last-state) " -symba-nojordan")
				      ;; fd-nojordan
				      (t " -fd-nojordan"))
				;; preprocess option
				(if (member 'vidal-alvaro last-state) " -alvaro" " -noalvaro")
				" &> " domain-dir result-dir  (generate-node-name node) ".log"
				)))
    ;; 	 (command-move (concatenate 'string "mv reformulated-" domain-file " " domain-dir new-domain-file
    ;; 				    " ; mv ReformCommandLine1.txt bagged-types.txt mappings.txt original-" domain-file " " domain-dir
    ;; 				    " ; mv reformulated-" problem-file " " probsets-dir new-problem-file
    ;; 				    " ; mv original-" problem-file " " probsets-dir)))
    (if *tracep* (format t "~2%  Running command: ~a" command))
    (ensure-directories-exist (concatenate 'string domain-dir result-dir))
    (execute-shell-command command)
    (setf (meta-node-sampling-time node) (elapsed-time initial-time 'real-time))
    (if (probe-file (concatenate 'string domain-dir result-dir rida-plan-file))
	(setf (meta-node-sampling-plan-p node) t))
    (setf (meta-node-plan-file node) (concatenate 'string domain-dir result-dir rida-plan-file))
    (setf (meta-node-rida-heuristics node) (concatenate 'string domain-dir result-dir rida-solving-heuristics-file))
    (setf (meta-node-evaluation-info node)
	  (read-all-file (concatenate 'string domain-dir result-dir rida-results-file) t))))

(defun generate-node-name (node)
  (if node (concatenate 'string "n" (format nil "~d-" (meta-node-id node)) (pathname-name *original-problem-file*) "-"
			(generate-combination-name node))
      "none"))
  

(defun generate-combination-name (node)
  (funcall #'from-list-to-string
	   (mapcan #'(lambda (state)
		       (mapcar #'(lambda (label)
				   (let ((label-string (format nil "~a" label)))
				     (format nil "~a~a" (elt label-string 0)
					     (elt label-string (1+ (or (search "-" label-string) 0))))))
			       (butlast state)))
		   (reverse (meta-node-state node)))))


;; planner: lama-first, symba, fd-opt
;; I use estimation-time/2 for max time limit for ipdb generation
(defun fd-estimation (node planner)
  (let* ((domain (meta-node-domain node))
	 (domain-file (meta-node-domain-file node))
	 (problem-file (meta-node-problem-file node))
	 (domain-dir (meta-node-domain-dir node))
	 (probsets-dir (meta-node-probsets-dir node))
	 (result-dir *result-dir*)
 	 (plan-file (concatenate 'string domain-dir result-dir (generate-node-name node) "-" *solution-file-postfix*))
	 (fd-estimation-file *fd-estimation-file*)
  	 (results-file (concatenate 'string (generate-node-name node) "-" *results-file*))
	 (aux-file (format nil "~a~aoutput-~a-aux.lisp" domain-dir result-dir planner))
	 (initial-time (get-internal-real-time))
	 (state (meta-node-state node))
	 (heuristics (intersection *heuristics* (car state)))
	 (pre-processor (intersection *pre-processors* (car state)))
	 (solution nil)
	 (evaluation nil)
	 (command nil))
    (cond ((and (probe-file (concatenate 'string domain-dir domain-file))
		(probe-file (concatenate 'string probsets-dir problem-file)))
	   (setq command (concatenate 'string "\rm " aux-file))
	   (if (eq planner 'fd-opt) (setq command (concatenate 'string command " " fd-estimation-file)))
	   (if *tracep* (format t "~2%  Running command: ~a" command))
	   (execute-shell-command command)
	   (setq solution (the-ring planner *estimation-timeout* domain domain-file problem-file
				    :domain-directory  domain-dir :probsets-dir probsets-dir
				    :plan-file plan-file
				    :search-options (generate-search-options heuristics pre-processor (/ *estimation-timeout* 2)))))
	  (*tracep* (format t "~2%Domain ~a or problem ~a files not found"
			    (concatenate 'string domain-dir domain-file)
			    (concatenate 'string probsets-dir problem-file)))
	  (t nil))
    (if *tracep*
	(format t "~2%   Evaluation of domain ~a~%     domain-file: ~a~%     problem-file: ~a~%     result: found ~a, length ~d, time ~,2f" 
		domain domain-file problem-file (and solution (solution-found solution))
		(and solution (solution-length solution)) (and solution (solution-total-time solution))))
    (case planner
      (lama-first
       (setf (meta-node-sampling-time node) (elapsed-time initial-time 'real-time))
       (setf (meta-node-evaluation-info node)
	     (list (cons 'time-to-solve
			 (if (and (solution-p solution) (solution-found solution))
			     ;; probably, we would have to store the plan in the corresponding attributes in the case of using it
			     ;; for satisficing planning
			     (solution-total-time solution)
			     most-positive-fixnum)))))
      ((symba fd-opt)
       (cond ((and solution (solution-p solution) (solution-found solution) (probe-file plan-file))
	      (setf (meta-node-sampling-plan-p node) t)
	      (setf (meta-node-plan-file node) plan-file)
	      (setf (meta-node-sampling-time node) (elapsed-time initial-time 'real-time))
	      (setf (meta-node-evaluation-info node) (list (cons 'time-to-solve (solution-total-time solution)))))
	     (t (ensure-directories-exist (concatenate 'string domain-dir result-dir))
		(setq command (if (eq planner 'symba)
				  (concatenate 'string "\rm " results-file "; " 
					       *parse-symba-estimation* aux-file " "
					       domain-dir result-dir results-file)
				  (concatenate 'string "mv " fd-estimation-file " " 
					       domain-dir result-dir results-file)))
		(if *tracep* (format t "~2%  Running command: ~a" command))
		(execute-shell-command command)
		(setq evaluation (read-all-file (concatenate 'string domain-dir result-dir results-file) t))
		(setf (meta-node-sampling-time node) (elapsed-time initial-time 'real-time))
		(setf (meta-node-evaluation-info node) evaluation))))
      (otherwise nil))))
;;  (if (eq planner 'fd-opt)
;; 				(concatenate 'string "\rm " results-file "; cd " *my-planning-path*
;; 					     "new-fd/downward/; ./parse-fd-opt-estimation.sh " aux-file " "
;; 					     domain-dir result-dir results-file)
;; 				  (concatenate 'string "\rm " results-file "; cd "
;; 					       *path-symba* "; ./parse-symba-estimation.sh " aux-file " "
;; 					       domain-dir result-dir results-file)))
				  ;; RF: now files are in the metasearch directory. Do not make cd

(defun generate-search-options (heuristics pre-processor timeout)
  (let ((search-options nil))
    (if heuristics
	(setq search-options `((heuristics ,@heuristics) (ipdb-timelimit . ,timeout))))
    (if (not (eq pre-processor 'fd))
	(push `(pre-processor . ,pre-processor) search-options))
    search-options))

(defun ff-estimation (node)
  (let* ((domain (meta-node-domain node))
	 (domain-file (meta-node-domain-file node))
	 (problem-file (meta-node-problem-file node)))
    (say-domain domain domain-file)
    (prob problem-file)
    (set-duplicate-hashing)
    (compute-heuristic (problem-lit-goals *current-problem*))))

;; for now, something simple
(defun done (node initial-time timeout)
  (or (meta-node-sampling-plan-p node)
      (>= (elapsed-time initial-time 'real-time) timeout)
      (>= (meta-node-depth node) *max-depth*)))

(defun meta-node-print (node stream z)
  (declare (ignore z))
  (format stream "<META-NODE: ~a, ~,2f>" (meta-node-state node) (meta-node-evaluation node)))

(defun rida (node domain original-domain-file original-problem-file domain-file problem-file probsets-dir domain-dir timeout)
  (let* ((initial-time (get-internal-real-time))
	 (domain-path (concatenate 'string domain-dir domain-file))
	 (problem-path (concatenate 'string probsets-dir problem-file))
	 (state (meta-node-state node))
	 (last-state (and node (car state)))
	 (rida-solving-heuristics-file (and node (meta-node-rida-heuristics node)))
 	 (rida-plan-file (and node (meta-node-plan-file node)))
	 (result-dir *result-dir*)
	 (plan nil)
;; 	 (command (if rida-solving-heuristics-file
;; 		      ;; DB: we would probably have to change this if using another estimation
;; 		      (concatenate 'string "sh ./downward-1 --Phase \"SOLVING\" --search \"astar(lmcut())\" &> output.log")
;; 		      (concatenate 'string "sh "
;; 				   ;;(format nil "ulimit -v ~a -t ~a; " *memory-bound* (round timeout))
;; 				   ;; rida sampling script
;; 				   rida-solving-heuristics-file " " (write-to-string (floor timeout))
;; 				   " &> " rida-solving-heuristics-file ".log" )))

	 (command   (concatenate 'string "sh "
				   ;;(format nil "ulimit -v ~a -t ~a; " *memory-bound* (round timeout))
				   ;;rida sampling script
				   rida-solving-heuristics-file " " (write-to-string (floor timeout))
				   " &> " rida-solving-heuristics-file ".log" )))
    (if *tracep* (format t "~2%  Running command: ~a" command))
    (execute-shell-command command)
    (translate-solution-meta-search node)
    (if *tracep* (format t "~%  plan file: ~a" (meta-node-plan-file node)))
    (cond  ((and rida-plan-file (probe-file rida-plan-file))
    	    (setq plan (get-solution-from-file 'rida (meta-node-plan-file node)))
	    (when *tracep*
	      (format t "~2% Plan:")
	      (pp-list plan))
	    (make-solution :found T
			   :total-time (elapsed-time initial-time 'real-time)
			   :length (length plan)
			   :path plan
			   :num-nodes 0
			   :total-cost (length plan)))
	   (t (make-solution :found nil
			     :total-time (elapsed-time initial-time 'real-time)
			     :length 0
			     :path nil
			     :num-nodes 0
			     :total-cost 0)))))

;; (defun rida (node domain original-domain-file original-problem-file domain-file problem-file probsets-dir domain-dir
;; 		  timeout translate-back-p)
;;   (let* ((initial-time (get-internal-real-time))
;; 	 (domain-path (concatenate 'string domain-dir domain-file))
;; 	 (problem-path (concatenate 'string probsets-dir problem-file))
;; 	 (state (meta-node-state node))
;; 	 (last-state (and node (car state)))
;; 	 (rida-solving-heuristics-file (and node (meta-node-rida-heuristics node)))
;;  	 (rida-plan-file (and node (meta-node-plan-file node)))
;; 	 (result-dir *result-dir*)
;; 	 (plan nil)
;; 	 (command (concatenate 'string "sh "
;; 			       (format nil "ulimit -v ~a -t ~a; " *memory-bound* (round timeout))
;; 			       ;; rida sampling script
;; 			       rida-solving-heuristics-file
;;                                " &> " rida-solving-heuristics-file ".log" ))
;; 	 (translate-back-command (if (and (assoc 'bagging state) translate-back-p)
;; 				     (create-translate-back-command original-domain-file original-problem-file
;; 								    domain-dir probsets-dir
;; 								    rida-plan-file
;; 								    (concatenate 'string rida-solving-heuristics-file
;; 										 ".mappings")))))
;;     (if *tracep* (format t "~%  Running command: ~a" command))
;;     (execute-shell-command command)
;;     (when translate-back-command
;;       (if *tracep* (format t "~%  Running command: ~a" translate-back-command))
;;       (execute-shell-command translate-back-command)
;;       ;; if translated, the translated plan is called rida-plan-file".translated"
;;       (setf (meta-node-plan-file node) (concatenate 'string rida-plan-file ".translated")))
;;     (if *tracep* (format t "~%  plan file: ~a" (meta-node-plan-file node)))
;;     (cond  ((probe-file (meta-node-plan-file node))
;;     	    (setq plan (get-solution-from-file 'rida (meta-node-plan-file node)))
;; 	    (make-solution :found T
;; 			   :total-time (elapsed-time initial-time 'real-time)
;; 			   :length (length plan)
;; 			   :path plan
;; 			   :num-nodes 0
;; 			   :total-cost (length plan)))
;; 	   (t (make-solution :found nil
;; 			     :total-time (elapsed-time initial-time 'real-time)
;; 			     :length 0
;; 			     :path nil
;; 			     :num-nodes 0
;; 			     :total-cost 0)))))

(defun translate-solution-meta-search (node)
  (do* ((ms-operators (meta-node-state node) (cdr ms-operators))
	(ancestor node (meta-node-parent ancestor))
	(translated-p nil)
	(plan-file (meta-node-plan-file node))
;; 	(new-plan-file (concatenate 'string plan-file ".translated"))
;; 	(plan-file-to-use plan-file new-plan-file)
	(rida-solving-heuristics-file (meta-node-rida-heuristics node)
				      (and ancestor (meta-node-rida-heuristics ancestor)))
	(domain-file (meta-node-domain-file *root-node*))
	(problem-file (meta-node-problem-file *root-node*))
	(domain-dir (meta-node-domain-dir *root-node*))
	(mappings-file (if (probe-file (concatenate 'string rida-solving-heuristics-file ".mappings"))
			   (concatenate 'string rida-solving-heuristics-file ".mappings")
			   (concatenate 'string domain-dir "mappings-" (pathname-name (meta-node-problem-file node)) ".txt")))
	(probsets-dir (meta-node-probsets-dir *root-node*))
	(plan-p (and plan-file (probe-file plan-file)))
	(translate-back-command nil)
	(translation-success-p nil)
	(plan nil)
	(command nil))
      ((or (not plan-p) (null ms-operators) (not ancestor)) ;;  (member 'original (car ms-operators))
       plan-file)
    (setq translated-p t)
    (cond ((member 'alphabetical (car ms-operators))
	   (setq plan (get-solution-from-file 'rida plan-file))
	   (setq command (concatenate 'string "cp " plan-file " " plan-file ".raw.alphabetical"))
	   (execute-shell-command command)
	   (when *tracep*
	     (format t "~% Plan to be translated back from alphabetical:")
	     (pp-list plan))
	   (with-open-file (ostream plan-file :direction :output :if-exists :supersede :if-does-not-exist
				    :create)
	     (when *tracep*
	       (format t "~% Plan translated:")
	       (pp-list (sublis (meta-node-substitution ancestor) plan)))
	     (say-pp-solution (sublis (meta-node-substitution ancestor) plan)
			      nil ostream nil)))
	  ((member 'bagging (car ms-operators))
	   (setq translate-back-command
		 (create-translate-back-command domain-file problem-file
						domain-dir probsets-dir
						plan-file
						mappings-file))
	   (if *tracep* (format t "~2%  Running command: ~a" translate-back-command))
	   (execute-shell-command translate-back-command)
	   ;; if translated, the translated plan is called plan-file".translated"
	   (if (and plan-file (setq translation-success-p (probe-file plan-file)))
	       (setf (meta-node-plan-file node) plan-file))
	   ;; 		 (setq plan-file (meta-node-plan-file node))
	   (when *tracep*
	     (if translation-success-p
		 (format t "~2% Plan translated back from bagging:")
		 (format t "~2% Plan not translated back from bagging (reformulated maintained):"))
	     (pp-list (get-solution-from-file 'rida plan-file))))
	  (t nil))))

;;./baggy.py domain.pddl(original) prob.pddl(original)  --solution planfile --mapping mappings.txt --writeout_reformulation_logic  > ReformCommandLine3.txt
;; the translated solution will be in file translated-planfile
(defun create-translate-back-command (domain-file problem-file domain-dir probsets-dir plan-file mappings-file)
  (let* ((domain-path (concatenate 'string domain-dir domain-file))
	 (problem-path (concatenate 'string probsets-dir problem-file))
	 
	 ;; command copy-inputs
	 ;; cp plan-file ./planfile
	 ;; cp mappings-file ./mappings.txt
	 (command-copy-inputs (concatenate 'string
					   "cp " plan-file " planfile ; "
					   "cp " mappings-file " mappings-baggy-translate.txt"))
	 (command (concatenate
		   'string
		   (format nil "ulimit -Sv ~a; " *memory-bound*)
                   ;;(format nil "./timeout -m ~a; " *memory-bound*)
		   #+linux		   "python3 "
		   #+(or macos darwin)	   "/opt/local/bin/python3 "
		   *path-baggy* "baggy.py " domain-path " " problem-path
		   " --solution  planfile"
		   " --mappings  mappings-baggy-translate.txt"
		   " --writeout_reformulation_logic"
		   " &> translate-back-plan.log"))

	 ;; command move
	 ;; mv translated-planfile planfile.translatedback
	 ;; RF TODO: inputs should be also removed....
	 (command-move (concatenate 'string
				    " cp " plan-file " " plan-file ".raw.bagging"
				    " ; mv translated-planfile " plan-file
				    " ; mv translate-back-plan.log " plan-file ".translation.log"
				    " ; rm planfile mappings-baggy-translate.txt")))
    (concatenate 'string command-copy-inputs " ; " command " ; " command-move)))

;; translates back a solution that comes from removing conditional effects
(defun translate-back-conditional-effects (plan-file solution-meta-search domain-dir domain-file probsets-dir problem-file)
  (let* ((plan-file-dir (build-domains-dir plan-file))
	 (plan-file-name (if (pathname-type plan-file)
				    (concatenate 'string (pathname-name plan-file) "." (pathname-type plan-file))
				    (concatenate 'string (pathname-name plan-file))))
	 (plan-file-name-no-translated (concatenate 'string "NoCEtranslated" plan-file-name))
	 (command (concatenate 'string

			       "cp " plan-file-dir plan-file-name " " plan-file-name-no-translated ";"

			       "python " *path-remove-conditional-effects* "clean_plans.py " plan-file-dir
			       plan-file-name-no-translated plan-file-name " "
			       (concatenate 'string domain-dir "WithCE-" domain-file) " "
       			       (concatenate 'string probsets-dir "WithCE-" problem-file))))
    (if *tracep* (format t "~2%  Running command: ~a" command))
    (execute-shell-command command)
    solution-meta-search))

(defun restore-original-domain-problem-conditional-effects (domain-dir domain-file probsets-dir problem-file)
  (let* ((command (concatenate 'string
			"cp "
			(concatenate 'string domain-dir domain-file) " " (concatenate 'string domain-dir "WithoutCE-" domain-file) 
			"; cp "
			(concatenate 'string probsets-dir problem-file) " " (concatenate 'string probsets-dir "WithoutCE-" problem-file)
			"; mv "
			(concatenate 'string domain-dir "WithCE-" domain-file) " " (concatenate 'string domain-dir domain-file)
			"; mv "
			(concatenate 'string probsets-dir "WithCE-" problem-file) " " (concatenate 'string probsets-dir problem-file))))
			
    (if *tracep* (format t "~2%  Running command: ~a" command))
    (execute-shell-command command)))

;; it reintroduces the arguments of the action that are separated by _---_ by the conditional effects code
;; it also removes actions containing condef substring, since they are artificially included by that code
(defun translate-solution-conditional-effects (solution)
  (mapcan #'(lambda (action)
	      (if (= (length action) 1)
		  (let ((name-string (format nil "(~a)" (car action))))
		    (if (not (or (search "CONDEF" name-string) (search "condef" name-string)))
			(list (original-action name-string))))
		  (list action)))
	  solution))

(defun original-action (name)
  (let* ((position (search "-___-" name)))
    (read-from-string (replace-all (if position (concatenate 'string (subseq name 0 position) ")") name) "_---_" " "))))
;; (defun original-action (name)
;;   (read-from-string (replace-all (format nil "(~a)" name) "_---_" " ")))

;; (defun remove-garbage-condeffs (string)
;;   (let ((new (subseq string 0 (search "-CONDEFF" string))))
;;     (subseq new 0 (search "-ENDOF" new))))


;; from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;

;; TBD: which files to copy from where to where when already-bagged-p=t
;; RF: when already-bagged-p= t domain-file and problem file contain the names for the bagged domain/problem that have to be copied into the new domain and new problem files. Otherwise they contain the domain/problem from where bagging should be applied
(defun baggy-script (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file
		     &optional (dont-types nil) (already-bagged-p nil))
  (cond (*can-bag-p*
	 (let* ((domain-path (concatenate 'string domain-dir domain-file))
		(problem-path (concatenate 'string probsets-dir problem-file))
		(dont-types-string (create-dont-bag-string dont-types))
		;;	 (command (concatenate 'string "/opt/local/bin/python3 " *path-baggy* "baggy.py " domain-path " " problem-path
		(command (concatenate
			  'string
			  (format nil "ulimit -Sv ~a; " *memory-bound*)
			  ;;(format nil "./timeout -m ~a; " *memory-bound*)
			  #+linux		   "python3 "
			  #+(or macos darwin)	   "/opt/local/bin/python3 "
			  *path-baggy* "baggy.py " domain-path " " problem-path
			  " --writeout_reformulation_logic --writeout_reformulated_pddl  --ground_operators --enable_pddl_repair"
			  dont-types-string
			  " &> baggy.log"))
		(new-mapping-file (concatenate 'string domain-dir "mappings-" (pathname-name new-problem-file) ".txt"))
		(prev-mapping-file (concatenate 'string domain-dir "mappings-" (pathname-name problem-file) ".txt"))
		(command-move (if already-bagged-p ;; TBD
				  (concatenate 'string
 					       " cp " domain-dir domain-file " " domain-dir new-domain-file
 					       " ; cp " domain-dir (pathname-name problem-file) ".baggy.log "  domain-dir (pathname-name new-problem-file) ".baggy.log"
 					       " ; cp " prev-mapping-file " " new-mapping-file
 					       " ; cp " probsets-dir problem-file " " probsets-dir new-problem-file)
				  (concatenate 'string
					       "\\rm " domain-dir new-domain-file " "
					       domain-dir "bagged-types.lisp" " " domain-dir "mappings.txt"
					       " ; \\rm " probsets-dir new-problem-file
					       " ; mv reformulated-" domain-file " " domain-dir new-domain-file
					       " ; mv bagged-types.lisp original-" domain-file " " domain-dir
					       " ; mv baggy.log " domain-dir (pathname-name new-problem-file) ".baggy.log"
					       " ; mv mappings.txt " new-mapping-file
					       " ; mv reformulated-" problem-file " " probsets-dir new-problem-file
					       " ; mv original-" problem-file " " probsets-dir)))
		(can-bag-p t))
	   (unless already-bagged-p
	     (if *tracep* (format t "~2%  Running command: ~a" command))
	     (execute-shell-command command))
	   ;; careful since those two output files are in the directory where baggy was called from
	   (if *tracep* (format t "~2% Running command move (already bagged: ~a): ~a" already-bagged-p command-move))
	   (execute-shell-command command-move)
	   (when (and (not already-bagged-p) (problem-with-bagging-p new-mapping-file))
	     (setq can-bag-p nil)
	     (setf *can-bag-p* nil)
	     (if *tracep* (format t "~2%  Could not bag")))
	   (list new-domain-file new-problem-file nil can-bag-p)))
	(t (list domain-file problem-file nil nil))))

(defun problem-with-bagging-p (mapping-file)
  (not (probe-file mapping-file)))

(defun copy-bagging-information (first-baggy-child child domain problem-file domain-dir probsets-dir new-domain-file new-problem-file)
  (setf (meta-node-domain-file child) (meta-node-domain-file first-baggy-child)) 
  (setf (meta-node-problem-file child) (meta-node-problem-file first-baggy-child))
  (setf (meta-node-substitution child) (meta-node-substitution first-baggy-child))
  (setf (meta-node-baggable-p child) (meta-node-baggable-p first-baggy-child))
  ;;  (baggy-script domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file nil t))
  (baggy-script domain (meta-node-problem-file child) (meta-node-domain-file child) domain-dir probsets-dir new-domain-file new-problem-file nil t))

(defun create-baggy-type-function (dont-types)
  #'(lambda (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file)
      (baggy-script domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file dont-types)))
;;       (let* ((domain-path (concatenate 'string domain-dir domain-file))
;; 	     (problem-path (concatenate 'string probsets-dir problem-file))
;; 	     (dont-types-string (create-dont-bag-string dont-types))
;; 	     ;;(command (concatenate 'string "/opt/local/bin/python3 " *path-baggy* "baggy.py " domain-path " " problem-path
;; 	     (command (concatenate
;; 		       'string
;;        		       (format nil "ulimit -Sv ~a; " *memory-bound*)
;; 		       ;;(format nil "./timeout -m ~a; " *memory-bound*)
;; 		       "python3 " *path-baggy* "baggy.py " domain-path " " problem-path
;; 		       " --writeout_reformulation_logic --writeout_reformulated_pddl  --ground_operators --enable_pddl_repair "
;; 				   dont-types-string " > ReformCommandLine1.txt"))
;; 	     (command-move (concatenate 'string "mv reformulated-" domain-file " " domain-dir new-domain-file
;; 					" ; mv ReformCommandLine1.txt bagged-types.txt original-" domain-file " " domain-dir
;; 					" ; mv mappings.txt " domain-dir "mappings-" (pathname-name new-problem-file)
;; 					".txt"
;; 					" ; mv reformulated-" problem-file " " probsets-dir new-problem-file
;; 					" ; mv original-" problem-file " " probsets-dir)))
;; 	(if *tracep* (format t "~2%  Running command: ~a" command))
;; 	(execute-shell-command command)
;; 	;; careful since those two output files are in the directory where baggy was called from
;; 	(execute-shell-command command-move)
;; 	(list new-domain-file new-problem-file))))

(defun create-dont-bag-string (dont-types)
  (if dont-types
      (let ((result (format nil "--dont_bag ~(~a~)" (car dont-types))))
	(dolist (type (cdr dont-types))
	  (setq result (format nil "~a,~(~a~)" result type)))
	result)
      ""))

(defun inverse-script (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file)
  (if *tracep* (format t "~%  Running inverse order"))
  (change-ops-order domain 'inverse :domain-file domain-file :new-domain-file new-domain-file :domain-dir domain-dir)
  (list new-domain-file problem-file nil t))

(defun random-script (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file)
  (if *tracep* (format t "~%  Running random order"))
  (change-ops-order domain 'random :domain-file domain-file :new-domain-file new-domain-file :domain-dir domain-dir)
  (list new-domain-file problem-file nil t))

(defun original-script (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file)
  (if *tracep* (format t "~%  Running original"))
  (list domain-file problem-file nil t))

(defun inverse-alphabetical-script (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file)
  (if *tracep* (format t "~%  Running alphabetical inverse order"))
  (let ((substitution (change-ops-order domain 'alphabetical-inverse :domain-file domain-file
					:new-domain-file new-domain-file :domain-dir domain-dir)))
    (list new-domain-file problem-file substitution t)))

(defun random-alphabetical-script (domain problem-file domain-file domain-dir probsets-dir new-domain-file new-problem-file)
  (if *tracep* (format t "~%  Running alphabetical random order"))
  (let ((substitution (change-ops-order domain 'alphabetical-random :domain-file domain-file
					:new-domain-file new-domain-file :domain-dir domain-dir)))
    (list new-domain-file problem-file substitution t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aux functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; it does not work if args include symbols. It works for numbers and strings
(defun print-alarm (format-string &rest args)
  (declare (special *alarms-file*))
  (with-open-file (stream *alarms-file* :direction :output :if-exists :append :if-does-not-exist :create)
    (eval `(format ,stream ,format-string ,@args))))

;; prints information on selected heuristics, planners, ...
;; validation can be: T/NIL (validated and passed or not) or :not-validated (did not validate the sol)
(defun report-statistics (meta-search-solution-node output-file problem-file new-domain-file new-problem-file
			  solution-meta-search ms-time solution-time use-heuristics-p selected-heuristics planner
			  rida-solving-heuristics-file translator pre-processor validation validation-VAL)
  (let* ((state (meta-node-state meta-search-solution-node))
	 (selected-node-evaluation-time (meta-node-evaluation-time meta-search-solution-node))
	 (baggingp nil)
	 (randomp nil)
	 (alphabetical-inverse-p nil)
;;	 (first-problem-p (not (probe-file output-file)))
 	 (first-problem-p nil)
	 )
    (with-open-file (ostream output-file :direction :output :if-exists :append :if-does-not-exist :create)
      (when first-problem-p
	(format ostream "~%Prob-file New-domain-file New-problem-file MSNode Solution? Validation ValidationVAL Cost MSTime SelEvTime Time Generated Expanded Evaluated")
	(dolist (heuristic *heuristics*) (format ostream " ~a" heuristic))
	(format ostream " LM-Cut-by-default Solved-when-MS Planner Translator Pre-processor Eval-selected Eval-parent")
	(format ostream " Bagging Random Alphabetical-inverse"))
      (format ostream "~%~a ~a ~a ~a ~a ~a ~a ~d ~,2f ~,2f ~,2f ~d ~d ~d"
	      (pathname-name problem-file) (pathname-name new-domain-file) (pathname-name new-problem-file)
	      (generate-combination-name meta-search-solution-node)
	      (and solution-meta-search (solution-found solution-meta-search)) validation validation-VAL
	      (and solution-meta-search (solution-found solution-meta-search) (solution-total-cost solution-meta-search))
	      ms-time selected-node-evaluation-time solution-time *generated* *expanded* *evaluated*)
      (dolist (heuristic *heuristics*)
	(format ostream " ~d"
		(if use-heuristics-p
		    (if (member heuristic selected-heuristics) 1 0)
		    (if (eq planner 'rida)
			(if (lisp-grep rida-solving-heuristics-file (format nil "~(~a~)" heuristic)) 1 0)
			0))))
      (format ostream " ~d ~d"
	      ;; RF info about if lmcut was selected by default. Only when not solved while sampling
	      (if (and (eq planner 'rida)
		       (not (meta-node-sampling-plan-p meta-search-solution-node))
		       (lisp-grep rida-solving-heuristics-file "lmcut default"))
		  1 0)
	      ;; RF solved while sampling
	      (if (meta-node-sampling-plan-p meta-search-solution-node)
		  1 0))
      (format ostream " ~a ~a ~a" planner translator pre-processor)
      (format ostream " ~2,f ~2,f"
	      (meta-node-evaluation meta-search-solution-node)
	      (if (meta-node-parent meta-search-solution-node)
		  (meta-node-evaluation (meta-node-parent meta-search-solution-node))))
      (dolist (node-state state)
	(setq baggingp (or baggingp (member 'bagging node-state)))
	(setq randomp (or randomp (member 'random node-state))))
      (setq alphabetical-inverse-p (alphabetical-inverse-effect-p state nil))
      (format ostream " ~d ~d ~d" (if baggingp 1 0) (if randomp 1 0) (if alphabetical-inverse-p 1 0)))))
;; 	 (last-state (car state))
;;       (format ostream " ~d ~d ~d ~d ~d ~d"
;; 	      (if (member 'fd-jordan last-state) 1 0)
;; 	      (if (member 'fd-nojordan last-state) 1 0)
;; 	      (if (member 'symba-jordan last-state) 1 0)
;; 	      (if (member 'symba-nojordan last-state) 1 0)
;; 	      (if (member 'vidal-alvaro last-state) 1 0)
;; 	      (if (member 'fd last-state) 1 0))

(defun alphabetical-inverse-effect-p (state foundp)
  (if state
      (if (member 'inverse (car state))
	  (if (member 'alphabetical (car state))
	      (if foundp nil (alphabetical-inverse-effect-p (cdr state) t))
	      (alphabetical-inverse-effect-p (cdr state) t))
	  (if (member 'original (car state))
	      (alphabetical-inverse-effect-p (cdr state) foundp)
	      (if (member 'random (car state))
		  nil
		  (if (member 'bagging (car state))
		      foundp
		      t))))
      foundp))

;; RF ... should be in common.lisp??
;; DB I would suggest calling grep. It is way more faster
;; RF ... Ok.. todo :-)
;; DB: I have tried it and it is way faster by using lisp
(defun lisp-grep (file word)
  (if (and file (probe-file file))
      (let ((exists nil))
	(with-open-file (stream file :direction :input)
	  (do ((line (read-line stream nil nil)
		     (read-line stream nil nil)))
	      ((or (null line) exists))
	    (setq exists (search word line))))
	exists)))


(defun validate-VAL (domain-dir domain-file probsets-dir problem-file plan-file)
  (execute-shell-command (concatenate 'string "rm val.output;  VAL-master/validate "
				      (concatenate 'string domain-dir  domain-file) " "
				      (concatenate 'string probsets-dir  problem-file) " "
				      plan-file " &> val.output"))
  (numberp (lisp-grep "val.output" "Plan valid")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Remove conditional effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Version that takes as input the output of removing conditional effects by calling the FF code
(defun remove-conditional-effects-if-any (domain-dir probsets-dir domain-file problem-file)
  (let ((domain-file-path (concatenate 'string domain-dir domain-file))
	(removed-p nil))
    (when (lisp-grep domain-file-path "when")
      (setq removed-p t)
      (let* ((problem-file-path (concatenate 'string probsets-dir problem-file))
	     (command (concatenate 'string
				   ;;(format nil "ulimit -Sv ~a; " *memory-bound*)
				   "(cp " domain-file-path " domain_original_CE.pddl; "
				   "cp " problem-file-path " problem_original_CE.pddl; "
   				   "rm domain_ready.pddl; "
      				   "rm problem_ready.pddl; "
				   "python " *path-remove-conditional-effects* "parse.py domain_original_CE.pddl problem_original_CE.pddl; "
	      
				   "cp " domain-file-path " "
				   (concatenate 'string domain-dir (concatenate 'string "WithCE-"  domain-file)) "; "
				   "cp " problem-file-path " "
				   (concatenate 'string probsets-dir (concatenate 'string "WithCE-" problem-file)) "; "
   				   "rm domain_original_CE.pddl; "
      				   "rm problem_original_CE.pddl; "
				   "cp domain_ready.pddl " domain-file-path "; "
				   "cp problem_ready.pddl " problem-file-path ")"
				   " &> " (concatenate 'string domain-dir *result-dir*
						       "remove-conditional-effects.log"))))
;; 	     (old-actions (give-me-all-actions (cdr (read-all-file domain-file-path)))))
	(if *tracep* (format t "~2%  Running command: ~a" command))
	(execute-shell-command command)
	(write-domain (add-negated-effects-in-preconds (cdr (read-all-file domain-file-path)))
		      domain-dir domain-file)))
;; 	(write-domain (replace-vars-in-actions old-actions (cdr (read-all-file domain-file-path))) domain-dir domain-file)))
    removed-p))

;; it adds to the preconds those del effects that did not appear in the preconds (probably due to removal of conditional
;; effects)
;; I assume actions are completely instantiated and no foralls, exists, ... 
(defun add-negated-effects-in-preconds (domain-def)
  (do* ((the-domain-def domain-def (cdr the-domain-def))
	(domain-element (car the-domain-def) (car the-domain-def))
	(new-actions nil)
	(old-action nil)
	(new-instantiated-action nil)
	(new-domain-def nil)
	(preconds nil)
	(effects nil)
	(dels nil))
       ((null the-domain-def) (append (reverse new-domain-def) (reverse new-actions)))
    (cond ((and (listp domain-element) (eq (car domain-element) :action))
	   (setq preconds (pddl-action-preconditions domain-element))
	   (if (eq (car preconds) 'and) (setq preconds (cdr preconds)))
	   (setq effects (pddl-action-effects domain-element))
	   (dolist (effect effects)
	     (if (and (eq (car effect) 'not)
		      (not (member (cadr effect) preconds :test #'equal))
		      (not (member (list 'not (cadr effect)) preconds :test #'equal)))
		 (push `(or ,(cadr effect) (not ,(cadr effect))) preconds)))
	   (push `(:action ,(pddl-action-name domain-element)
			   :parameters ,(pddl-action-parameters domain-element)
			   :precondition (and ,@preconds)
			   :effects (and ,@effects))
		 new-actions))
	  (t (push domain-element new-domain-def)))))
    
;; solves problem created by removal of conditional effects leaving free vars
(defun replace-vars-in-actions (old-actions domain-def)
  (do* ((the-domain-def domain-def (cdr the-domain-def))
	(domain-element (car the-domain-def) (car the-domain-def))
	(new-actions nil)
	(old-action nil)
	(new-instantiated-action nil)
	(new-domain-def nil))
       ((null the-domain-def) (append (reverse new-domain-def) (reverse new-actions)))
    (cond ((and (listp domain-element) (eq (car domain-element) :action))
	   (setq new-instantiated-action (original-action (cadr domain-element)))
	   (setq old-action (find (car new-instantiated-action) old-actions :key #'cadr))
	   (push (sublis (get-substitution (cdr new-instantiated-action)
					   (cleanup-params (pddl-action-parameters old-action)))
			 domain-element)
		 new-actions))
	  (t (push domain-element new-domain-def)))))

;; ;; this code does not work given that whens come with foralls and it complicates things a lot
;; (defun remove-conditional-effects (domain-dir domain-file)
;;   (let* ((domain-file-path (concatenate 'string domain-dir domain-file))
;; 	 (removed-p nil))
;;     (when (lisp-grep domain-file-path "when")
;;       (setq removed-p t)
;;       (let* ((problem-file-path (concatenate 'string probsets-dir problem-file))
;; 	     (domain-def (cdr (read-all-file domain-file-path)))
;; 	     (actions (give-me-all-actions domain-def))
;; 	     (new-actions (handle-conditional-effects (actions-conditional-effects actions) actions))
;; 	     (command (concatenate 'string "cp " domain-file-path " " (concatenate 'string domain-dir (concatenate 'string "WithCE-"  domain-file)))))
;; 	(if *tracep* (format t "~2%  Running command: ~a" command))
;; 	(execute-shell-command command)
;; 	(write-domain (replace-actions new-actions domain-def) domain-dir domain-file)))
;;     removed-p))
;; 
;; (defun actions-conditional-effects (actions)
;;   (mapcar #'(lambda (action)
;; 	      (cons action (get-whens (pddl-action-effects action))))
;; 	  actions))
;; 				    
;; (defun get-whens (exp)
;;   (case (car exp)
;;     ((and or) (get-whens (cdr exp)))
;;     ((not increase decrease assign) nil)
;;     ((forall exists) (get-whens (caddr exp)))
;;     (when (cdr exp))
;;     (otherwise (if (listp (car exp))
;; 		   (mapcan #'(lambda (subexp)
;; 			       (let ((whens (get-whens subexp)))
;; 				 (if whens (list whens))))
;; 			   exp)))))
;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To create the executable
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun meta-search-from-exe ()
;;   (let ((domain (nth 1 *posix-argv*))
;; 	(domain-file (nth 2 *posix-argv*))
;; 	(problem-file (nth 3 *posix-argv*))
;; ;; 	(evaluation-fn (function (nth 4 *posix-argv*)))
;; 	(timeout (read-from-string (nth 4 *posix-argv*)))
;; 	)
;; 
;;     ;;Setting again global vars to make them relative to the sbcl image at runtime 
;;     (setf *my-path* (namestring (truename "./")))
;;     (setf *my-tmp-path* (concatenate 'string *my-path* "tmp/"))
;; ;;     (setf *my-planning-path* (concatenate 'string *my-path* "software/metasearch/planning/"))
;;     (setf *my-planning-path* (concatenate 'string *my-path* "planning/"))
;;     (setf *domains-dir* (concatenate 'string *my-path* "domains/"))
;;     (setf *path-baggy* (concatenate 'string *my-planning-path* "bagging/translate/"))
;;     (setf *path-rida* (concatenate 'string *my-planning-path* "RIDA/"))
;;     (setf *path-rida-sampling* (concatenate 'string *my-planning-path* ""))
;; ;;     (setf *path-baggy* (concatenate 'string *my-planning-path* "represent-metasearch/software/bagging/translate/"))
;; ;;     (setf *path-rida* (concatenate 'string *my-planning-path* "represent-metasearch/software/RIDA/"))
;; ;;     (setf *path-rida-sampling* (concatenate 'string *my-planning-path* "represent-metasearch/software/metasearch/"))
;; 
;;     (setf *lama-first-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-first.sh "))
;; 
;;     (setf *trace-p* t)
;;     ;;(setf *trace-ho-p* t)
;; 
;;     (format t "~% RUNNING META-SEARCH")
;;     (format t "~% Domains Dir:~a" *domains-dir*)
;;     (format t "~% Domain:~a   Domain-file:~a  Problems:~a" domain domain-file problem-file)
;; ;;     (format t "~% Evaluation function: ~a" evaluation-fn)
;;     (format t "~% Timeout:~a" timeout)
;; 
;;     (meta-search domain problem-file :domain-file domain-file :timeout timeout)
;;     (format t "~%")))
;; 
;; 

;; (meta-search-planner "Tetris" "p01-10.pddl" :evaluation-fn 'rida-estimation-2 :estimation-timeout 100 :domain-dir "/home/raquel/articulos/represent-metasearch/software/metasearch/domains/Tetris/"  :probsets-dir "/home/raquel/articulos/represent-metasearch/software/metasearch/domains/Tetris/") 
(defun meta-search-from-exe ()
  (let (
	(domain (nth 1 *posix-argv*)) ;; not used
	(problem-file (nth 2 *posix-argv*))
	(domain-dir (nth 3 *posix-argv*))	
	(problems-dir (nth 4 *posix-argv*))
;; 	(evaluation-fn (function (nth 4 *posix-argv*)))
	(timeout (read-from-string (nth 5 *posix-argv*)))
	(plan-file (nth 6 *posix-argv*))
	(my-path (nth 7 *posix-argv*)) ;; directory with the code use "./" as default
	)

    ;; change it accordingly
    (setf *release64-p* t)
    ;;(setf *memory-bound* (* 7.8 (expt 2 20)))
    (setf *memory-bound* 8000000)
     ;;Setting again global vars to make them relative to the sbcl image at runtime
    ;; RF: not sure that this is necessary?
    ;;Variables in .init.lisp
    ;;(setf *my-path* (namestring (truename "./")))
    (setf *my-path* (namestring (truename my-path)))    
    ;;(setf *my-path* (namestring (pathname "./")))
    (setf *my-lisp-path* (concatenate 'string *my-path* "mi-software/lisp/"))
    (setf *my-planning-path* (concatenate 'string *my-path* "planning/"))
    ;;(setf *my-tmp-path* (concatenate 'string *my-path* "tmp/"))
    (setf *my-tmp-path* "/tmp/")
    (setf *sayphi-loader* (concatenate 'string *my-planning-path* "sayphi/loader"))
    (setf *ipss-loader* (concatenate 'string *my-planning-path* "ipss/init"))

    (setf *domains-dir* *my-path*)
    (setf *path-baggy* (concatenate 'string *my-planning-path* "bagging/translate/"))
    (setf *path-rida* (concatenate 'string *my-planning-path* "RIDA/"))
    (setf *path-rida-sampling* (concatenate 'string *my-planning-path* ""))
    ;;(setf *lama-first-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-first.sh "))
    (setf *path-symba* (concatenate 'string *my-planning-path* "SYMBA/seq-opt-symba-2/"))
    (setf *symba-script* (concatenate 'string *path-symba* "run-symba.sh "))
;;    (setf *symba-script* (concatenate 'string "cd " *my-planning-path* "SYMBA/seq-opt-symba-2; ./run-symba.sh "))
    (setf *parse-symba-script* (concatenate 'string (format nil "ulimit -Sv ~a; " *memory-bound*) *path-symba* "parse-symba-output.sh "))
    (setf *parse-symba-estimation* (concatenate 'string *path-symba* "parse-symba-estimation.sh "))
    (setf *path-fd* (concatenate 'string *my-planning-path* "new-fd/downward/"))
    (setf *fd-opt-script* (concatenate 'string (format nil "ulimit -Sv ~a; " *memory-bound*)
				       *path-fd* "fast-downward.py " (if *release64-p* "--build release64 " "")))
    (setf *fd-translate-opt-script* (concatenate 'string (format nil "ulimit -Sv ~a; " *memory-bound*)
						 *path-fd* "fast-downward.py --translate " (if *release64-p* "--build release64 " "")))
    ;; it should be a path to an estimation file
    (setf *fd-estimation-file* "estimation.lisp")

    (setf *path-remove-conditional-effects* (concatenate 'string *my-planning-path* "conditionalEffect/"))
    
    (setf *parse-lama-first-script* (concatenate 'string *path-fd* "parse-lama-first-output.sh "))

    ;;(setf *trace-ho-p* t)

    (format t "~% RUNNING META-SEARCH")
    (format t "~% Domains Dir: ~a" domain-dir)
    (format t "~% Problems Dir: ~a" problems-dir)
    (format t "~% Domain: ~a" domain)    
    (format t "~% Problem: ~a" problem-file)
    (format t "~% Plan file: ~a" plan-file)
;;     (format t "~% Evaluation function: ~a" evaluation-fn)
    (format t "~% Timeout: ~a" timeout)

;;  Raquel: this was for the experiments of the random walk   
;;     (setf *random-state* (make-random-state t))
;;     (with-open-file (stream (concatenate 'string domain-dir "random-state-" (pathname-name  problem-file) ".out") 
;; 			    :direction :output :if-exists :supersede :if-does-not-exist :create)
;;       (print *random-state* stream))

    (execute-shell-command (concatenate 'string "rm " plan-file))

;;    (setf *tracep* t)
;;    (trace better-statistics better-node-p ms-enforced-hill-climbing)
;;    (trace timed-execute-shell-command)
;;    (trace execute-shell-command)
;;    (trace the-ring)

    ;;    (trace solution-found)
;;    (trace solution-path)
;;     (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-2 :timeout timeout :estimation-timeout 100
;; 		 :domain-dir domain-dir  :probsets-dir problems-dir :run-original-planner-p t
;; 		 :planners (list 'rida))
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-2 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :meta-search-algorithm #'ms-generate-and-test
;; 		 :planners (list 'rida))

;;     (meta-search-planner domain problem-file :init-state `(original nojordan vidal-alvaro ,#'original-script)
;; 			 :evaluation-fn 'rida-estimation-2 :timeout timeout :estimation-timeout 300
;; 			 :domain-dir domain-dir :probsets-dir problems-dir
;; 		 :planners (list 'rida))
    
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-2 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :run-original-planner-p t
;; 		 :planners (list 'rida))

;;-----------------------------------------------------------------------------------------------------------------------
;; re-run for AAAI final paper
;;   (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-2 :timeout
;;			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;;			problems-dir :ms-operators-translators (list 'fd-jordan 'fd-nojordan)
;; 		 :planners (list 'rida))
;;-----------------------------------------------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------------------------------------------------
;; new run for AAAI final paper random ordering operators
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-2 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir  :ms-operators-translators (list 'fd-jordan 'fd-nojordan)
;; 			:random-ordering-operators-p t
;; 		 :planners (list 'rida))

;;-----------------------------------------------------------------------------------------------------------------------
;; new run for AAAI final paper random walk meta-search algorithm
;;    (meta-search-planner domain problem-file :timeout timeout :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-translators (list 'fd-jordan 'fd-nojordan)
;; 			:meta-search-algorithm #'ms-random-walk
;; 		 :planners (list 'rida))
;;-----------------------------------------------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------------------------------------------------
;;  Meta-search with Symba planner
;;     (meta-search-planner domain problem-file :timeout timeout :domain-dir domain-dir
;; 			 :probsets-dir problems-dir
;; 			 :evaluation-fn 'symba-estimation
;; 			 ;; :estimation-timeout 50
;; 			 :estimation-timeout 100
;; 			 :meta-search-timeout 500
;; 			 :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 			 :ms-operators-translators (list 'symba-nojordan)
;; 			 :ms-operators-pre-processors (list 'vidal-alvaro)
;; 			 :planners (list 'symba) 
;; 			 :init-state (list 'symba 'original 'symba-nojordan 'vidal-alvaro #'original-script)
;; 			 :heuristics NIL
;; 			 )

    ;; ;;  Meta-search with FD and heuristics planner
;;     (meta-search-planner domain problem-file :timeout timeout :domain-dir domain-dir
;; 			 :probsets-dir problems-dir
;; 			 :evaluation-fn 'fd-estimation
;; 			 :estimation-timeout 100
;; 			 :meta-search-timeout 500
;; 			 :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; ;;			 :ms-operators-translators (list 'symba-nojordan)
;; ;;			 :ms-operators-pre-processors (list 'vidal-alvaro)
;; 			 :planners (list 'fd-opt) 
;; 			 :init-state (list 'fd-opt 'original 'fd-nojordan 'vidal-alvaro #'original-script)
;;                       :use-heuristics-p T)
   
    ;; ;;  Meta-search with FD and heuristics planner and symba
    (meta-search-planner domain problem-file :timeout timeout :domain-dir domain-dir
			 :probsets-dir problems-dir
			 :evaluation-fn 'mixed-estimation
 			 :estimation-timeout 75
;; 			 :estimation-timeout 100
 			 :meta-search-timeout 900
;;			 :meta-search-timeout 1800
;			 :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
 			 :ms-operators-keywords (list 'bagging 'original)
			 ;; Raquel, check the right values
;;			 :ms-operators-translators (list 'symba-nojordan)
;;			 :ms-operators-pre-processors (list 'vidal-alvaro)
			 :planners (list 'symba 'fd-opt) 
			 :init-state (list 'symba 'original 'symba-nojordan 'vidal-alvaro #'original-script)
			 :plan-file plan-file
			 :heuristics (list 'potentials 'operatorcounting 'ipdb 'lmcut 'blind)
;; 			 (list 'lmcut 'ipdb 'cegar 'cpdbs 'diverse_potentials 'operatorcounting 'zopdbs)
;; :heuristics (list 'lmcut 'ipdb 'gapdb  'cegar 'cpdbs 'diverse_potentials 'operatorcounting 'zopdbs)
                         :use-heuristics-p T
			 :report-statistics-p NIL
			 :validate-sol-p NIL
			 )
;; 



;;-----------------------------------------------------------------------------------------------------------------------

;; 
;;     
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :pruning-ops-fn #'applicable-ms-operator-not-original-p
;; 		 :planners (list 'rida))
;;-----------------------------------------------------------------------------------------------------------------------
;; estimation-timeout 300 rida-estimation-3
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))
;;-----------------------------------------------------------------------------------------------------------------------
;; estimation-timeout 300 rida-estimation-3 only original bagging AV jordan
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original)
;;                        :ms-operators-translators (list 'fd-jordan)
;; 		       :ms-operators-pre-processors (list 'vidal-alvaro)
;; 		 :planners (list 'rida))
;; 

;;-----------------------------------------------------------------------------------------------------------------------
;;estimation-timeout 300 rida-estimation-4
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-4 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))
;; 

;;-----------------------------------------------------------------------------------------------------------------------
;;estimation-timeout 600 rida-estimation-4
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-4 :timeout
;; 			timeout :estimation-timeout 600 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))

;;estimation-timeout 600 rida-estimation-4
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-4 :timeout
;; 			timeout :estimation-timeout 900 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))


;;-----------------------------------------------------------------------------------------------------------------------
;;estimation-timeout 300 rida-estimation-5
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-5 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))

;;estimation-timeout 300 rida-estimation-6
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-6 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))
;; 

;; estimation-timeout 600 rida-estimation-3
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 600 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))
;; 
;;-----------------------------------------------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------------------------------------------------
;; estimation-timeout 500
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 500 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse)
;; 		 :planners (list 'rida))
;;-----------------------------------------------------------------------------------------------------------------------
;; partial generate&test for evaluating nodes
;; changed:    (defvar *max-depth* 5) -->  (defvar *max-depth* 3)
;;      
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout timeout :estimation-timeout
;; 			300 :domain-dir domain-dir :probsets-dir problems-dir :ms-operators-keywords (list 'bagging
;; 			'original) :ms-operators-translators (list 'fd-jordan
;; 			'fd-nojordan) :ms-operators-pre-processors (list 'vidal-alvaro 'fd) :meta-search-algorithm
;; 			#'ms-generate-and-test :run-planner-p NIL
;; 		 :planners (list 'rida))
;; 
;;    
;;-----------------------------------------------------------------------------------------------------------------------
;; estimation-timeout 300. Change metasearch time. 25% (450s), 75% (1350s)
;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :ms-operators-keywords (list 'bagging 'original 'alphabetical-inverse) :meta-search-timeout 450
;; 		 :planners (list 'rida))
;;-----------------------------------------------------------------------------------------------------------------------


;;    (meta-search-planner domain problem-file :evaluation-fn 'rida-estimation-3 :timeout
;; 			timeout :estimation-timeout 300 :domain-dir domain-dir :probsets-dir
;; 			problems-dir :meta-search-algorithm #'ms-generate-and-test
;; 		 :planners (list 'rida))
;;   run-planner-p que debes poner a nil si no quieres que ejecute los nodos que genera generate-and-test
;; 

    (format t "~%")))
