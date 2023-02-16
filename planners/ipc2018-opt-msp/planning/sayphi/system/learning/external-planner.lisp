(in-package "COMMON-LISP-USER")

(defvar *release64-p* nil "If T, it will use 64bits versions of planners. Now, only FD-Opt")
;; in case this is loaded before meta-search
(defvar *path-fd* (concatenate 'string *my-planning-path* "new-fd/downward/"))
(defvar *path-symba* (concatenate 'string *my-planning-path* "symba/"))

;; Scripts for running external planners
;; These scripts correspond to the latest version of FD code (2017)
(defvar *lama-first-cost-script* (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./fast-downward.py --alias lama-first-cost ")
  "Run lama-first. I am now calling a simplified Lama that only finds the first solution using actions cost")
(defvar *lama-first-script* (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./fast-downward.py --alias lama-first ")
  "Run lama-first. I am now calling a simplified Lama that only finds the first solution using
unit cost. Note, this is different behavior than previous use of Lama first for most of my code, but it is consistent with FD new code.")
(defvar *lama-seq-script* (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./fast-downward.py --alias seq-sat-lama-2011 ")
  "Run winner seq-sat 2011 (lama).")
(defvar *lama-second-script* (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./fast-downward.py --alias lama-second ")
  "Run LAMA starting from the 2nd iteration. It assumes someone has already found a solution, so we provide an upper bound on cost")
(defvar *lama-opt-script* (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./fast-downward.py --alias seq-opt-fdss-1 ")
  "Run winner of seq-opt 2011 (FDSS-1).")
(defvar *fd-opt-script* (concatenate 'string *path-fd* "fast-downward.py " (if *release64-p* "--build release64 " ""))
  "Run optimal FD with an input describing the search algorithm and heuristic to be used.")
(defvar *fd-translate-opt-script* (concatenate 'string *path-fd* "fast-downward.py --translate " (if *release64-p* "--build release64 " ""))
  "Run FD translate.")
;; (defvar *fd-opt-script* (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./fast-downward.py "
;; 				     (if *release64-p* "--build release64 " ""))
;;   "Run optimal FD with an input describing the search algorithm and heuristic to be used.")
;; (defvar *fd-translate-opt-script* (concatenate 'string "cd " *my-planning-path*
;; 					       "new-fd/downward/;  ./fast-downward.py --translate "
;; 					       (if *release64-p* "--build release64 " ""))
;;   "Run optimal FD with an input describing the search algorithm and heuristic to be used.")
(defvar *default-opt-heuristic* "lmcut" "Default heuristic for optimal FD planning")
(defvar *h2-preprocessor-path*
  (concatenate 'string *path-fd* "src/translate/h2-fd-preprocessor/builds/"
	       (if *release64-p* "release64" "release32") "/bin/preprocess"))
;; (defvar *h2-preprocessor-path*
;;   (concatenate 'string *my-planning-path* "new-fd/downward/src/translate/h2-fd-preprocessor/builds/"
;; 	       (if *release64-p* "release64" "release32")
;; 	       "/bin/preprocess"))

(defvar *symba-script* (concatenate 'string "cd " *my-planning-path* "symba; ./run-symba.sh ") "Running SyMBA*")
(defvar *parse-symba-script* (concatenate 'string  "cd " *my-planning-path* "symba; ./parse-symba-output.sh ") "Parsing SyMBA* output")
(defvar *parse-lama-first-script* (concatenate 'string  "cd " *my-planning-path* "new-fd/downward/; ./parse-lama-first-output.sh "))
(defvar *ma-fd-script* (concatenate 'string "cd " *my-planning-path* "ma-fd/src; ./run-ma-fd.sh ")
  "We can configure how to run MA-FD.")
(defvar *cgamer-script* (concatenate 'string "cd " *my-planning-path* "cgamer; ./run-cgamer.sh ")
  "We can configure how to run CGamer.")
(defvar *metric-ff-script* (concatenate 'string "cd " *my-planning-path* "Metric-FF; ./run-ff.sh ")
  "We can configure how to run Metric-FF. I am currently calling a EHC version")
(defvar *cbp-estocastico-script* (concatenate 'string "cd " *my-planning-path* "Roller; ./run-cbp-estocastico.sh ")
  "We can configure how to run CBP. I am currently calling an stochastic version")
(defvar *cbp-script* (concatenate 'string "cd " *my-planning-path* "Roller; ./run-roller.sh ")
  "We can configure how to run CBP. I am currently calling an anytime BFS")
(defvar *lpg-adapt-script* (concatenate 'string *my-planning-path* "LPG-adapt/run-lpg-adapt.sh ")
  "We can configure how to run LPG-Adapt")
(defvar *lpg-adapt-quality-script* (concatenate 'string *my-planning-path* "LPG-adapt/run-lpg-adapt-quality.sh ")
  "We can configure how to run LPG-Adapt with multiple solutions")
(defvar *optic-script* (concatenate 'string "cd " *my-planning-path* "optic; ./run-optic-opt.sh ")
  "We can configure how to run OPTIC")
;; Old versions
;; These scripts correspond to older versions of FD code (-2017)
;; (defvar *lama-unit-cost-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-unit-cost.sh ")
;;   "We can configure how to run lama-first. I am now calling a simplified Lama that only finds the first solution using unit cost")
;; (defvar *lama-first-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-first.sh ")
;;   "We can configure how to run lama-first. I am now calling a simplified Lama that only finds the first solution using cost")
;; (defvar *lama-seq-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-seq.sh ")
;;   "We can configure how to run sequential lama.")
;; (defvar *lama-second-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-second.sh ")
;;   "We can configure how to run LAMA starting from the 2nd iteration. It assumes someone has already found a solution, so
;; we provide an upper bound on cost")
;; (defvar *lama-opt-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-lama-opt.sh ")
;;   "We can configure how to run optimal lama (FDSS-1).")
;; (defvar *fd-opt-script* (concatenate 'string "cd " *my-planning-path* "fd; ./run-fd-opt.sh ")
;;   "Runs optimal FD with an input describing the search algorithm and heuristic to be used.")

;; (defvar *sols* nil "Auxiliary")
(defvar *trace-ma-sayphi* nil)

(defvar *domains-dir* (concatenate 'string *my-planning-path* "sayphi/domains/") "In case, we did not load Sayphi before")
;; (defvar *domain-dir* (concatenate 'string *domains-dir* "blocksworld/") "In case, we did not load Sayphi before")
(defvar *max-time* 0 "When using MA-FD, it has the makespan of planning time among all agents.")

;; copied from Sayphi so I do not have to load it
(defstruct (solution (:print-function solution-print))
  (found nil)
  (total-time nil)
  (pre-time nil)
  (search-time nil)
  (path nil)
  (num-nodes nil)
  (evaluated-nodes nil)
  (depth nil)
  (last-node nil)
  (stop-reason nil)
  (length nil)
  (total-cost nil))

;; I can call now Lama-first, Lama-second, Lama-seq, Lama-opt, FD-opt, cgamer, Metric-FF, MA-FD, CBP, CBP estocastico, ERRTPlan, Optic and Sayphi
;; max-cost for anytime algorithms
(defun the-ring (algorithm timeout domain domain-file problem-file
		 &key (previous-solution nil)
		   (domain-directory (format nil "~a~a/" *domains-dir* domain))
		   (output-directory (concatenate 'string domain-directory "result/"))
		   (probsets-dir (concatenate 'string domain-directory "probsets/"))
		   (plan-file (concatenate 'string output-directory (format nil "plan-~a.lisp" algorithm)))
		   (output-file (concatenate 'string output-directory (format nil "output-~a.lisp" algorithm)))
		   (results-file (concatenate 'string output-directory (format nil "results-~a.lisp" algorithm)))
		   max-cost search-options)
  (if *trace-ma-sayphi* (format t "~%Executing problem ~a with ~a..." problem-file algorithm))
  (case algorithm
    ((lama-first lama-first-cost lama-seq lama-second lama-opt fd-opt cgamer metric-ff ma-fd cbp-estocastico cbp optic symba)
     (if (eq algorithm 'ma-fd) (prepare-for-ma-fd domain))
     (let ((solution-list (execute-planner :planner algorithm :domain domain :domain-file domain-file
					   :problem-file problem-file :result-in-file-p nil
					   :domain-directory domain-directory :output-directory output-directory
					   :probsets-dir probsets-dir :plan-file plan-file
					   :output-file output-file :results-file results-file
					   :timeout timeout
					   :max-cost (or max-cost most-positive-fixnum)
					   :search-options search-options)))
       (if *trace-ma-sayphi* (format t "result: ~a" (if (nth 5 solution-list) 'solved 'unsolved)))
;;        (break)
       (make-solution :found (nth 5 solution-list)
		      :total-time (nth 0 solution-list)
		      :length (nth 1 solution-list)
		      :path (nth 4 solution-list)
		      :num-nodes (nth 3 solution-list)
		      :total-cost (nth 2 solution-list))))
    ((lpg-adapt lpg-adapt-quality)
     (let ((solution-list (execute-planner :planner algorithm :domain domain :domain-file domain-file
					   :problem-file problem-file :result-in-file-p nil
					   :previous-solution previous-solution :timeout timeout
					   :domain-directory domain-directory :output-directory output-directory
					   :probsets-dir probsets-dir :plan-file plan-file
					   :output-file output-file :results-file results-file)))
       (if *trace-ma-sayphi* (format t "result: ~a" (if (nth 4 solution-list) 'solved 'unsolved)))
       ;; we could also report on similarity
       (make-solution :found (and (nth 5 solution-list) t)
		      :total-time (nth 0 solution-list)
		      :length (nth 1 solution-list)
		      :path (nth 4 solution-list)
		      ;; this is different from the contents of the results file where the nth 3 is the similarity
		      :num-nodes (nth 3 solution-list)
		      :total-cost (nth 2 solution-list))))
    ;; not ready yet to use probsets-dir
    (errtplan (errt-planning domain-file problem-file previous-solution :domain domain :stochastic-p nil
			     :say-timeout timeout :load-domain-p nil))
    ((sayphi greedy-best-first a-star enforced-hill-climbing drives-planner a-star-drives)
     (let ((solution (if (eq algorithm 'greedy-best-first)
			 (plan :algorithm 'a-star :timeout timeout :search-options (cons '(:w_g 0) search-options))
			 (plan :algorithm (if (eq algorithm 'sayphi) 'enforced-hill-climbing algorithm)
			       :timeout timeout :search-options search-options))))
       (if (and (solution-p solution) (solution-found solution))
	   (setf (solution-path solution)
		 (mapcar #'(lambda (step) (gaction-planaction (snode-applied-action step)))
			 (solution-path solution))))
       solution))
    (otherwise (format t "~%I cannot handle planner ~a yet. Define appropriate interfaces in the-ring function (external-planner.lisp)" algorithm)
	       (make-solution :found nil
			      :total-time 0
			      :length 0
			      :path nil
			      :num-nodes 0
			      :total-cost 0))))

(defun prepare-for-ma-fd (domain)
  (let* ((agents (if (boundp '*agents*)
		     (mapcar #'(lambda (agent) (if (agent-p agent) (agent-name agent) agent)) *agents*)))
	 (num-agents (length agents))
	 (command1 (concatenate 'string "chmod a+x " *my-planning-path* "ma-fd/src/run-ma-fd.sh"))
	 (command2 (concatenate 'string "chmod a+x " *my-planning-path* "ma-fd/src/parse-ma-fd-output.sh"))
	 (output-file (concatenate 'string *domains-dir* domain "/result/output-MA-FD"))
	 (aux-output-file (concatenate 'string output-file ".lisp"))
	 (init-tcpip (+ (random 20) 3010)))
    (when agents
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/run-ma-fd.sh")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofile "#! /bin/bash")
	(format ofile "~%cd ~ama-fd/src/" *my-planning-path*)
	(format ofile "~%export PATH=/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/etc:/usr/sbin:/sbin:./:/usr/local/bin")
	(format ofile "~%\rm downward.tmp.* output output.sas plan_numbers_and_cost elapsed.time lama-output*")
	(format ofile "~%./translate/translate.py $1 $2")
	(format ofile "~%./preprocess/preprocess < output.sas")
	(dotimes (i num-agents)
	  (if (zerop i)
	      (format ofile "~%./search/downward --search \"eager_greedy([ff(),cea()])\" --agents ~d < output > ~a~d"
		      i output-file i)
	      (format ofile " &~%./search/downward --search \"eager_greedy([ff(),cea()])\" --agents ~d < output > ~a~d"
		      i output-file i)))
	(terpri ofile))
      (execute-shell-command command1)
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/agents")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofile "~d" num-agents)
	(dolist (agent agents)
	  (format ofile "~%~(~a~)" agent))
	(terpri ofile))
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/comm")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(dotimes (i num-agents)
	  (format ofile "127.0.0.1:~d~%" (+ i init-tcpip))))
      (with-open-file (ofile (concatenate 'string *my-planning-path* "ma-fd/src/parse-ma-fd-output.sh")
			     :direction :output :if-exists :supersede :if-does-not-exist :create)
	(format ofile "#! /bin/bash~%")
	(format ofile "~%\rm ~a~%" aux-output-file)
	(dotimes (i num-agents)
	  (format ofile "cat ~a~d | grep \"Solution cost is: \\|Generated [0-9]* state\\|Total time:\\|^(\" | sed -e  \"s/step(s).//\" | sed -e \"s/state(s).//\" | sed -e \"s/SOLVED!!     //\" | sed -e \"s/Solution cost is: /co /\" |  sed -e \"s/Generated /no /\" | sed -e \"s/state(s)./ /\" | sed -e \"s/Total time: /ti /\" | sed -e \"s/s//\" >> ~a~%"
		  output-file i aux-output-file)))
      (execute-shell-command command2))))
	
;; planner: lama-first, lama-second, lama-seq, lama-opt, fd-opt cgamer, metric-ff
;; output-file: auxiliary file
;; results-file: accumulated info
;; example: (execute-probset-planner :planner 'lama-first :domain "rover" :domain-file "StripsRover.pddl" :problem-prefix "pfile*")
(defun execute-probset-planner (&key (planner 'lama-first-cost) (domain "blocksworld") (domain-file "ipc-domain.pddl")
				  (problem-prefix "prob*")
				  (previous-solution nil) (timeout 300) (max-cost most-positive-fixnum)
				  (domain-directory (concatenate 'string *domains-dir* domain "/"))
				  (output-directory (concatenate 'string domain-directory "result/"))
				  (probsets-dir (concatenate 'string domain-directory "probsets/"))
				  (plan-file (concatenate 'string output-directory (format nil "plan-~a.lisp" planner)))
				  (output-file (concatenate 'string output-directory (format nil "output-~a.lisp" planner)))
				  (results-file (concatenate 'string output-directory
							     (format nil "results-~a.lisp" planner))))
  (let ((problem-file-no-dir nil))
    (with-open-file (ostream results-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ostream ";; ((Problem Time Length Cost Nodes Solution)*)~2%("))
    (if (not (member planner '(lama-first lama-first-cost lama-second lama-seq lama-opt fd-opt ma-fd cgamer metric-ff
			       lpg-adapt lpg-adapt-quality cbp-estocastico cbp optic symba)))
	(say-domain domain domain-file nil))
    (dolist (problem (directory (concatenate 'string probsets-dir problem-prefix)))
      (setq problem-file-no-dir (if (pathname-type problem)
				    (concatenate 'string (pathname-name problem) "." (pathname-type problem))
				    (concatenate 'string (pathname-name problem))))
      (case planner
	((lama-first lama-first-cost lama-second lama-seq lama-opt fd-opt ma-fd cgamer metric-ff
		     lpg-adapt lpg-adapt-quality cbp-estocastico cbp optic symba)
	 (execute-planner :planner planner :domain domain :domain-file domain-file
			  :problem-file problem-file-no-dir :probsets-dir probsets-dir
			  :previous-solution previous-solution :timeout timeout :max-cost max-cost
			  :domain-directory domain-directory :output-directory output-directory
			  :output-file output-file :plan-file plan-file :results-file results-file))
	;; not ready yet to use probsets-dir
	(errtplan (errt-planning domain-file problem-file-no-dir previous-solution :domain domain
				 :stochastic-p nil :say-timeout timeout :load-domain-p nil))
	(otherwise (plan :algorithm planner :timeout timeout))))
    (with-open-file (ostream results-file :direction :output :if-exists :append)
      (format ostream ")"))))

;; previous-solution: list or pathname in case of ERRTPLAN; pathname in case of LPG-Adapt and LPG-ADAPT-QUALITY
;; example: (execute-planner :planner 'metric-ff :domain "blocksworld" :domain-file "ipc-domain.pddl" :problem-file "probBLOCKS-10-2.pddl" :result-in-file-p nil)
(defun execute-planner (&key (planner 'lama-first-cost) (domain "blocksworld") (domain-file "ipc-domain.pddl")
			  (problem-file "probBLOCKS-4-0.pddl")
			  (domain-directory (concatenate 'string *domains-dir* domain "/"))
			  (previous-solution nil) (timeout 300) (max-cost most-positive-fixnum)
			  (output-directory (concatenate 'string domain-directory "result/"))
			  (probsets-dir (concatenate 'string domain-directory "probsets/"))
			  (plan-file (concatenate 'string output-directory (format nil "plan-~a.lisp" planner)))
			  (output-file (concatenate 'string output-directory (format nil "output-~a.lisp" planner)))
			  (results-file (concatenate 'string output-directory (format nil "results-~a.lisp" planner)))
			  (result-in-file-p t)
			  (search-options nil))
  (if (eq planner 'errtplan)
      (if (not (listp previous-solution))
	  (setq previous-solution (solution-from-file previous-solution)))
      (when (consp previous-solution)
	(with-open-file (ostream (concatenate 'string probsets-dir "previous-solution.sol")
				 :direction :output :if-exists :supersede :if-does-not-exist :create)
	  (say-pp-solution previous-solution nil ostream t))
	(setq previous-solution (concatenate 'string probsets-dir "previous-solution.sol"))))
  (with-open-file (ostream output-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (terpri ostream))
  (let* ((end-plan-p nil)
	 (lamap (and (member planner '(lama-first lama-first-cost lama-second lama-seq lama-opt fd-opt ma-fd symba)) t))
	 ;; set ma-fd output
	 (aux-output-file (if (or lamap (member planner '(optic lpg-adapt lpg-adapt-quality cbp cbp-estocastico)))
			      (concatenate 'string output-directory (format nil "output-~a-aux.lisp" planner))))
	 ;; removes previous plans
	 (command (concatenate 'string "rm " plan-file " " plan-file ".*")))
    (execute-shell-command command)
    ;; to empty all previous solutions
    (with-open-file (ostream plan-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (terpri ostream))
    (inner-execute-planner planner domain-directory domain-file probsets-dir problem-file plan-file
			   (if (or lamap (eq planner 'optic)) aux-output-file output-file)
			   (if (member planner '(lpg-adapt lpg-adapt-quality cbp cbp-estocastico)) aux-output-file output-file)
			   previous-solution timeout max-cost search-options)
    (multiple-value-bind (length cost nodes time plan solvedp)
	(parse-output-files planner plan-file lamap aux-output-file output-file timeout)
      (cond (result-in-file-p
	     (with-open-file (ostream results-file :direction :output :if-exists :append :if-does-not-exist :create)
	       (format ostream "~%(~a" problem-file)
	       (format ostream " ~,3f ~d ~,2f ~d ~% (" time length cost nodes)
	       (if (and solvedp (not plan))
		   (format ostream "~%  T")
		   (dolist (action plan)
		     (cond ((and (listp action) (not end-plan-p))
			    (format ostream "~%  ~a" action))
			   (end-plan-p
			    (format ostream "~% ~a" action))
			   (t (setq end-plan-p t)
			      (format ostream ")~% ~a" action)))))
	       (format ostream "))~%"))
	     plan)
	    (t `(,time ,length ,cost ,nodes ,plan ,solvedp))))))

(defun inner-execute-planner (planner domain-directory domain-file probsets-dir problem-file plan-file
			      output-file aux-output-file previous-solution timeout max-cost search-options)
  (let ((command (case planner
		   (lama-first-cost (concatenate 'string *lama-first-cost-script*  " --plan-file " plan-file " " domain-directory domain-file
						" " probsets-dir problem-file " > " output-file))
		   (lama-first (concatenate 'string *lama-first-script*  " --plan-file " plan-file " " domain-directory domain-file
						" " probsets-dir problem-file " > " output-file))
		   ;; it seems the most-positive-fixnum of C++ is much less the one of Lisp!!
		   ;; this does not work now, since I have to incorporate the bound in the new script
		   (lama-second (concatenate 'string *lama-second-script* domain-directory domain-file
					     " " probsets-dir problem-file " --plan-file " plan-file " > " output-file
					     (format nil " ~d" (min max-cost
								    (floor (/ most-positive-fixnum 10000000000.0))))))
		   (lama-seq (concatenate 'string *lama-seq-script* " --plan-file " plan-file " " domain-directory domain-file
					  " " probsets-dir problem-file " > " output-file))
		   (lama-opt (concatenate 'string *lama-opt-script* " --plan-file " plan-file " " domain-directory domain-file
					  " " probsets-dir problem-file " > " output-file))
		   (fd-opt (generate-fd-opt-command plan-file domain-directory domain-file probsets-dir problem-file
						    search-options output-file))
		   (ma-fd (concatenate 'string *ma-fd-script* domain-directory domain-file
				       " " probsets-dir problem-file))
		   (symba (concatenate 'string *symba-script* " " *my-planning-path* " " domain-directory domain-file
				       " " probsets-dir problem-file " " plan-file " " output-file))
		   (cgamer (concatenate 'string *cgamer-script* domain-directory domain-file
					" " probsets-dir problem-file " " plan-file " " output-file))
		   (metric-ff (concatenate 'string *metric-ff-script* " " domain-directory domain-file
					   " " probsets-dir problem-file " " plan-file))
		   (cbp (concatenate 'string *cbp-script* " " domain-directory " " domain-file
				     " " (compute-relative-path domain-directory probsets-dir) problem-file
				     " " (format nil "~,2f" timeout) " " aux-output-file))
		   (cbp-estocastico (concatenate 'string *cbp-estocastico-script* " " domain-directory " " domain-file
						 " " (compute-relative-path domain-directory probsets-dir) problem-file
						 " 0.7 " (format nil "~,2f" timeout) " " aux-output-file))
		   (lpg-adapt (concatenate 'string *lpg-adapt-script* domain-directory domain-file
					   " " probsets-dir problem-file " " plan-file
					   " " previous-solution " " (format nil "~d" timeout) " " aux-output-file))
		   (lpg-adapt-quality (concatenate 'string *lpg-adapt-quality-script* domain-directory domain-file
						   " " probsets-dir problem-file " " plan-file
						   " " previous-solution " " (format nil "~d" timeout) " " aux-output-file))
		   (optic (concatenate 'string *optic-script* " " domain-directory domain-file
				       " " probsets-dir problem-file " " output-file))
		   (t nil)))
	(script (concatenate 'string *my-tmp-path* "run-planner")))
    (if command
	(timed-execute-shell-command command :bash-file script :timeout timeout :time-units 'real-time)
	(format t "~%Unknown planner ~a" planner))))

;; I just assume that probsets-dir is a subdirectory of domain-dir
(defun compute-relative-path (domain-dir probsets-dir)
  (subseq probsets-dir (string-lessp domain-dir probsets-dir)))

;; It generates an appropriate command string to execute FD-OPT with/out H2 pre-processor or heuristics
(defun generate-fd-opt-command (plan-file domain-directory domain-file probsets-dir problem-file search-options
				output-file)
  (let ((command (concatenate 'string "\rm output.sas estimation.lisp " output-file "; "))
	(translate-p (eq (cdr (assoc 'pre-processor search-options)) 'vidal-alvaro)))
    (if translate-p
	(setq command (concatenate 'string command *fd-translate-opt-script* domain-directory domain-file
				   " " probsets-dir problem-file "; "
				   *h2-preprocessor-path* " < output.sas; ")))
    (concatenate 'string command *fd-opt-script*
		 "--plan-file tmp-plan-file.sol "
		 (if translate-p
		     "output.sas "
		     (concatenate 'string domain-directory domain-file " " probsets-dir problem-file))
		 (generate-search-string search-options)
		 " > " output-file " ; mv tmp-plan-file.sol " plan-file)))

;; (defun generate-fd-opt-command (plan-file domain-directory domain-file probsets-dir problem-file search-options
;; 				output-file)
;;   (let ((command *fd-opt-script*))
;;     (if (eq (cdr (assoc 'pre-processor search-options)) 'vidal-alvaro)
;; 	(setq command (concatenate 'string command "--translate " domain-directory domain-file
;; 				   " " probsets-dir problem-file "; "
;; 				   *h2-preprocessor-path* " < output.sas; ./fast-downward.py ")))
;;     (concatenate 'string command (if *release64-p* "--build release64 " "")
;; 		 "--plan-file  tmp-plan-file.sol " domain-directory domain-file
;; 		 " " probsets-dir problem-file (generate-search-string search-options)
;; 		 " > " output-file " ; mv tmp-plan-file.sol " plan-file)))

;; DB I could make it more general by allowing other parameters for other heuristics
(defun generate-search-string (search-options)
  (let ((heuristics (cdr (assoc 'heuristics search-options)))
	(heuristics-string "")
	(result (format nil " --search \"astar(")))
    (setq heuristics-string
	  (if heuristics
	      (cond ((> (length heuristics) 1)
		     (dolist (heuristic heuristics (format nil "max([~a])" heuristics-string))
		       (setq heuristics-string
			     (if (string= heuristics-string "")
				 (heuristic-string heuristic search-options)
				 (format nil "~a,~a" heuristics-string
					 (heuristic-string heuristic search-options))))))
		    (t (format nil "~a" (heuristic-string (car heuristics) search-options))))
	      (format nil "~(~a~)()" *default-opt-heuristic*)))
    (format nil "~a~a)\"" result heuristics-string)))

(defun heuristic-string (heuristic search-options)
  (case heuristic
    (diverse_potentials "diverse_potentials(num_samples=1000, max_num_heuristics=infinity, max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true, random_seed=-1)")
    (sample_based_potentials "sample_based_potentials(num_heuristics=1, num_samples=1000, max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true, random_seed=-1)")
    (merge_and_shrink "merge_and_shrink(merge_strategy=merge_precomputed(merge_tree=linear(variable_order=reverse_level)),shrink_strategy=shrink_bisimulation(greedy=true),label_reduction=exact(before_shrinking=true,before_merging=false),max_states=infinity,threshold_before_merge=1)")
    (all_states_potential "all_states_potential(max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)")
    (potentials "max([sample_based_potentials(num_heuristics=1, num_samples=1000, max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true, random_seed=-1),all_states_potential(max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true),initial_state_potential(max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)])")
    (cpdbs "cpdbs(patterns=systematic(1), dominance_pruning=true, transform=no_transform(), cache_estimates=true)")
    (cegar "cegar(subtasks=[landmarks(),goals()], max_states=infinity, max_transitions=1000000, max_time=infinity, pick=MAX_REFINED, use_general_costs=true, transform=no_transform(), cache_estimates=true, random_seed=-1)")
    (ipdb (format nil "~(~a(max_time=~d)~)" heuristic (or (cdr (assoc 'ipdb-timelimit search-options)) 20)))
    (zopdbs "zopdbs(patterns=systematic(1), transform=no_transform(), cache_estimates=true)")
    (pdb "pdb(pattern=greedy(max_states=1000000), transform=no_transform(), cache_estimates=true)")
    (operatorcounting-lmc "operatorcounting(constraint_generators=[lmcut_constraints()], lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)")
    (operatorcounting-pho "operatorcounting(constraint_generators=[pho_constraints(patterns=systematic(2))], lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)")
    ((operatorcounting operatorcounting-lmc-seq) "operatorcounting(constraint_generators=[lmcut_constraints(),state_equation_constraints()], lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)")
    (initial_state_potential "initial_state_potential(max_potential=1e8, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)")
    (lmcut "lmcut(transform=no_transform(), cache_estimates=true)")
    (lmcount "lmcount(lm_factory=lm_hm(m=2, reasonable_orders=false, only_causal_landmarks=false, disjunctive_landmarks=true, conjunctive_landmarks=true, no_orders=false, lm_cost_type=NORMAL), admissible=true, optimal=true, pref=false, alm=true, lpsolver=CPLEX, transform=no_transform(), cache_estimates=true)")
    (gapdb "cpdbs(patterns=genetic(pdb_max_size=50000, num_collections=5, num_episodes=30, mutation_probability=0.01, disjoint=false, random_seed=-1), dominance_pruning=true, transform=no_transform(), cache_estimates=true)")
    (otherwise (format nil "~(~a()~)" heuristic))))


;; max([gapdb(mutation_probability=0.01,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.05,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.10,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.15,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.20,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.25,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.30,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.35,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.40,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.45,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.50,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.55,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.60,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.65,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.70,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.75,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.80,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.85,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.90,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.95,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=1.00,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.01,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.05,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.10,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.15,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.20,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.25,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.30,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.35,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.40,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.45,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.50,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.55,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.60,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.65,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.70,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.75,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.80,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.85,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.90,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=0.95,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     gapdb(mutation_probability=1.00,disjoint=true,pdb_max_size=200000,num_episodes=30,num_collections=5),\
;;     ])
;; merge_and_shrink(merge_strategy=merge_linear(random_seed=-1, update_option=use_random, variable_order=CG_GOAL_LEVEL), shrink_strategy=shrink_bisimulation(greedy=false, at_limit=RETURN), label_reduction=exact(before_shrinking=true, before_merging=false, method=ALL_TRANSITION_SYSTEMS_WITH_FIXPOINT, system_order=RANDOM, random_seed=-1), prune_unreachable_states=true, prune_irrelevant_states=true, max_states=-1, max_states_before_merge=-1, threshold_before_merge=-1, transform=no_transform(), cache_estimates=true, verbosity=silent)
;; (defun generate-search-string1 (search-options)
;;   (let ((heuristics (cdr (assoc 'heuristics search-options)))
;; 	(ipdb-timelimit (or (cdr (assoc 'ipdb-timelimit search-options)) 20))
;; 	(heuristic-string "")
;; 	(result ""))
;;     (if heuristics
;; 	(if (> (length heuristics) 1)
;; 	    (dolist (heuristic heuristics (format nil " --search astar\\(max\\([~a]\\)\\) " result))
;; 	      (setq heuristic-string (if (eq heuristic 'ipdb)
;; 					 (format nil "~(~a\\(max_time=~d\\)~)" heuristic ipdb-timelimit)
;; 					 (format nil "~(~a\\(\\)~)" heuristic)))
;; 	      (setq result (if (string= result "")
;; 			       (format nil "~a" heuristic-string)
;; 			       (format nil "~a,~a" result heuristic-string))))
;; 	    (format nil " --search astar\\(~a\\) " (if (eq (car heuristics) 'ipdb)
;; 					      (format nil "~(~a\\(max_time=~d\\)~)" (car heuristics) ipdb-timelimit)
;; 					      (format nil "~(~a\\(\\)~)" (car heuristics)))))
;; 	(format nil " --search astar\\(~(~a~)\\(\\)\\) " *default-opt-heuristic*))))
      

;; It reads plan and output files and returns as values length, cost, nodes, time and plan
(defun parse-output-files (planner plan-file lamap aux-output-file output-file timeout)
  (let ((command nil) (plan nil) (solvedp nil) (input nil)
	(length most-positive-fixnum) (cost most-positive-fixnum) (nodes 0) (time timeout) (max-time 0))
    (setq command (case planner
		    (ma-fd ;; MA-FD does not generate a plan
		     (setq plan-file nil)
		     (concatenate 'string "cd " *my-planning-path* "ma-fd/src; ./parse-ma-fd-output.sh"))
		    ((lama-first lama-first-cost lama-opt fd-opt)
		     (concatenate 'string *parse-lama-first-script* " "  aux-output-file " " output-file))
		    ((cbp cbp-estocastico)
		     (setq plan-file nil)
		     (concatenate 'string "cd " *my-planning-path* "Roller; ./parse-cbp-output.sh "
				  aux-output-file " " output-file))
		    ((lama-second lama-seq)
		     ;; Since they generate several plans, I take the last one
		     (setq plan-file (car (sort (directory (concatenate 'string plan-file ".*"))
						#'(lambda (file1 file2)
						    (string> (pathname-type file1) (pathname-type file2))))))
		     (concatenate 'string "cd " *my-planning-path* "new-fd/downward/; ./parse-lama-seq-output.sh "
				  aux-output-file " " output-file))
		    (symba (concatenate 'string *parse-symba-script* " " aux-output-file " " output-file))
		    (optic (concatenate 'string "cd " *my-planning-path* "optic; ./parse-optic-output.sh "
					aux-output-file " " output-file))
		    ((lpg-adapt lpg-adapt-quality)
		     (concatenate 'string "cd " *my-planning-path* "LPG-adapt; ./parse-lpg-adapt-output.sh "
				  plan-file " " output-file " " aux-output-file))
		    (otherwise "ls")))
    (execute-shell-command command)
    ;; LAMA generates a plan file and an output-file, while the other two only an output file
    (if (and plan-file (probe-file plan-file))
	(with-open-file (istream plan-file :direction :input)
	  (case planner
	    ((lama-first lama-first-cost lama-second lama-seq lama-opt fd-opt cgamer symba)
	     (do* ((action (read istream nil :eof) (read istream nil :eof)))
		  ((eq action :eof)
		   (setq plan (nreverse plan)))
	       (push action plan)))
	    (metric-ff
	     (setq input (read istream nil :eof))
	     (when (and input (not (eq input :eof)))
	       (setq plan (car input))
	       (setq length (length plan))
	       (setq cost (length plan)) ;; to be declared. I would have to run Metric-FF in -O mode
	       (setq nodes (cadr input))
	       (setq time (caddr input))))
	    (t nil))
	  (if plan (setq solvedp t))))
    (if (probe-file output-file)
	(with-open-file (istream output-file :direction :input)
	  (case planner
	    ((lama-first lama-first-cost cgamer lama-opt fd-opt symba)
	     (setq solvedp (and plan (or (read istream nil nil) solvedp)))
	     (setq length (read istream nil '0))
	     (setq cost (read istream nil '0))
	     (setq nodes (read istream nil '0))
	     (setq time (read istream nil '0)))
	    ((lama-seq lama-second)
	     (do ((metric (read istream nil :eof) (read istream nil :eof))
		  (read nil)
		  (new-solution-p nil))
		 ((eq metric :eof))
	       (if (member metric '(length cost nodes))
		   (setq read (read istream nil :eof)))
	       (case metric
		 (length (if (numberp read)
			     (when (<= read length)
			       (setq new-solution-p t)
			       (setq length read))
			     0))
		 (cost (if (numberp read)
			   (when (<= read cost)
			     (setq new-solution-p t)
			     (setq cost read))
			   0))
		 (nodes (if (numberp read)
			    (if new-solution-p
				(incf nodes read)
				(setq nodes read))
			    0))
		 (time (if (numberp read)
			   (setq time read)
			   0))
		 (t nil))))
	    ((cbp cbp-estocastico)
	     (setq input (read istream nil :eof))
	     (setq solvedp (and (listp input) (atom (car input))))
	     (when solvedp
	       (setq plan (butlast (cdr input)))
	       (setq length (length plan))
	       (setq cost length)
	       ;; 		   (setq nodes (read istream nil '0))
	       (setq time (car (last input)))))
	    (ma-fd
	     (when (boundp '*agents*)
	       (setq time 0.0)
	       (do ((the-read (read istream nil :eof)
			      (if (not (eq the-read :eof)) (read istream nil :eof))))
		   ((eq the-read :eof))
		 (case the-read
		   (co (setq the-read (read istream nil :eof))
		       (if (numberp the-read) (setq cost the-read)))
		   (no (setq the-read (read istream nil :eof))
		       (if (numberp the-read) (incf nodes the-read)))
		   (ti (setq the-read (read istream nil :eof))
		       (when (numberp the-read)
			 (incf time the-read)
			 ;; I report the time, but I set the max-time, so that anyone can access it to report it
			 (setq max-time (max max-time the-read))))
		   (otherwise nil)))
	       ;; MA-FD does not report the plan, nor its length
	       (setq length cost)
	       (setq *max-time* max-time)
	       ;; plan=nil means no plan
	       (if (< cost most-positive-fixnum) (setq plan '(t)))))
	    ((lpg-adapt lpg-adapt-quality)
	     (setq input (peek-char nil istream))
	     (unless (eq input #\))
	       (setq input (read istream nil :eof))
	       (when (and input (not (eq input :eof)) (listp input))
		 ;; to get rid of name of problem
		 (if (atom (car input)) (setq input (cdr input)))
		 (setq solvedp (cadr (assoc 'solvedp input)))
		 (setq plan (and solvedp (cdr (assoc 'plan input))))
		 (setq length (if solvedp (length plan) most-positive-fixnum))
		 (setq cost (if solvedp
				(or (cadr (assoc 'cost input)) length)
				most-positive-fixnum))
		 (setq nodes 0) ;; it does not print it
		 (setq time (if solvedp
				(or (cadr (assoc 'time input)) 0.01)
				timeout)))))
	    (optic 
	     (setq input (read istream nil :eof))
	     (when (and input (not (eq input :eof)))
	       (setq solvedp (car input))
	       (setq length (nth 1 input))
	       (setq cost (nth 2 input))
	       (setq nodes (nth 3 input))
	       (setq time (nth 4 input))
       	       (setq plan (nth 5  input))))
	  (otherwise nil))))
    ;; I am not sure when it happens
    (if (> time timeout)
	(setq time timeout))
    (if (and plan (or (not length) (= length most-positive-fixnum)))
	(setq length (length plan)))
    (if (and plan (or (not cost) (= cost most-positive-fixnum)))
	(setq cost (length plan)))
    (values length cost nodes time plan solvedp)))
;; LPG:
;; 		 (setq plan (if (listp (nth 3 input)) (nth 3 input) (nth 4 input)))
;; 		 ;; sometimes the plan gets extra parenthesis
;; 		 (setq plan (if (consp (caar plan)) (car plan) plan))
;; 		 (setq solvedp (or (and plan t) (nth 0 input)))
;; 		 (setq length (length plan))
;; 		 (setq cost (nth 2 input))
;; 		 (setq cost (if (and (numberp cost) (> cost 0)) cost length))
;; 		 (setq nodes 0) ;; it does not print it
;; 		 (setq time (nth 1 input))
;; 	    (optic (do* ((previous-solution nil solution-data)
;; 			 (solution-data (read istream nil :eof) (read istream nil :eof)))
;; 			((eq solution-data :eof)
;; 			 (cond ((and previous-solution (listp previous-solution))
;; 				(setq solvedp t)
;; 				(setq length 0)
;; 				(setq cost (car previous-solution))
;; 				(setq nodes (cadr previous-solution))
;; 				(setq time (caddr previous-solution))
;; 				(setq plan (cadddr previous-solution)))
	    ;; 			       (t (setq solved nil))))))

;; to convert a file generated by the above call, like ((prob1 time1 length1 cost1 nodes1 solution1) ... (probn timen lengthn costn nodesn solutionn))
;; into a plot file
;; (write-data-file "/home/messier/planning/sayphi/domains/test-grid/result/summary-results-planner.lisp" "/home/messier/planning/sayphi/domains/test-grid/result/results-planner.lisp")
(defun write-data-file (data-file summary-file)
  (let ((the-sols nil))
    (with-open-file (ifile summary-file :direction :input)
      (setf the-sols (read ifile)))
    (with-open-file (ofile data-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format ofile "# 0 Prob, 1 Solved, 2 Time, 3 Length, 4 Nodes, 5 Evals, 6 Cost, 7 ReuseClosed, 8 Reuse, 9 CurrentSearch, 10 NumberBetterLength, 11 NumberBetterCost, 12 TotalTimeHeuristic, 13 Unstability (12 AvgTimeHeuristic 13 TotalTimeHeuristic 14 Unstability in PLANNER)~%")
      (do* ((sols the-sols (cdr sols))
	    (sol (car sols) (car sols))
	    (i 0 (1+ i)))
	   ((null sols))
	(format ofile "~d 1.0 ~4$ ~d ~d 0 ~4$ 0 0 0 0 0 0.0 0~%" i (nth 1 sol) (nth 2 sol) (nth 4 sol) (nth 3 sol))))))

;; it takes a list or a structure of type solution and returns the plan in a list
(defun give-me-plan-list (solution &optional (even-if-not-found-p nil))
  (if (listp solution)
      solution
      (if (and solution (solution-p solution) (or even-if-not-found-p (solution-found solution)))
	  (if (snode-p (car (solution-path solution)))
	      (pp-solution-sayphi solution)
	      (solution-path solution)))))

(defun solution-print (solution stream z)
  (declare (type solution solution)
	   (stream stream)
	   (ignore z))
  (format stream "~%#<SOLUTION: ~a  " (solution-found solution))
  (format stream " Length: ~d " (solution-length solution))
  (format stream " Nodes: ~d " (solution-num-nodes solution))
  (format stream " Evaluated: ~d " (solution-evaluated-nodes solution))
  (format stream " Depth: ~d >" (solution-depth solution))
  (format stream "~%       Total Time        : ~d" (solution-total-time solution))
  (format stream "~%       Instantiating Time: ~d" (solution-pre-time solution))
  (format stream "~%       Search Time       : ~d" (solution-search-time solution))
  (format stream "~%")
  (if (and (boundp '*current-problem*) *current-problem*)
      (format stream "~%       Metric            : ~a" (problem-metric *current-problem*)))
  (format stream "~%       Total Cost        : ~a" (solution-total-cost solution))
  )
