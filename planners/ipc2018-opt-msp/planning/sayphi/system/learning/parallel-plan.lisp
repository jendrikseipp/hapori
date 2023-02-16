;;Convierte el plan secuencial guardado en solution-file en un plan paralelo y lo salva en output.
;;solution-file puede ser un fichero con el formato 'ipc-num, cada linea representa una accion del plan con el siguiente formato:
;; 0: (CALIBRATE ROVER0 CAMERA0 OBJECTIVE1 WAYPOINT3) [1]
;;un fichero como (si format es 'ipc):
;; ;;Runtime:0.116 length:10
;; (CALIBRATE ROVER0 CAMERA0 OBJECTIVE1 WAYPOINT3)
;; (TAKE_IMAGE ROVER0 WAYPOINT3 OBJECTIVE1 CAMERA0 HIGH_RES)
;; o una solucion directamente  (si format es 'sol)
;;domain y problem son los ficheros pddl del dominio y problema con la ruta completa
;;(parallel-plan "/home/sfernandez/svn-nerea-all/domains/rover/domain.pddl" "/home/sfernandez/svn-nerea-all/domains/rover/ipc3-pfile07" "/home/sfernandez/svn-nerea-all/mapr/rover07.soln.seq" "rover07.pp")
(defun parallel-plan (domain problem solution-file output &key (format 'ipc))
    (let* ((parallel-plan nil)
	   (init-time (get-internal-real-time))
	   (solution (case format (sol solution-file)
			   (ipc-num (solution-from-file solution-file))
			   (ipc (read-plan-from-file solution-file))))
;;	   (solution (if (eq format 'ipc-num) (solution-from-file solution-file) (read-plan-from-file solution-file)))
	   )

       ;;Para buscar el plan paralelo primero hay que cargar el dominio y el problema en sayphi
       (read-pddl-domain domain)
       (read-pddl-problem problem)

       (setq parallel-plan (build-parallel-plan solution))
       ;;Hay que escribir la solucion en el outputfile
       (write-solution-in-output-file output  parallel-plan nil nil nil nil nil nil
				     (elapsed-time init-time 'real-time))
       (format t "~%Parallel-plan save in ~a~%" output)
      )
    )


(defun write-solution-in-output-file (nfile solution timeout goal-selection replanning-algorithm run-cmap-p run-mapr-p sort-agents par-time)
  (let ((makespan (+ 1 (caar (last solution))))
        (len (length solution))
	)
    ;(format t "~%Makespan: ~a, longitud del plan ~a ~%" makespan len)
    (with-open-file (stream nfile
          :direction :output  :if-exists :supersede :if-does-not-exist :create)

      (format stream ";;~a, " (if run-cmap-p 'cmap (if run-mapr-p 'mapr)))
        (format stream "~a, ~a, ~a, ~a~%" timeout goal-selection replanning-algorithm sort-agents)
      (format stream ";;Makespan: ~a~%" makespan)
      (format stream ";;Plan length: ~a~%" len)
      (format stream ";;Parallelization time: ~a~%" par-time)
      (dolist (act solution)
         (format stream "~3d: ~a~%" (car act) (cadr act)))
   )
   (if solution
     (format t " SOLUTION FOUND!. See file ~a~%" nfile)
     (format t " NOT solution found. See file ~a~%" nfile)
     )
  ))


(defun read-plan-from-file (solution-file)
  (let ((plan nil))
  (with-open-file (ifile solution-file :direction :input)
	(do* ((action (read ifile nil 'eof) (read ifile nil 'eof)))
	     ((eq action 'eof) plan)
	  (setf plan (append plan (list action)))))))


