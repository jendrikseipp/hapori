;; =========================================================================    
;;  (C) Copyright 2006, 2008 
;;      Universidad Carlos III de Madrid
;;      Planning & Learning Group (PLG)
;; 
;; =========================================================================
;; 
;; This file is part of SAYPHI
;; 
;; 
;; SAYPHI is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; SAYPHI is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with SAYPHI.  If not, see <http://www.gnu.org/licenses/>.
;; ========================================================================

;; Authors: Angel Garcia Olaya & Tomas de la Rosa 
;; Description: Some functions for handling oversubscription problems
;; Date: 2007.10.22
;; 
;; ========================================================================

(defparameter *sayover-algorithm* 'over-search)


(defun over-search (init-node heuristic-fn cost-fn search-options)
  (declare (ignore cost-fn search-options))
  ;;Calculando el plan relajado por meta
  (dolist (igoal (problem-lit-goals *current-problem*))
    (let ((igoal-state (onegoal-bitmap igoal)))
      (multiple-value-bind 
          (h-value relaxed-plan focus-goals) (funcall heuristic-fn init-node igoal-state)
	(format t "~% GOAL: ~a" igoal)
	(format t "~% >>> Heuristic value: ~a" h-value)
	(format t "~% >>> Relaxed Plan: ~% ~a" relaxed-plan)
	(format t "~% _______________________")
     ))))



(defun over-plan (&key (algorithm *sayover-algorithm*)
		       (heuristic nil)
		       (timeout *say-timeout*)
                       (depthbound *say-depthbound*)
		       (cost *say-costfn*)
		  )
  (let ((problem *current-problem*)
	(i-node nil)
	(algorithm-fn (if (not (null algorithm)) (symbol-function algorithm) (say-plan-defaults 'algorithm)))
	(heuristic-fn (if (not (null heuristic)) (symbol-function heuristic) (say-plan-defaults 'heuristic)))
	(cost-fn (symbol-function cost))
	(start-time (get-internal-run-time))
	(search-options nil)
	(sol nil))


    (set-sayp :say-timeout timeout)
    (set-sayp :say-depthbound depthbound)

    (sayout-initialize)
    (setf i-node (initialize-current-problem))
    (set-duplicate-hashing)

    (setf (get (problem-plist problem) :initial-pre-time) start-time)
    (setf (get (problem-plist problem) :initial-search-time) (get-internal-run-time))
    
 
    (sayout-search t algorithm heuristic cost)


    ;; De momento no buscamos ninguna solucion
   (setf sol (funcall algorithm-fn i-node heuristic-fn cost-fn search-options))


))
