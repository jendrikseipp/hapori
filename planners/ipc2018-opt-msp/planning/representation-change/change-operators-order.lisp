(in-package "COMMON-LISP-USER")

;; let us assume for now that we will have at most this number of actions. Just add more combinations of letters to this
;; list in case of having more actions
(defvar *letters* '(aa ab ac ad ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ay ax az
		    ba bb bc bd be bf bg bh bi bj bk bl bm bn bo bp bq br bs bt bu bv bw by bx bz
		    ca cb cc cd ce cf cg ch ci cj ck cl cm cn co cp cq cr cs ct cu cv cw cy cx cz
		    da db dc dd de df dg dh di dj dk dl dm dn do dp dq dr ds dt du dv dw dy dx dz
		    ea eb ec ed ee ef eg eh ei ej ek el em en eo ep eq er es et eu ev ew ey ex ez
		    fa fb fc fd fe ff fg fh fi fj fk fl fm fn fo fp fq fr fs ft fu fv fw fy fx fz
		    ga gb gc gd ge gf gg gh gi gj gk gl gm gn go gp gq gr gs gt gu gv gw gy gx gz
		    ha hb hc hd he hf hg hh hi hj hk hl hm hn ho hp hq hr hs ht hu hv hw hy hx hz
		    ia ib ic id ie if ig ih ii ij ik il im in io ip iq ir is it iu iv iw iy ix iz
		    ja jb jc jd je jf jg jh ji jj jk jl jm jn jo jp jq jr js jt ju jv jw jy jx jz
		    ka kb kc kd ke kf kg kh ki kj kk kl km kn ko kp kq kr ks kt ku kv kw ky kx kz
		    la lb lc ld le lf lg lh li lj lk ll lm ln lo lp lq lr ls lt lu lv lw ly lx lz
		    ma mb mc md me mf mg mh mi mj mk ml mm mn mo mp mq mr ms mt mu mv mw my mx mz
		    na nb nc nd ne nf ng nh ni nj nk nl nm nn no np nq nr ns nt nu nv nw ny nx nz
		    oa ob oc od oe of og oh oi oj ok ol om on oo op oq or os ot ou ov ow oy ox oz
		    pa pb pc pd pe pf pg ph pi pj pk pl pm pn po pp pq pr ps pt pu pv pw py px pz))

;; change-type can be (for now): random, inverse, alphabetical-inverse, alphabetical-random
(defun change-ops-order (domain change-type
			 &key (domain-file "domain.pddl")
			   (new-domain-file (format nil "~a-~a" change-type domain-file))
			   (domain-dir (concatenate 'string *domains-dir* domain "/")))
  (let* ((domain-def (cdr (read-all-file (concatenate 'string domain-dir domain-file))))
	 (types (find-argument domain-def :types))
	 (predicates (find-argument domain-def :predicates))
	 (functions (find-argument domain-def :functions))
	 (constants (find-argument domain-def :constants))
	 (actions (give-me-all-actions domain-def))
	 (new-actions nil)
	 (substitution nil))
    (case change-type
      (alphabetical-inverse
       (multiple-value-setq (new-actions substitution)
	 (change-alphabetical-names (sort actions #'string> :key #'pddl-action-name) 'inverse)))
      (alphabetical-random
       (multiple-value-setq (new-actions substitution)
	 (change-alphabetical-names actions 'random)))
      (random (setq new-actions (shuffle actions :test #'equal)))
      (inverse (setq new-actions (reverse actions)))
      (otherwise nil))
    (write-domain-pddl-file (car (find-argument domain-def 'domain))
			    (find-argument domain-def :requirements)
			    types predicates functions
			    new-actions
			    (concatenate 'string domain-dir new-domain-file)
			    constants)
    substitution))
;;        (setq actions (sort actions #'string-lessp :key #'pddl-action-name))))

(defun change-alphabetical-names (actions operation)
  (let* ((substitution nil)
	 (new-actions (mapcar #'(lambda (action letters)
				  (let* ((old-name (cadr action))
					 (new-name (intern (format nil "~:@(~a~a~)" letters old-name))))
				    (setf (cadr action) new-name)
				    (push (cons new-name old-name) substitution)
				    action))
			      actions
			      (if (eq operation 'random)
				  (shuffle *letters*)
				  *letters*))))
    (values new-actions substitution)))
