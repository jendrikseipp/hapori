#!/usr/bin/sbcl --script

;; Script for compiling metasearch
;; It generates a SBCL image 
;; For running it type:
;; sbcl --dynamic-space-size 950 --script "metasearch-exe-generator.lisp" 

(load ".sbclrc")
;;(load ".init.lisp")
(load-meta-search)

(save-lisp-and-die "metasearch-exe" :save-runtime-options t :executable t)
