;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; Load GATE.
;
;*******************************************************************************

(setq *question-mark-atom* '\?)

(progn
 (setq *gate-version* "GATE 2.3, Common Lisp version of 2004-12-20")
 (format t "=======================~%")
 (format t "Loading ~A...~%" *gate-version*)
 (format t "=======================~%")
 nil)

(load "compat.cl")

(setq *gate-input* *standard-input*)
(setq *gate-output* *standard-output*)
(setq *gate-dbg* *standard-output*)
(setq *gate-warn-dbg* t)
(setq *gen-stream* nil)

(if (not (boundp '*gate-load-options*))
    (setq *gate-load-options* nil)
    nil)

(load "loop.cl")
(load "gate_macros.cl")

(load "gate_main.cl")
(load "gate_ty.cl")
(load "gate_cx.cl")
(load "gate_instan.cl")
(load "gate_prove.cl")
(load "gate_read_pr.cl")
(load "gate_unify.cl")
(load "gate_utils.cl")

(load "gate_obs.cl")

(interest 'ob-warn 'all)
(interest 'context 'all)

(format t "=======================~%")
(format t "Welcome to ~A~%" *gate-version*)
(format t "=======================~%")
    
; End of file.
