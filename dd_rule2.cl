;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 11/19/86: Broke up into two files
;
;*******************************************************************************

; Todo: Is it redundant also to run fact plans in run-plan? (no 's') Will they
; all already have been tried before?
(defun run-plans (top-level-goal context belief-path)
 (ndbg-roman-nl *gate-dbg* rule "Running plans in ~A for ~A bp ~A"
       context top-level-goal belief-path)
 ; (New) p-goals override any other goals. If there is a p-goal, then
 ; that's all we do this time. Plus we only do one p-goal at
 ; a time (thus the until; in the case of regular planning,
 ; seq next will ensure that only one subgoal is planned for at
 ; a time; if there is no seq next, then the user desired for
 ; more than one to be planned at a time...)
 (let ((rule-fired? nil)
       (elems (cx$get-all context)))
  (yloop
   (yfor elem in elems)
   (yuntil rule-fired?)
   (ydo
    (setq elem (absolute->relative elem belief-path))
    (if (and elem
             (ty$instance? elem 'active-goal)
             (not (ty$instance? elem 'p-goal))
             (eq? (ob$get elem 'top-level-goal) top-level-goal)
             (not (has-link-relative? elem 'linked-from-of *intends-ob*
                                      context belief-path))
             (preservation-goal-subgoal? elem)
             ; Added the below
             (seq-head? elem context belief-path)
             ) ; end and
        (setq rule-fired? (or (run-plan elem top-level-goal context
                                       belief-path) rule-fired?)))))
  (if (not rule-fired?)
   (yloop 
    (yfor elem in elems)
    (ydo (setq elem (absolute->relative elem belief-path))
        (cond
         ((and elem
               (ty$instance? elem 'active-goal)
               (not (ty$instance? elem 'p-goal))
               (eq? (ob$get elem 'top-level-goal) top-level-goal)
               (not (has-link-relative? elem 'linked-from-of *intends-ob*
                                        context belief-path))
               (not (preservation-goal-subgoal? elem))
               (seq-head? elem context belief-path)
               ) ; end and
          (setq rule-fired? (or (run-plan elem top-level-goal context
                                         belief-path) rule-fired?)))
         ; This case would allow fact plans to fire in non-seq order.
         ; This does not work for EXPERIENCE1 in the beginning when
         ; it needs to keep ?Person2 as a variable so that serendipity
         ; can occur later.
         ; Fact plans are also done by the above, so the below isn't
         ; strictly necessary.
         ((and nil
               elem
               (ty$instance? elem 'active-goal)
               (not (ty$instance? elem 'p-goal))
               (eq? (ob$get elem 'top-level-goal)
                    top-level-goal))
          ;           (not (ob$get elem 'seq-next-of)) or like above
          (setq rule-fired? (or (run-fact-plan elem top-level-goal
                                              context belief-path)
                               rule-fired?)))))
    (yresult rule-fired?))
  rule-fired?)))

(defun preservation-goal-subgoal? (goal)
  (or (ty$instance? (ob$get goal 'obj) 'preservation)
      (ob$get goal 'preservation-subgoal?)))
 
; (Active-Goal Me ?x)
; (Believe Debra (Active-Goal Debra ?x)
; (Believe Debra (Believe Me (Active-Goal Me ?x)))

; Below to be determined empirically
(setq *perf-sim-thresh* 0.0)
(setq *perf-reality-thresh* 0.6)
(setq *perf-desir-thresh* 0.0) ; who knows?
(setq *goal-relaxation-realism* 0.2)

(setq *fanciful-sim-thresh* 0.0)
(setq *realistic-sim-thresh* 0.0)

;
; Order (and prune) episodes within candidates:
; - Real top-level-goal: Order according to reality*similarity*desirability
;                        with threshold cutoffs for each
;   Note: We want to keep around some alternatives for reversal; that's
;         why why don't just prune all but the highest.
; - Imaginary-realistic top-level-goal: Order according to
;       reality*similarity*desirability with threshold only for similarity
; - Imaginary-fanciful top-level goal: Order according to similarity with
;       threshold.
;
(defun order-candidates (goal-obj candidates top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule "Order candidates ~A" candidates)
  (yloop (initial (sim nil)
                 (ordering nil)
                 (any-episode? nil))
        (yfor candidate in candidates)
        (ydo (yloop (yfor episode in (candidate-episodes candidate))
                  (ydo (setq sim (ob$similarity goal-obj
                                               (ob$pget episode '(goal obj))))
                      (cond
                       ((real? top-level-goal)
                        (setq ordering (fl* (thresh (ob$get episode 'realism)
                                                   *perf-reality-thresh*)
                                           (fl* (thresh sim *perf-sim-thresh*)
                                                (thresh (ob$get episode
                                                                 'desirability)
                                                    *perf-desir-thresh*)))))
                       ((imaginary-realistic? top-level-goal)
                        (setq ordering (fl* (ob$get episode 'realism)
                                           (fl* (thresh sim
                                                 *realistic-sim-thresh*)
                                           (ob$get episode 'desirability)))))
                       ((imaginary-fanciful? top-level-goal)
                        (setq ordering (thresh sim *fanciful-sim-thresh*))))
                      (ob$set episode 'ordering ordering)
                      ; 'ordering and 'bd are used purely as temporary
                      ; variables which need dure only until all plans
                      ; are sprouted, which will not be interrupted
                      ; by other top-level goals.
                      (if (fl> ordering 0.0)
                          (setq any-episode? t)))))
    (yresult any-episode?)))

(defun candidates->episodes (candidates)
  (ndbg-roman-nl *gate-dbg* rule "Candidates to episodes ~A" candidates)
  (yloop (initial (result nil))
        (yfor candidate in candidates)
        (ydo (yloop (yfor episode in (candidate-episodes candidate))
                  (ydo (if (fl> (ob$get episode 'ordering) 0.0)
                          (progn
                           (ob$set episode 'bd
                                             (candidate-bd candidate))
                           (setq result (cons episode result)))))))
        (yresult
         (ndbg-roman-nl *gate-dbg* rule "Result = ~A" result)
         result)))

(defun find-candidate-rules (goal-obj believe-other? belief-path context)
  (ndbg-roman-nl *gate-dbg* rule "Find candidate rules for obj ~A in ~A"
                        goal-obj context)
  (yloop
   (initial (candidates nil))
   (yfor rule in (collect-planning-rules goal-obj))
   (ydo
    (if (plan? rule)
        (yloop (yfor bd in (rule-applications goal-obj context rule
                                            belief-path believe-other?))
              (ydo (setq candidates
                       (cons (candidate-create rule bd (episode-retrieve rule))
                             candidates))))))
   (yresult candidates)))

(defun collect-planning-rules (goal-obj)
   (ndbg-roman-nl *gate-dbg* rule-long "Collecting planning rules for ~A"
                  goal-obj)
   (let ((rules (backward-chain-rules goal-obj)))
        (if (eq? rules *rules*)
            (ndbg-roman-nl *gate-dbg* rule-long "Using full rule set")
            (ndbg-roman-nl *gate-dbg* rule-long "Collected rules = ~A" rules))
        rules))

; Todo: We need to break this function into several to make it more
; readable.
(defun run-plan (goal top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule "Run plan for ~A in ~A" goal context)
  (if (not (planning-loop? goal context top-level-goal belief-path))
  (let ((goal-obj (ob$get goal 'obj))
        (sprouted-contexts nil)
        (candidates nil)
        (existing-analogical-ep? (ob$get goal 'analogical-episode)))
   ; Todo: In fact the below is necessary to give 'fact plans' priority
   ; over generic planning so that action mutations function to
   ; provide new possibilities. Only this isn't implemented clearly.
; But I got rid of this since fact plans are now in here.
;   (if (cx$retrieve-relative context goal-obj belief-path)
;       (progn (ndbg-roman-nl *gate-dbg* rule
;                "Warning: Goal obj ~A already true in ~A"
;                    goal-obj context)
;              nil))
    ;
    ; Highest priority--existing analogical plan.
    ;
    (if existing-analogical-ep?
        (progn
     (ndbg-roman-nl *gate-dbg* rule "Try existing analogical plans")
     (yloop (yfor episode in (ob$gets goal 'analogical-episode))
           (ydo (setq sprouted-contexts
                    (append!
                    ; Ordering is 1.0: There should only be one
                    ; analogical goal at lower levels anyway--I don't know why
                    ; this is a loop.
                    (try-analogical-plan goal goal-obj context
                                         episode
                                         belief-path top-level-goal)
                     sprouted-contexts))))))
    ;
    ; Mutation plans are next highest priority.
    ;
    (if nil ;(and (me-belief-path? belief-path)
             ; (null? sprouted-contexts)
             ; (ob$gets top-level-goal 'mutation-plan-contexts))
        (progn
     (ndbg-roman-nl *gate-dbg* rule "Try mutation plans")
        (setq sprouted-contexts (run-mutation-plans goal
                                                   top-level-goal
                                                   context))))
    ;
    ; Facts are next highest priority.
    ;
    ; Actions cannot be satisfied by fact plans, though.
    ;
    (if (and (null? sprouted-contexts)
             (not (ty$instance? goal-obj 'action)))
        (progn
         (ndbg-roman-nl *gate-dbg* rule "Try fact plans")
         (setq sprouted-contexts (run-fact-plan goal top-level-goal
                                               context belief-path))))
    ;
    ; Otherwise, try analogical plans or generic rules, depending.
    ;
    ; Get candidate rules and episodes
 (if (null? sprouted-contexts)
     (progn
  (ndbg-roman-nl *gate-dbg* rule "Try rules and episodes")
  (setq candidates (find-candidate-rules goal-obj nil
                                        belief-path context))
  ; If there are any episodes whose ordering is greater than
  ; 0.0, go for analogical planning; otherwise use generic
  ; planning.
  ; (Since leaf goals shouldn't be indexed, these analogical plans
  ; WILL go through, and so we don't need to have generic plans
  ; take over if no analogical plans fire).
  (if (order-candidates goal-obj candidates top-level-goal)
      (yloop (yfor episode in (candidates->episodes candidates))
            (ydo (setq sprouted-contexts
                     (append!
                     (run-analogical-plan goal goal-obj context
                                     (ob$get episode 'bd)
                                     (ob$get episode 'goal)
                                     (ob$get episode 'context)
                                     (ob$get episode 'rule)
                                     (ob$get episode 'ordering)
                                     belief-path top-level-goal
                                     episode t)
                      sprouted-contexts))))
      ; Rules generated automatically from input episodes are not used in
      ; generic planning (but other ones, e.g., from reversal, may be?)
      (yloop (yfor candidate in candidates)
            (ydo (if (not (constructed-plan? (candidate-rule candidate)))
                    (progn
                     (setq sprouted-contexts
                          (append!
                           (run-generic-plan goal goal-obj context
                                             (candidate-rule candidate)
                                             belief-path nil
                                             (candidate-bd candidate)
                                             top-level-goal)
                           sprouted-contexts)))))))))
    ;
    ; If still no luck, try believe others.
    ;
    (if (null? sprouted-contexts)
        (progn
         (ndbg-roman-nl *gate-dbg* rule "Try believe others")
        (if (and (ty$instance? goal-obj 'believe)
                 (neq? (car belief-path) (ob$get goal-obj 'actor))
                 (ty$instance? (ob$get goal-obj 'obj) 'mental-state))
;                 (or (ty$instance? (ob$get goal-obj 'obj)
;                                   'mental-state)
;                     (ty$instance? (ob$get goal-obj 'obj)
;                                   'personal-attribute))
            (let ((new-goal-obj (ob$get goal-obj 'obj)))
             (yloop
              (initial (believe-other? (ob$get goal-obj 'actor)))
              (yfor rule in (collect-planning-rules new-goal-obj))
              (ydo (if (plan? rule)
                       (progn
                        (setq sprouted-contexts
                              (append!
                               (try-generic-plan goal new-goal-obj context
                                rule belief-path believe-other?
                                top-level-goal)
                              sprouted-contexts))))))))))
    ;
    ; Eventually apply analogical plans in a relaxed way here?
    ;
    ; If still no luck, employ reality relaxation to a high degree
    ; and assume goal succeeds
    ;
    (if (and (null? sprouted-contexts)
             (progn
              (ndbg-roman-nl *gate-dbg* rule "Try subgoal relaxation")
              t)
             (not (top-goal? goal context belief-path))
             (or (not (real? top-level-goal))
                 (not (vars-in? goal-obj)))  ; for now...
             (not (ty$instance? goal-obj 'action))
             (not (ty$instance? goal-obj 'minimization))
             (me-belief-path? belief-path)
             ; Can only relax goals involving other actors than me for
             ; real planning.
             (cond
              ((or (real? top-level-goal)
                   (imaginary-realistic? top-level-goal))
               (if (ob? (ob$pget goal '(obj actor)))
                   (if (and (ob$pget goal '(obj actor))
                            (not-me? (ob$pget goal '(obj actor))))
                       t nil)
                   t))
              (else ;(imaginary-fanciful? top-level-goal)
               t)))
        (let ((sprouted-context (cx$sprout context)))
         (delay-dbgs sprouted-context
          (set-ordering sprouted-context 0.1)
          (ndbg-roman *gate-dbg* rule "Subgoal relaxation")
          (ndbg-roman-nl *gate-dbg* rule ", ~A succeeds" goal)
          (ndbg-newline *gate-dbg* rule)
          (gen-relaxation sprouted-context
             (cx$assert-relative sprouted-context goal-obj belief-path))
             ; Can only do below if goal is fully instantiated.
            (setq goal (make-goal-success goal sprouted-context nil belief-path
                               *empty-bd*)) ; could just wait..
            ; Todo: Have to copy goal objective!! This setting will affect
            ; other contexts! (If make-goal-success does deep copy, however,
            ; we are OK).
            (ob$pset goal '(obj strength) *goal-relaxation-realism*))
         (setq sprouted-contexts (list sprouted-context))))
    sprouted-contexts)
      nil))

(defun planning-loop? (goal context top-level-goal belief-path)
  (ndbg-roman-nl *gate-dbg* rule-xtra "Plan loop check ~A" goal)
  (let ((pl?
         (mem? (lambda (x y)
                       (and (not (vars-in? (ob$get x 'obj)))
                            (not (vars-in? (ob$get y 'obj)))
                            (ob$unify (ob$get x 'obj) (ob$get y 'obj)
                                      *empty-bd*)))
                   goal (cdr (goal-supergoals goal context belief-path)))))
       (if pl?
           (ndbg-roman-nl *gate-dbg* rule "Is loop.")
           (ndbg-roman-nl *gate-dbg* rule-xtra "Is not loop."))
       pl?))

(defun ob$instantiate-o (ob bd)
  (ob$instan-omit ob bd #'varize-object? nil *link-slots* nil))

; subgoal-objs should be in correct forward seq order
;
; plan-no-gen :
;      nil = do generate subgoal always
;        t = never generate subgoal
; activate = generate only when activated
;  outcome = generate only on outcome
;
(defun ga-gen-on-activate? (gen-advice)
  (or (null? gen-advice)
      (eq? gen-advice 'activate)))

(defun ga-gen-on-outcome? (gen-advice)
  (or (null? gen-advice)
      (eq? gen-advice 'outcome)))

(defun instan-and-activate-subgoals (goal subgoal-objs bd rule
                                      sprouted-context seq? analogical-subgoals
                                      believe-other? top-level-goal belief-path)
  (ndbg-roman-nl *gate-dbg* rule "Instantiate and activate subgoals")
  (yloop (initial (subgoalnum 0)
                 (active-subgoals nil)
                 (analogical-subgoal nil)
                 (plan-no-gen (ob$get rule 'plan-no-gen))
                 (gen-advice nil))
        (yfor subgoal-obj in subgoal-objs)
        (ydo (if analogical-subgoals
                (setq analogical-subgoal (car analogical-subgoals)))
            (if plan-no-gen
                (progn
                 (setq gen-advice (car plan-no-gen))
                 (setq plan-no-gen (cdr plan-no-gen))))
            ; Todo: we should uniquify any unbound variables
            ; accumulatively through instantiation.
            (setq subgoal-obj (ob$instantiate-o subgoal-obj bd))
            (if (ty$instance? subgoal-obj 'rnot)
                (ob$set subgoal-obj 'type *not-ob*))
            (if believe-other?
                (setq subgoal-obj (ob$fcreate
                                  `(BELIEVE actor ,believe-other?
                                            obj ,subgoal-obj)))
                (ob$set subgoal-obj 'plan-rule rule))
            (ob$set subgoal-obj 'plan-subgoalnum subgoalnum)
            (setq active-subgoals
                 (cons (activate-subgoal goal
                                         subgoal-obj
                                         sprouted-context
                                         rule analogical-subgoal seq?
                                         belief-path top-level-goal gen-advice)
                       active-subgoals))
            (if analogical-subgoals
                (setq analogical-subgoals (cdr analogical-subgoals)))
            (setq subgoalnum (+ 1 subgoalnum)))
        (yresult 
         (progn
          (setq active-subgoals (reverse active-subgoals))
          (if seq? (make-seq active-subgoals))
          active-subgoals))))
; Above is reverse because subgoals come in forward and we cons, which
; reverses, so we have to reverse again.

(defun activate-subgoal (goal subgoal-obj context rule analogical-subgoal
                          seq? belief-path top-level-goal gen-advice)
  (ndbg-roman-nl *gate-dbg* rule "Activate subgoal for ~A obj ~A in ~A"
        goal subgoal-obj context)
  (ndbg-roman-nl *gate-dbg* rule-xtra "Analogical subgoal = ~A"
                 analogical-subgoal)
  (let ((analogical-links? (if analogical-subgoal
                               (goal-intends-links-uo
                                analogical-subgoal
                                (ob$get analogical-subgoal 'top-context)
                                *me-belief-path*)
                               nil)))
       ; The below is redundant
;       (if (ty$instance? subgoal-obj 'rnot)
;           (ob$set subgoal-obj 'type *not-ob*))
       (if (and analogical-subgoal (null? analogical-links?))
           (ndbg-roman-nl *gate-dbg* rule
                 "Analogical plan for ~A in ~A will bottom out"
                 goal context))
       (let* ((subgoal (if (and analogical-subgoal analogical-links?)
                           (ob$fcreate `(ACTIVE-GOAL
                                          obj ,subgoal-obj
                                          top-level-goal ,top-level-goal
                                          activation-context
                                          ,context
                                          analogical-episode
                                          ,(ob$get analogical-subgoal
                                                    'episode)))
                           (ob$fcreate `(ACTIVE-GOAL
                                          top-level-goal ,top-level-goal
                                          activation-context
                                          ,context
                                          obj ,subgoal-obj))))
              (intends (if seq?
                           (ob$fcreate `(INTENDS
                                          linked-from ,goal linked-to ,subgoal
                                          rule ,rule seq? 't))
                           (ob$fcreate `(INTENDS
                                          linked-from ,goal linked-to ,subgoal
                                          rule ,rule)))))
             (if gen-advice
                 (ob$set subgoal 'gen-advice gen-advice))
         (if (preservation-goal-subgoal? goal)
             (progn
              (ndbg-roman-nl *gate-dbg* rule-xtra "Is preservation subgoal")
              (ob$set subgoal 'preservation-subgoal? t)))
         (if (or (null? (ga-gen-on-activate? gen-advice))
                 (cx$retrieve-relative context subgoal-obj belief-path))
             (no-gen (cx$assert-relative context intends belief-path)
                     (cx$assert-relative context subgoal belief-path))
             (progn
              (cx$assert-relative context intends belief-path)
              (cx$assert-relative context subgoal belief-path)))
         subgoal)))

(defun planner-empty-bd (belief-path)
  (bd-bind 'self (car belief-path) *empty-bd*))

; Now incorporates bindings from retrieved deletes.
; Is currently only called for believe others.
(defun try-generic-plan (goal goal-obj context rule belief-path believe-other?
                          top-level-goal)
;  (ndbg-roman-nl *gate-dbg* rule "Try generic plan for ~A obj ~A in ~A"
;                 goal goal-obj context)
  (yloop 
   (initial (sprouted-contexts nil))
   (yfor bd in (rule-applications goal-obj context rule belief-path
                                 believe-other?))
   (ydo (setq sprouted-contexts
            (append! (run-generic-plan goal goal-obj context rule
                                       belief-path believe-other?
                                       bd top-level-goal)
                     sprouted-contexts)))
   (yresult sprouted-contexts)))

(defun rule-applications (goal-obj context rule belief-path believe-other?)
;  (ndbg-roman-nl *gate-dbg* rule "Find rule applications for ~A obj ~A in ~A"
;        rule goal-obj context)
  (let* ((rule-goal-obj (ob$get rule 'goal))
         (rule-initial (ob$get rule 'initial))
         (rule-tlg (ob$get rule 'top-level-goal))
         (bd (if believe-other?
                 (bd-bind 'self believe-other? *empty-bd*)
                 (planner-empty-bd belief-path)))
         (temp nil))
        (if (self-type-ok? rule (bd-lookup 'self bd))
            (progn
             (setq bd (ob$unify-cx rule-goal-obj goal-obj bd context))
             (if (and bd
                      rule-tlg
                      (null? (ob$unify rule-tlg
                                       (ob$get *top-level-goal* 'obj)
                                       bd)))
                 (setq bd nil))
             (if bd
                 (if rule-initial
                     (progn
                      (setq temp (show rule-initial context bd belief-path))
                      ; was (cx$retrieve-bd context rule-initial bd)
                      (if temp (list (car (car temp))) nil))
; Initial must be satisfied; was (list bd) for optional satisfaction
                     ; put ndbgs in
                     (list bd))
                 nil))
            nil)))

; add stripping to goal-obj in case of believe-other?
;
; ALL subgoals are activated, whether true or not, so that they get into the
; planning rule (used by analogical planning). Unbound variables are later
; concretized by fact planning or analogical planning.
(defun run-generic-plan (goal goal-obj context rule belief-path believe-other?
                          bd top-level-goal)
  (ndbg-add-item rule)
  (ndbg-roman-nl *gate-dbg* rule "Run generic plan ~A for ~A in ~A"
                 rule goal context)
  (let ((rule-subgoal-obj (ob$get rule 'subgoal))
;        (rule-goal-obj (ob$get rule 'goal)) never used
        (new-goal nil)
        (sprouted-context nil) (sprouted-contexts nil))
       (cond
        ((ty$instance? rule-subgoal-obj 'RCODE)
         (let ((old-ob-bindings *ob-bindings*))
              ; The lambda object of RCODE is responsible for:
              ; - Taking arguments (goal context top-level-goal rule bd)
              ; - Returning a list of sprouted contexts (yes, RCODE, a
              ;   single rule may actually cause several plans to
              ;   be generated, e.g., Reversal-Plan does this)
              ; - Creating INTENDS links in each sprouted context
              ; - Setting the ordering for each sprouted context
              ;   (using [set-ordering sprout value])
              ; - Linearization using the following code:
              ; (delay-dbgs sprouted-context
          ;      (rule-fire-msg rule "plan" context bd sprouted-context goal)
              ;      (ndbg-roman-nl *gate-dbg* rule "Coded plan")
              ;      <other stuff that produces rule output>)
              ;
;              (intends (ob$fcreate `(INTENDS linked-from ,goal
;                                               rule ,rule)))
;             (setq sprouted-context (cx$sprout context))
;             (set-ordering sprouted-context
;                            (ob$get rule 'plausibility))
              (setq *ob-bindings* bd)
              (setq sprouted-contexts
                   (eval (ob$get rule-subgoal-obj 'obj)))
; (was) to stop later firing
;             (cx$assert-relative sprouted-context intends belief-path)
              (setq *ob-bindings* old-ob-bindings)))
        ((ty$instance? rule-subgoal-obj 'ROR)
         (error "Planning rule ~A has ROR as subgoal--not allowed" rule))
        (else
         (setq sprouted-context (cx$sprout context))
         (delay-dbgs sprouted-context
          (cond
           (believe-other?
            (rule-fire-msg rule "backward vicarious plan"
                           context bd sprouted-context goal))
           ((me-belief-path? belief-path)
            (rule-fire-msg rule "plan" context bd sprouted-context goal))
           (else
            (rule-fire-msg rule "forward vicarious plan"
                           context bd sprouted-context goal)))
            (set-ordering sprouted-context (ob$get rule 'plausibility))
            ; The below modification enables variable instantiation
            ; from the goal unification. Iynwim.
            (if (vars-in? goal-obj)
;              (any? (lambda (var) (bd-lookup (variable-name var) bd))
;                       (variables-in goal-obj))
                (setq new-goal (or (plan-instantiate goal
                                                    (bd-no-var-bds bd)
                                                    sprouted-context
                                                    top-level-goal belief-path
                                                    nil)
                                  goal))
                (setq new-goal goal))
            (instan-and-activate-subgoals new-goal (rule-subgoal-objs rule) bd
                                          rule sprouted-context
                                          (ty$instance? rule-subgoal-obj 'rseq)
                                          nil believe-other? top-level-goal
                                          belief-path))
         (setq sprouted-contexts (list sprouted-context))))
  (if (and (ob$get rule 'halt?)
           (not (dd-goal? top-level-goal)))
      (ob$set top-level-goal 'status 'fired-halt))
  (ndbg-remove-item rule)
  sprouted-contexts))

(defun bd-no-var-bds (bd)
  (let ((new-bd nil)
        (val nil))
       (bd-walk
        (lambda (var)
                (setq val (bd-hyper-lookup var bd))
                (if (and val (not (var? val)))
                    (setq new-bd (cons (list var val) new-bd))))
        bd)
       (cons 't new-bd)))
;
; The code has to special case on goal activation rules to not
; initiate a goal if it is already satisfied (by why would it be initiated
; if already satisfied?)
;
; Perhaps reality scaling should be done on the inference side, rather
; than the planning side. However, this doesn't quite feel right.
; Or on both sides? (then make the numbers higher?)

(defun run-inferences (context top-level-goal belief-path)
  (ndbg-roman-nl *gate-dbg* rule "Run inferences in ~A, bp = ~A"
                 context belief-path)
  (yloop
   (initial (ever-fired? nil))
   (ywhile (run-inferences1 context top-level-goal belief-path))
   (ydo (setq ever-fired? t))
   (yresult ever-fired?)))

(defun dbg-bo (rule goal context)
  (let ((goal-obj (ob$get goal 'obj))
        (other (ob$get  goal 'actor))
        (result nil))
   (trace ob$unify0)
   (setq result (rule-applications goal-obj context rule *me-belief-path* other))
   (untrace ob$unify0)
   result))

(defun dbg-prule (rule goal-obj context)
  (let ((result nil))
       (trace ob$unify0)
       (setq result (rule-applications goal-obj context rule
                                      *me-belief-path* nil))
       (untrace ob$unify0)
       result))

(defun dbg-prulex (rule goal-obj context)
  (rule-applications goal-obj context rule *me-belief-path* nil))

(defun dbg-irule (rule context)
  (let ((show-results nil))
       (trace ob$unify0)
       (setq show-results (show (ob$get rule 'subgoal)
                               (if (ob$get rule 'reality-subgoal)
                                   *reality*
                                   context)
                               (planner-empty-bd *me-belief-path*)
                               *me-belief-path*))
       (untrace ob$unify0)
       show-results))

(defun dbg-irulex (rule context)
  (show (ob$get rule 'subgoal)
        (if (ob$get rule 'reality-subgoal) *reality* context)
        (planner-empty-bd *me-belief-path*)
        *me-belief-path*))

(defun run-inferences1 (context top-level-goal belief-path)
  (let ((assertions (beliefs-of (ob$get context 'touched-facts) belief-path)))
   (ndbg-roman-nl *gate-dbg* rule-long "Inference cycle in ~A" context)
   (if (me-belief-path? belief-path) (ob$removes context 'touched-facts))
   (yloop
    (initial (show-results nil) (ctxt nil) (rule-fired? nil)
             (already-inferred nil))
    (yfor rule in (collect-inference-rules assertions))
    (ydo
(if (and (self-type-ok? rule (car belief-path))
         (or (null? *top-level-goal*)
             (null? (ob$get rule 'top-level-goal))
             (ob$unify (ob$get rule 'top-level-goal)
                       (ob$get *top-level-goal* 'obj)
                       (planner-empty-bd belief-path))))
    (progn
     (ndbg-add-item rule)
     (ndbg-roman-nl *gate-dbg* inference "Considering rule ~A" rule)
     (if (global-inference? rule) ; This feature is never used?
         (setq ctxt *reality*)
         (setq ctxt context))
     (if (and (inference? rule)
              (setq show-results
                   (show (ob$get rule 'subgoal)
                         (if (ob$get rule 'reality-subgoal) *reality* context)
                         (planner-empty-bd belief-path) belief-path)))
         (progn
          ; Below is moved from before if.
          (setq already-inferred (facts-inferred-by rule ctxt belief-path))
         (yloop (yfor show-result in show-results)
               (ydo
                (ndbg-roman-nl *gate-dbg* inference
                               "Considering show result ~A" show-result)
                (if (not (already-inferred? show-result ctxt
                                            already-inferred belief-path
                                            rule))
                    (progn
                     (setq rule-fired?
                          (or (inference-fire context ctxt top-level-goal
                                              belief-path rule show-result)
                              rule-fired?))))))))
    (ndbg-remove-item rule))))
   (yresult rule-fired?))))

(defun inference-fire (context ctxt top-level-goal belief-path
                        rule show-result)
  (ndbg-roman-nl *gate-dbg* inference "Inference fire")
  (let ((assertion nil)
        (fired? nil))
   (yloop
    (yfor elem in (ob$gets rule 'goal))
    (ydo (setq assertion (ob$instan-strength elem (car show-result)))
        (if (ty$instance? assertion 'rcode)
            (progn
             (possible-fired-msg fired? rule context show-result
                                 belief-path)
             (setq fired? t)
             (inference-rcode assertion show-result))
            (setq fired? (or (inference-assert assertion ctxt top-level-goal
                                              belief-path rule show-result
                                              fired?)
                            fired?)))))
   ; Too bad we can't do retracts first, so they don't retract assertions.
   ; (But we need the complex fire criteria from above.)
   (if fired?
       (yloop
        (yfor elem in (ob$gets rule 'delete))
        (ydo (inference-retract (ob$instantiate-o elem (car show-result)) ctxt
                               belief-path))))
    fired?))

(defun possible-fired-msg (old-fired? rule context show-result bp)
  (if (null? old-fired?)
      (if (me-belief-path? bp)
          (rule-fire-msg rule "inference" context (car show-result) nil nil)
          (rule-fire-msg rule "forward vicarious inference"
                         context (car show-result) nil nil))))

(defun inference-retract (ob ctxt belief-path)
  (ndbg-roman-nl *gate-dbg* rule "Inference retract ~A in ~A" ob ctxt)
  (let ((found (cx$retrieve ctxt (relative->absolute ob belief-path))))
     (if (null? found)
         (ndbg-roman-nl *gate-dbg* rule
                        "Inference-retract: ~A already false in ~A" ob ctxt)
         (let ((leafs (get-leaf-causes (car (car found)) ctxt)))
              (ndbg-roman-nl *gate-dbg* rule "Leafs = ~A" leafs)
              (yloop
               (initial (found? nil))
               (yfor leaf in leafs)
               (ydo (if (ty$instance? leaf 'action)
                       (progn
                        (retract-dependencies leaf ctxt)
                        (setq found? t))))
               (yresult (if (null? found?)
                           (cx$retract ctxt (car (car found))))))))))

; Alternatively, could maintain 'invalidated' justifications for
; beliefs.
(defun retract-dependencies (ob ctxt)
  (ndbg-roman-nl *gate-dbg* rule "Retracting dependencies of ~A in ~A"
                 ob ctxt)
  (if (and (not (ty$instance? ob 'believe))
           (not (ty$instance? ob 'goal)))
      (progn
       (yloop
        (initial (dependee nil))
        (yfor d-link1 in (get-links ob *dependency-ob* ctxt))
        (ydo 
         (setq dependee (ob$get d-link1 'linked-to))
         (yloop (yfor d-link2 in (get-links-from dependee *dependency-ob* ctxt))
               (ydo (cx$retract ctxt d-link2)))
         (retract-dependencies dependee ctxt)))
       (cx$retract ctxt ob))
      (progn
       (ndbg-roman-nl *gate-dbg* rule "Bottoms out at ~A" ob)
       nil)))

; For the case of an ACTIVE-GOAL whose object has a UPROC strength.
(defun ob$instan-strength (ob bd)
  (if (and (ty$instance? ob 'active-goal)
           (ob? (ob$get (ob$get ob 'obj) 'strength)))
      (let ((strength-val (ob$get (ob$get ob 'obj) 'strength))
            (result (ob$instantiate-o ob bd)))
           (set-strength (ob$get result 'obj) strength-val)
           result)
      (ob$instantiate-o ob bd)))

; Note that NOTs are not asserted in the context, so dependencies
; point to NOTs not in the context
(defun inference-rcode (assertion show-result)
  (let ((old-ob-bindings *ob-bindings*))
       (setq *ob-bindings* (car show-result))
       (ndbg-roman-nl *gate-dbg* rule "Executing coded inference")
       ; actually, code should work for inferences and sprouters both.
       ; They should both take a context arg.
       (eval (ob$get assertion 'obj))
       (setq *ob-bindings* old-ob-bindings)))

(defun empty-bd-in (retrieved)
  (any (lambda (x) (if (empty-bd? x) x nil)) retrieved))

(setq *inf-fire-thresh* 0.8) ; Todo: there must be a better way
; of deciding when a new instantiation of an inference should
; be allowed to fire.

; Todo: Note, if another inference sums into an existing assertion,
; that inference will fire over and over again, because no note
; is made that that fact was also inferred from that inference.
(defun inference-assert (assertion ctxt top-level-goal belief-path
                         rule show-result fired?)
  (ndbg-roman-nl *gate-dbg* inference "Inference assert")
  (let ((temp nil))
   (cond
    ; Activate new top-level self goal
    ((and (or (null? top-level-goal) (dd-goal? assertion)
              (eq? (ob$get top-level-goal 'planning-type) 'real)
              ) ; end or
          (me-belief-path? belief-path)
          (ty$instance? assertion 'active-goal)
          (not (ty$instance? (ob$get assertion 'obj) 'preservation)))
     (if (not (top-level-goal-exists? assertion))
         (progn
          (possible-fired-msg fired? rule ctxt show-result belief-path)
          (ob$set assertion 'inference-rule rule) ; necessary?
          (activate-top-level-goal assertion ctxt (car show-result) rule)
          t)
         (progn
          (ndbg-roman-nl *gate-dbg* inference
                         "Ignoring duplicate tlg inference")
          nil)))
    ((setq temp (empty-bd-in (cx$retrieve-relative ctxt assertion belief-path)))
     (setq assertion (car temp))
     (ndbg-roman-nl *gate-dbg* inference "Retrieved ~A" assertion)
     (if (fl< (strength assertion) *inf-fire-thresh*)
         (progn
          (possible-fired-msg fired? rule ctxt show-result belief-path)
          (ob$add assertion 'inference-rule rule)
          
          
          (print "doing make-dependency in dd_rule2.cl, line 854")
          
          
          (make-dependency (cdr show-result) assertion rule ctxt belief-path
                           (ob$get rule 'plausibility) (car show-result))
          t)
         nil))
    ; Other goal activation
    ((and (ty$instance? assertion 'believe)
          (not-me? (ob$get assertion 'actor))
          (ty$instance? (ob$get assertion 'obj) 'active-goal))
     (if (not (other-goal-exists? (ob$get assertion 'actor)
                                  (ob$pget assertion '(obj obj))
                                  ctxt))
         (progn
          (possible-fired-msg fired? rule ctxt show-result belief-path)
          (ob$set assertion 'inference-rule rule)
          (activate-other-top-goal (ob$get assertion 'obj) top-level-goal
                                   ctxt (->belief-path
                                         (ob$get assertion 'actor)))
          
          
          
          (print "doing make-dependency in dd_rule2.cl, line 879")
          
          
          (make-dependency (cdr show-result) assertion rule ctxt belief-path
                           (ob$get rule 'plausibility) (car show-result))
          t)
         (progn
          (ndbg-roman-nl *gate-dbg* inference
                         "Ignoring duplicate other goal inference")
          nil)))
    ; Goal inferences.
    ((ty$instance? assertion 'goal)
     (if (not (goal-exists? assertion ctxt))
         (progn
          (possible-fired-msg fired? rule ctxt show-result belief-path)
          (ob$set assertion 'inference-rule rule)
          (ob$add assertion 'top-level-goal top-level-goal)
          (cx$assert-relative ctxt assertion belief-path)
          
          
          (print "doing make-dependency in dd_rule2.cl, line 899")
          
          
          (make-dependency (cdr show-result) assertion rule ctxt belief-path
                           (ob$get rule 'plausibility) (car show-result))
          (if (and (me-belief-path? belief-path)
                   (or (ty$instance? assertion 'succeeded-goal)
                       (ty$instance? assertion 'failed-goal))
                   (personal-goal? assertion))
              (personal-goal-outcome assertion ctxt top-level-goal))
          t)
         (progn
          (ndbg-roman-nl *gate-dbg* inference
                         "Ignoring duplicate goal inference")
          nil)))
    (else
     (possible-fired-msg fired? rule ctxt show-result belief-path)
     (ob$set assertion 'inference-rule rule)
; as yet, is only a global indication--can't specify to
; gen some goals and not others.
     (if (ob$get rule 'inf-no-gen)
         (no-gen (cx$assert-relative ctxt assertion belief-path))
         (cx$assert-relative ctxt assertion belief-path))
          
          
          (print "doing make-dependency in dd_rule2.cl, line 924")
          
          
     (make-dependency (cdr show-result) assertion rule ctxt belief-path
                      (ob$get rule 'plausibility) (car show-result))
     t))))

(setq *exists-ignores* '(strength top-level-goal linked-from-of
                                  linked-to-of termination-context))

(defun top-level-goal-exists? (goal)
  (mem (lambda (x y) (and (tlg? y *reality*)
                          (ob$unify1 (ob$get x 'obj) (ob$get y 'obj)
                                     *empty-bd* *exists-ignores*)
                          (not (consider-as-new-goal? x y))))
       goal
       (append (cx$get-all-ty *reality-lookahead* *succeeded-goal-ob*)
               (cx$get-all-ty *reality-lookahead* *failed-goal-ob*)
               (cx$get-all-ty *reality-lookahead* *active-goal-ob*))))

;  (or (mem (lambda (x y) (ob$unify1 x (ob$get y 'obj) *empty-bd*
;                                    *exists-ignores*))
;           (ob$get goal 'obj)
;           *top-level-goals*)
;      (mem (lambda (x y) (and (tlg? y *reality*)
;                              (ob$unify1 x (ob$get y 'obj) *empty-bd*
;                                         *exists-ignores*)))
;           (ob$get goal 'obj)
;           (cx$get-all-ty *reality* *succeeded-goal-ob*))
;      (mem (lambda (x y) (and (tlg? y *reality*)
;                              (ob$unify1 x (ob$get y 'obj) *empty-bd*
;                                        *exists-ignores*)))
;           (ob$get goal 'obj)
;           (cx$get-all-ty *reality* *failed-goal-ob*))))

(defun goal-exists? (goal context)
  (if (ty$instance? goal 'p-goal)
      ; then
      (mem (lambda (x y) (ob$unify1 x (ob$get y 'obj) *empty-bd*
                                    *exists-ignores*))
           (ob$get goal 'obj)
           (cx$get-all-ty context *active-p-goal-ob*))
      ; else
      (or (top-level-goal-exists? goal)
          (mem (lambda (x y) (and (ob$unify1 (ob$get x 'obj) (ob$get y 'obj)
                                             *empty-bd* *exists-ignores*)
                                  (not (consider-as-new-goal? x y))))
               goal
               (append (cx$get-all-ty context *succeeded-goal-ob*)
                       (cx$get-all-ty context *failed-goal-ob*)
                       (cx$get-all-ty context *active-goal-ob*))))))

(defun consider-as-new-goal? (new-goal old-goal)
;  Don't reactivate succeeded or failed goals.
;  (not (and (ty$instance? new-goal ^active-goal)
;            (or (ty$instance? old-goal ^succeeded-goal)
;                (ty$instance? old-gaol ^failed-goal))))
  (and (neq? (ob$ty new-goal) (ob$ty old-goal))
       (if (ty$instance? new-goal 'active-goal)
           (not (dd-goal? old-goal))
           t)))

(defun other-goal-exists? (actor goal-obj context)
  (any?
   (lambda (belief)
           (and (eq? actor (ob$get belief 'actor))
                (ty$instance? (ob$get belief 'obj) 'goal)
                (ob$unify1 goal-obj (ob$pget belief '(obj obj))
                           *empty-bd* *exists-ignores*)))
   (cx$get-all-ty context *believe-ob*)))

(defun activate-other-top-goal (goal top-level-goal context belief-path)
  (ndbg-roman-nl *gate-dbg* rule "******************")
  (ndbg-roman *gate-dbg* rule "Activate top-level goal")
  (ndbg-roman *gate-dbg* rule " ~A for ~A in ~A" goal (car belief-path)
              context)
  (ndbg-newline *gate-dbg* rule)
  (cx$assert-relative context goal belief-path)
  (ob$add goal 'top-level-goal top-level-goal))

; In theory, bindings would have to be checked also (since there are
; several possible mappings). But this is sufficient for the cases
; which have arisen so far.
;
; There can only be one set of antecedents connected to a consequent on
; behalf of a given rule (in accordance with above assumption).
;
(defun already-inferred? (show-result context already-inferred
                           belief-path rule)
  (yloop (initial (result nil))
        (yfor fact in already-inferred)
        (yuntil result)
        (ydo (if (same-as-show-result (reverse
                                      (ol-get-relative-rule fact *dependency-ob*
                                                            'backward context
                                                            belief-path rule))
                                     show-result)
                (setq result t)))
        (yresult
         (progn
          (if (null? result)
              (ndbg-roman-nl *gate-dbg* inference
                             "Not already inferred; show = ~A, facts = ~A in ~A"
                             show-result already-inferred context)
              (ndbg-roman-nl *gate-dbg* inference
                             "Already inferred; show = ~A, facts = ~A in ~A"
                             show-result already-inferred context))
          result))))

(defun collect-inference-rules (assertions)
  (if (< (length assertions) 10)
   (ndbg-roman-nl *gate-dbg* rule-long "Collecting inferences rules for ~A"
                  assertions)
   (ndbg-roman-nl *gate-dbg* rule-long
                  "Collecting inferences rules for many assertions"))
  (yloop (initial (result nil)
                 (temp nil))
        (yfor assertion in assertions)
        (yuntil (eq? result *rules*))
        (ydo (setq temp (forward-chain-rules assertion))
            (if (eq? temp *rules*)
                (setq result *rules*)
                (setq result (union result temp))))
        (yresult 
         (progn
          (if (eq? result *rules*)
              (ndbg-roman-nl *gate-dbg* rule-long "Using full rule set")
              (ndbg-roman-nl *gate-dbg* rule-long "Collected rules = ~A"
                             result))
          result))))

(defun slow-facts-inferred-by (rule context belief-path)
  (yloop (initial (result nil))
        (yfor ob in (cx$get-all context))
        (ydo (setq ob (absolute->relative ob belief-path))
            (if (any? (lambda (x) (eq? rule (ob$get x 'rule)))
                      (get-dependencies ob context belief-path))
                (setq result (cons ob result))))
        (yresult result)))

(defun facts-inferred-by (rule context belief-path)
  (yloop (initial (result nil))
        (yfor ob in (cx$get-all context))
        (ydo (setq ob (absolute->relative ob belief-path))
            (if (and ob (memq? rule (ob$gets ob 'inference-rule)))
                (setq result (cons ob result))))
        (yresult result)))

(defun beliefs-of (facts belief-path)
  (yloop (initial (result nil)
                 (temp nil))
        (yfor fact in facts)
        (ydo (if (setq temp (absolute->relative fact belief-path))
                (setq result (cons temp result))))
        (yresult result)))

(defun same-as-show-result (lst show-result)
  (if (= (length lst) (length (cdr show-result)))
      (yloop
          (initial (result t)
                   (l lst)
                   (s (cdr show-result)))
          (ywhile l)
            (yuntil (null? result))
            (ydo (if (neq? (car l) (caar s))
                       (setq result nil))
               (setq l (cdr l))
               (setq s (cdr s)))
            (yresult result))
      nil))

; Let's not bother having others do p-goal planning for now.
(defun run-p-goals (context top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule "Running p-goals in ~A" context)
  (yloop (initial (sprouted-contexts nil))
        (yfor elem in (cx$get-all-ty context *active-p-goal-ob*))
        (ydo (if (not (has-link? elem 'linked-from-of *intends-ob* context))
                (setq sprouted-contexts
                     (append! (plan-p-goal elem context top-level-goal)
                              sprouted-contexts))))
        (yresult sprouted-contexts)))

;
; P-goal planning: so far this is only used in rationalization daydream.
;
(defun plan-p-goal (p-goal context top-level-goal)
  (ndbg-roman-nl *gate-dbg* rule "Planning for p-goal ~A in ~A" p-goal context)
  (yloop
   (initial (sprouted-context nil)
            (sprouted-contexts nil)
            (subgoal-obj nil))
   (yfor leaf-cause in (get-leaf-causes p-goal context))
   (ydo
    (ndbg-roman-nl *gate-dbg* rule-xtra "leaf-cause = ~A" leaf-cause)
    (if (ty$instance? leaf-cause 'not)
        (progn
         (setq sprouted-context (cx$sprout context))
         (setq sprouted-contexts (cons sprouted-context sprouted-contexts))
         (delay-dbgs sprouted-context
            (setq subgoal-obj (ob$copy (ob$get leaf-cause 'obj)))
            (ob$set subgoal-obj 'plan-subgoalnum 0)
            (activate-subgoal p-goal subgoal-obj sprouted-context
                              (ob$fcreate '(RULE constructed? 't))
                              nil t *me-belief-path* top-level-goal t)
            (set-ordering sprouted-context 1.0)))))
   (yresult sprouted-contexts)))

; End of file.
