;; 
;;  The play-loop procedure takes as its  arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang scheme

;; TWO-PLAYER PRISONER'S DILEMMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) 
           (print-out-results strat0 strat1 history0 history1 limit))
	  (else (let ((result0 (strat0 history0 history1))
		      (result1 (strat1 history1 history0)))
		  (play-loop-iter strat0 strat1 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
				  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
		  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results strat0 strat1 history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display (format "Player 1 averaged ~s points using ~s"
                     (* 1.0 (/ (car scores) number-of-games)) 
                     (object-name strat0)))
    (newline)
    (display (format "Player 2 averaged ~s points using ~s"
                     (* 1.0 (/ (cadr scores) number-of-games)) 
                     (object-name strat1)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
	   (list score0 score1))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
				     (+ (get-player-points 0 game) score0)
				     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define play-before-last cadr)
(define rest-of-plays cdr)

(define (number-of-plays history)
  (if (empty-history? history) 0
      (+ 1 (number-of-plays (rest-of-plays history)))))

(define (list-length list)
  (if (null? list) 0
      (+ 1 (list-length (cdr list)))))

(define (string-list=? list1 list2)
  (cond ((null? list1) #t)
        ((string=? (car list1) (car list2))
         (string-list=? (cdr list1) (cdr list2)))
        (else #f)))

(define (extract-entry play game-matrix)
  (if (string-list=? play (caar game-matrix))
      (car game-matrix)
      (extract-entry play (cdr game-matrix))))
 
;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
	  ((string=? (most-recent-play hist) test)
	   (+ (count-instances-of test (rest-of-plays hist)) 1))
	  (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
	(cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

;; EGALITARIAN is slower than others because it uses recursion
;; others run in constant time and space
;; EGALITARIAN grows linearly (in time and space)
;; with the number of rounds already played (i.e. size of history list)

(define (Egalitarian my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
           (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

;; Egalitarian uses iteration instead of pure recursion
;; as a result, it runs in linear time but constant space

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (or (empty-history? my-history) 
          (empty-history? (rest-of-plays my-history))
          (string=? "c" (most-recent-play other-history))
          (string=? "c" (play-before-last other-history)))
      "c" "d"))

;; EYE-FOR-TWO-EYES does just as well as EYE-FOR-EYE,
;; except against SPASTIC (due to the reduced likelihood of defection)

;; EYE-FOR-N-EYES defects iff opponent defected for the last n consecutive rounds

(define (EYE-FOR-N-EYES n)
  (lambda (my-history other-history)
    (cond ((= n 0) "d")
          ((null? my-history) "c")
          ((string=? "c" (most-recent-play other-history)) "c")
          (else ((EYE-FOR-N-EYES (- n 1))
                 (rest-of-plays my-history)
                 (rest-of-plays other-history))))))

;; GENERALIZED-SPASTIC has a given likelihood of cooperating
;; (e.g. (G-S 0.5) = SPASTIC, (G-S 1) = PATSY)

(define (GENERALIZED-SPASTIC coop-likelihood)
  (lambda (my-history other-history)
    (let ((rand (/ (random 100) 100.)))
      (if (< rand coop-likelihood) "c" "d"))))
  
(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (lambda (my-history other-history)
    (let ((phase (remainder (number-of-plays my-history) (+ freq0 freq1))))
      (if (< phase freq0) 
          (strat0 my-history other-history)
          (strat1 my-history other-history)))))

;; (make-rotating-strategy PATSY NASTY 10 9) crushes EGALITARIAN

;; make-higher-order-spastic cycles through a list of strategies

(define (make-higher-order-spastic playbook)
  (if (null? (cdr playbook))
      (car playbook)
      (make-rotating-strategy 
       (car playbook) (make-higher-order-spastic (cdr playbook)) 1 1)))

(define WTF (make-higher-order-spastic 
             (list PATSY EYE-FOR-EYE NASTY EGALITARIAN SPASTIC)))

;; gentle creates a "gentler" version of a strategy
;; that has some probability of changing a defect to a cooperate

(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (if (string=? "c" (strat my-history other-history)) "c"
         ((GENERALIZED-SPASTIC gentleness-factor) my-history other-history))))

(define SLIGHTLY-GENTLE-NASTY (gentle NASTY 0.1))
(define SLIGHTLY-GENTLE-EYE-FOR-EYE (gentle EYE-FOR-EYE 0.1))

;; round-robin takes a list of strategies and pits them all against each other

(define (generate-pairs playbook)
  (if (null? playbook) empty
      (append
       (map (lambda (x) (list (car playbook) x)) playbook)
       (generate-pairs (cdr playbook)))))

(define (round-robin playbook)
  (define (run-tournament match-list)
    (if (null? match-list) 
        (begin
          (newline) 
          (display "-----END OF TOURNAMENT-----"))
        (begin 
          (play-loop (caar match-list) (cadar match-list))
          (run-tournament (cdr match-list)))))
  (let ((match-list (generate-pairs playbook)))
    (run-tournament match-list)))
        
;; THREE-PLAYER PRISONER'S DILEMMA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; basic game procedures must be modified to account for three players

(define (play-loop-3p strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) 
           (print-out-results-3p 
            strat0 strat1 strat2 
            history0 history1 history2 limit))
	  (else (let ((result0 (strat0 history0 history1 history2))
		      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
		  (play-loop-iter strat0 strat1 strat2 (+ count 1)
				  (extend-history result0 history0)
				  (extend-history result1 history1)
                                  (extend-history result2 history2)
				  limit)))))
  (play-loop-iter strat0 strat1 strat2 0 
                  the-empty-history the-empty-history the-empty-history
		  (+ 90 (random 21))))

(define (print-out-results-3p 
         strat0 strat1 strat2 
         history0 history1 history2 number-of-games)
  (let ((scores (get-scores-3p history0 history1 history2)))
    (newline)
    (display (format "Player 1 averaged ~s points using ~s"
                     (* 1.0 (/ (car scores) number-of-games)) 
                     (object-name strat0)))
    (newline)
    (display (format "Player 2 averaged ~s points using ~s"
                     (* 1.0 (/ (cadr scores) number-of-games)) 
                     (object-name strat1)))
    (newline)
    (display (format "Player 3 averaged ~s points using ~s"
                     (* 1.0 (/ (caddr scores) number-of-games)) 
                     (object-name strat2)))
    (newline)))

(define (get-scores-3p history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
	   (list score0 score1 score2))
	  (else (let ((game (make-play (most-recent-play history0)
				       (most-recent-play history1)
                                       (most-recent-play history2))))
		  (get-scores-helper (rest-of-plays history0)
				     (rest-of-plays history1)
                                     (rest-of-plays history2)
				     (+ (get-player-points-3p 0 game) score0)
				     (+ (get-player-points-3p 1 game) score1)
                                     (+ (get-player-points-3p 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define (get-player-points-3p num game)
  (list-ref (get-point-list-3p game) num))

(define (get-point-list-3p game)
  (cadr (extract-entry game *game-association-list-3p*)))

(define *game-association-list-3p*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

;; here are some three-player strategies

(define (PATSY-3 my-hist other-hist1 other-hist2) "c")

(define (NASTY-3 my-hist other-hist1 other-hist2) "d")

(define (SPASTIC-3 my-hist other-hist1 other-hist2)
  (if (= (random 2) 0) "c" "d"))

(define (TOUGH-EYE my-hist other-hist1 other-hist2)
  (if (empty-history? my-hist) "c"
      (if (or (string=? "d" (most-recent-play other-hist1))
              (string=? "d" (most-recent-play other-hist2)))
          "d" "c")))

(define (SOFT-EYE my-hist other-hist1 other-hist2)
  (if (empty-history? my-hist) "c"
      (if (and (string=? "d" (most-recent-play other-hist1))
               (string=? "d" (most-recent-play other-hist2)))
          "d" "c")))
          
;; make-combined-strategies takes two strategies from the
;; two-player version of the game, playing one strategy against 
;; each opponent. it then uses a specified combination rule
;; to combine the two results into a single decision

(define (make-combined-strategies strat0 strat1 rule)
  (lambda (my-hist other-hist1 other-hist2)
    (let ((r1 (strat0 my-hist other-hist1))
          (r2 (strat1 my-hist other-hist2)))
      (rule r1 r2))))

(define (GOOD-COP-BAD-COP my-hist other-hist1 other-hist2)
  ((make-combined-strategies
    EYE-FOR-EYE EGALITARIAN
    (lambda (r1 r2) (if (= (random 2) 0) r1 r2)))
   my-hist other-hist1 other-hist2))

;; defining a new data structure, history-summary, will make it easy
;; to create a strategy that can *guess* what the opposing strategies are!

;; history-summary needs to keep track of what a given opponent does
;; based on what happened the previous round. what does a given opponent do
;; when both others defected? when one cooperated and one defected? etc.

(define (make-history-summary cc-behavior cd-behavior dd-behavior) 
  (list cc-behavior cd-behavior dd-behavior))
(define (cc-behavior history-summary) (car history-summary))
(define (cd-behavior history-summary) (cadr history-summary))
(define (dd-behavior history-summary) (caddr history-summary))

(define (make-behavior c-count d-count)
  (list c-count d-count (+ c-count d-count)))
(define (c-count behavior) (car behavior))
(define (d-count behavior) (cadr behavior))
(define (total-count behavior) (caddr behavior))

;; count-scenarios counts the number of occurrences for each scenario,
;; returning them in list form. forgive me - i couldn't find a better way to do it.

(define (count-scenarios hist0 hist1 hist2 ccc dcc ccd dcd cdd ddd)
  (if (or (empty-history? hist0) (empty-history? (cdr hist0)))
      (list ccc dcc ccd dcd cdd ddd)
      (let ((scenario (list (car hist0) (cadr hist1) (cadr hist2))))
        (cond ((string-list=? scenario (list "c" "c" "c"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                (+ ccc 1) dcc ccd dcd cdd ddd))
              ((string-list=? scenario (list "d" "c" "c"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc (+ dcc 1) ccd dcd cdd ddd))
              ((string-list=? scenario (list "c" "c" "d"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc dcc (+ ccd 1) dcd cdd ddd))
              ((string-list=? scenario (list "c" "d" "c"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc dcc (+ ccd 1) dcd cdd ddd))
              ((string-list=? scenario (list "d" "c" "d"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc dcc ccd (+ dcd 1) cdd ddd))
              ((string-list=? scenario (list "d" "d" "c"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc dcc ccd (+ dcd 1) cdd ddd))
              ((string-list=? scenario (list "c" "d" "d"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc dcc ccd dcd (+ cdd 1) ddd))
              ((string-list=? scenario (list "d" "d" "d"))
               (count-scenarios (rest-of-plays hist0)
                                (rest-of-plays hist1)
                                (rest-of-plays hist2)
                                ccc dcc ccd dcd cdd (+ ddd 1)))))))

(define (get-history-summary hist0 hist1 hist2)
  (let ((count-log (count-scenarios hist0 hist1 hist2 0 0 0 0 0 0)))
    (let ((ccc (list-ref count-log 0))
          (dcc (list-ref count-log 1))
          (ccd (list-ref count-log 2))
          (dcd (list-ref count-log 3))
          (cdd (list-ref count-log 4))
          (ddd (list-ref count-log 5)))
      (make-history-summary (make-behavior ccc dcc)
                            (make-behavior ccd dcd)
                            (make-behavior cdd ddd)))))

(define (get-probability-of-c history-summary)
  (define (probability-getter behavior)
    (if (zero? (total-count behavior)) empty
        (/ (c-count behavior) (* 1.0 (total-count behavior)))))
  (map probability-getter history-summary))

;; get-probability-of-c returns a list containing the probability of cooperation
;; in each scenario: two cooperating opponents, one of each, and two defecting opponents
;; this list of probabilities allows us to guess strategies

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f)
         ((and (null? (car actual-values)) (car expected-values)) #f)
         ((or (not (car expected-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (get-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (get-history-summary hist0 
                                                               hist1
                                                               hist2)))))

;; more advanced strategies based on this idea...

(define (DONT-SUFFER-FOOLS-GLADLY my-hist other-hist1 other-hist2)
  (cond ((< (number-of-plays my-hist) 10) 
         "c")
        ((and (could-he-be-a-fool? other-hist1 other-hist2 my-hist)
              (could-he-be-a-fool? other-hist2 other-hist1 my-hist))
         "d")
        (else "c")))

(define (soft-eye? hist0 hist1 hist2)
  (test-entry (list 1 1 0)
              (get-probability-of-c 
               (get-history-summary hist0 hist1 hist2))))

(define (BLACK-EYE my-hist other-hist1 other-hist2)        ;; Note that this strategy only works 
  (cond ((< (number-of-plays my-hist) 10)                  ;; if one opponent is using SOFT-EYE.
         (SPASTIC-3 my-hist other-hist1 other-hist2))      ;; If there are two of them, there will
        ((or (soft-eye? other-hist1 other-hist2 my-hist)   ;; never be a double-defect scenario,
             (soft-eye? other-hist2 other-hist1 my-hist))  ;; so we can't verify that either
         "d")                                              ;; is a SOFT-EYE! The strategy could
        (else "c")))                                       ;; be refined to account for this.

;; we can even do three-player round robin
;; WARNING: CPU intensive; keep your playbook small!

(define (generate-triplets playbook)
  (if (null? playbook) empty
      (let ((playbook-pairs (generate-pairs playbook)))
        (append
         (map (lambda (x) (cons (car playbook) x)) playbook-pairs)
         (generate-triplets (cdr playbook))))))

(define (round-robin-3p playbook)
  (define (run-tournament match-list)
    (if (null? match-list) 
        (begin
          (newline) 
          (display "-----END OF TOURNAMENT-----"))
        (begin 
          (play-loop-3p (caar match-list) (cadar match-list) (caddar match-list))
          (run-tournament (cdr match-list)))))
  (let ((match-list (generate-triplets playbook)))
    (run-tournament match-list)))

#|

(define playbook (list
     DONT-SUFFER-FOOLS-GLADLY
     PATSY-3
     SOFT-EYE
     BLACK-EYE))

(round-robin-3p playbook)

|#