#lang racket

(define nil empty)

(define (list-length list)
  (if (null? list) 0 (+ 1 (list-length (cdr list)))))

(define (make-increasing-list n start)
  (if (= n 0) nil
      (cons start (make-increasing-list (- n 1) (+ start 1)))))

(define (make-increasing-matrix rows columns start)
  (if (= rows 0) nil
      (cons (make-increasing-list columns start)
            (make-increasing-matrix (- rows 1) columns (+ start columns)))))

(define (transpose matrix)
  (if (or (null? matrix) (null? (car matrix))) nil
      (cons (map car matrix) (transpose (map cdr matrix)))))

(define (make-column-increasing-matrix rows columns start)
  (transpose (make-increasing-matrix columns rows start)))

(define (generate-interval a b)
  (if (= a b) nil
      (cons a (generate-interval (+ a 1) b))))

(define (divisible? b a)
  (if (zero? a) #f (zero? (remainder b a))))

(define (remove-divisible list a)
  (filter (lambda (x) (not (divisible? x a))) list))

(define (sieve list)
  (if (null? list) nil
      (cons (car list) (sieve (remove-divisible (cdr list) (car list))))))

(define (remove-it elt list)
  (define (different? a) (lambda (b) (not (= a b))))
  (filter (different? elt) list))

(define (reduce op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (reduce op init (cdr lst)))))

(define fold-right reduce)

(define (max a b) (if (> a b) a b))

(define (max-elt list)
  (fold-right max 0 list))

(define (sortem list)
  (if (null? list) nil
      (cons (max-elt list) (sortem (remove-it (max-elt list) list)))))

(define (sortem-gen list proc init)
  (define (proc-iter list)
    (fold-right proc init list))
  (if (null? list) nil
      (let ((current-elt (proc-iter list)))
        (cons current-elt (sortem-gen (remove-it current-elt list) proc init)))))

(define (memq word text)
  (cond ((null? text) #f)
        ((eq? word (car text)) text)
        (else (memq word (cdr text)))))

(define (lookup word dictionary)
  (cond ((null? dictionary) #f)
        ((eq? word (caar dictionary)) (cadar dictionary))
        (else (lookup word (cdr dictionary)))))

(define (translate sentence dictionary)
  (map (lambda (word) (lookup word dictionary)) sentence))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      ;;
;;  DOCUMENT HISTOGRAM  ;;
;;                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a document, return a histogram
;;; List < symbol > -> histogram

(define make-histogram
  (lambda (document)
    (add-words-to-histogram document (make-empty-histogram))))

;;; Make a new histogram with no words in it
;;; null -> histogram

(define make-empty-histogram
  (lambda () '()))

;; Define some basic operations on histograms

(define (make-entry word count) (list word count))
(define (get-word entry) (car entry))
(define (get-count entry) (cadr entry))

(define (first-entry histogram) (car histogram))
(define (rest-of-entries histogram) (cdr histogram))
(define (add-entry entry histogram) (cons entry histogram))

;;; Given a word and a histogram, return #t if the word is in the histogram,
;;; otherwise return #f

;;; symbol,histogram->boolean
(define in-histogram? 
  (lambda (word histogram)
    (and (pair? histogram)
	 (or (eq? word (get-word (first-entry histogram)))
	     (in-histogram? word (rest-of-entries histogram))))))

;;; Add a new word to a histogram with a count of 1
;;; symbol,histogram->histogram

(define add-new-word-to-histogram
  (lambda (word histogram)
    (add-entry (make-entry word 1) histogram)))

;; Increments the count of an existing word in the histogram
;; symbol,histogram->histogram

(define increment-word-count-in-histogram
  (lambda (word histogram)
    (let ((current-entry (first-entry histogram)))
      (if (eq? word (get-word current-entry))
          (add-entry 
           (make-entry word (+ 1 (get-count current-entry)))
           (rest-of-entries histogram))
          (add-entry
           current-entry
           (increment-word-count-in-histogram 
            word (rest-of-entries histogram)))))))

;; Generalizes the operation of processing words
;; symbol,histogram->histogram

(define process-word-for-histogram
  (lambda (word histogram)
    (if (in-histogram? word histogram)
        (increment-word-count-in-histogram word histogram)
        (add-new-word-to-histogram word histogram))))

;; steps through a document (list of words), processing each word
;; each word will either need to be added or incremented
;; list<symbol>,histogram -> histogram

(define add-words-to-histogram
  (lambda (document histogram)
    (if (null? document) histogram
        (let ((word (car document)))
          (add-words-to-histogram 
           (cdr document) (process-word-for-histogram word histogram))))))

;; counts the number of times that a given word occurs in a document
;; histogram,symbol -> number

(define times-occuring
  (lambda (histogram word)
    (cond ((null? histogram) 
           0)
          ((eq? word (get-word (first-entry histogram)))
           (get-count (first-entry histogram)))
          (else
           (times-occuring (rest-of-entries histogram) word)))))

;; COUNTING UTTERANCES

(define match-helper
  (lambda (full-pattern unmatched-pattern remaining-text count)
    (cond ((null? unmatched-pattern)
           (match-helper full-pattern full-pattern remaining-text (+ count 1)))  ;; match!
          ((null? remaining-text) 
           count)  ;; base case
          ((not (eq? (car unmatched-pattern) (car remaining-text)))
           (let ((remaining-text 
                  (if (eq? (car full-pattern) (car remaining-text))  ;; decide whether to throw out non-matching symbol
                      remaining-text (cdr remaining-text))))         ;; (if it matches the start of the pattern, keep it)
             (match-helper full-pattern full-pattern remaining-text count))) ;; no match, start over with remaining text
          (else 
           (match-helper full-pattern (cdr unmatched-pattern) (cdr remaining-text) count)))))  ;; keep going

(define match
  (lambda (pattern text)
    (match-helper pattern pattern text 0)))

(define (eq*? a b)
  (or (eq? a b)
      (eq? a '*)
      (eq? b '*)))

(define match*
  (lambda (pattern text)
    (define match-helper
      (lambda (full-pattern unmatched-pattern remaining-text count)
        (cond ((null? unmatched-pattern)
               (match-helper full-pattern full-pattern remaining-text (+ count 1)))  ;; match!
              ((null? remaining-text) 
               count)  ;; base case
              ((not (eq*? (car unmatched-pattern) (car remaining-text)))
               (let ((remaining-text 
                      (if (eq? (car full-pattern) (car remaining-text))  ;; decide whether to throw out non-matching symbol
                          remaining-text (cdr remaining-text))))         ;; (if it matches the start of the pattern, keep it)
                 (match-helper full-pattern full-pattern remaining-text count))) ;; no match, start over with remaining text
              (else 
               (match-helper full-pattern (cdr unmatched-pattern) (cdr remaining-text) count)))))  ;; keep going
    (match-helper pattern pattern text 0)))