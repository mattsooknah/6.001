(load "generate.scm")

;; These are a series of tests for the procedures defined in search.scm.

;(DFS 'http://sicp.csail.mit.edu/
;     (lambda (node) (eq? node 'cuttlefish))
;     the-web)

;(BFS 'http://sicp.csail.mit.edu/
;     (lambda (node) (eq? node 'cuttlefish))
;     the-web)

;; (define the-web-index (make-index))
;; 
;; (add-document-to-index! the-web-index
;;                         the-web 
;;                         'http://sicp.csail.mit.edu/)
;; 
;; (find-in-index the-web-index 'HELP)
;; ;Value: (http://sicp.csail.mit.edu/)
;; 
;; (find-in-index the-web-index '*MAGIC*)
;; ;Value: #f

;(define find-docs (make-web-index the-web 'http://sicp.csail.mit.edu/))
;(find-docs 'COLLABORATIVE)

;(search-any the-web 'http://sicp.csail.mit.edu/ 'COLLABORATIVE)
;(search-all the-web 'http://sicp.csail.mit.edu/ 'COLLABORATIVE)

;; -------------- WEB INDEX VS DYNAMIC SEARCH --------------

#|

(define smallweb (generate-random-web 5))
(define medweb (generate-random-web 25))
(define bigweb (generate-random-web 100))

(timed * 0 0)

(timed search-any smallweb '*start* 'HELP)
(timed search-any smallweb '*start* 'SHOCKFIELD)
(timed search-all smallweb '*start* 'HELP)
(timed make-web-index smallweb '*start*)
(define find-docs (make-web-index smallweb '*start*))
(timed find-docs 'HELP)
(timed find-docs 'SHOCKFIELD)

(timed search-any medweb '*start* 'HELP)
(timed search-any medweb '*start* 'SHOCKFIELD)
(timed search-all medweb '*start* 'HELP)
(timed make-web-index medweb '*start*)
(define find-docs (make-web-index medweb '*start*))
(timed find-docs 'HELP)
(timed find-docs 'SHOCKFIELD)

(timed search-any bigweb '*start* 'HELP)
(timed search-any bigweb '*start* 'SHOCKFIELD)
(timed search-all bigweb '*start* 'HELP)
(timed make-web-index bigweb '*start*)
(define find-docs (make-web-index bigweb '*start*))
(timed find-docs 'HELP)
(timed find-docs 'SHOCKFIELD)

;; when there is a lot of shared text between documents,
;; indexing performs much better than dynamic search.
;; large collections of documents (such as the real Web)
;; are likely to have a lot of shared text.

|#

;; -------------- BINARY SEARCH BENCHMARK --------------

(define *verbose* #f)

(define hugeweb (generate-random-web 5))
(define *all-words-list* (vector->list *all-words*))

(define web-index (make-index))
(BFS-active '*start* (lambda (doc) (add-document-to-index! web-index hugeweb doc)) no-goal hugeweb)
(define opt-web-index (optimize-index web-index))

(define (search-benchmark search index word-list cycles)
  (define (repeat cycles-remaining)
    (if (= cycles-remaining 0) 'done
        (iterate word-list cycles-remaining)))
  (define (iterate word-list cycles-remaining)
    (if (null? word-list) (repeat (- cycles-remaining 1))
        (let ((result (search index (car word-list))))
          (if *verbose* (begin (display result) (newline)))
          (iterate (cdr word-list) cycles-remaining))))
  (repeat cycles))

;(timed search-benchmark find-in-index web-index *all-words-list* 1)
;(timed search-benchmark find-in-optimized-index opt-web-index *all-words-list* 1)

;(timed search-benchmark find-in-index web-index '(HELP COLLABORATIVE SHOCKFIELD) 1000)
;(timed search-benchmark find-in-optimized-index opt-web-index '(HELP COLLABORATIVE SHOCKFIELD) 1000)
