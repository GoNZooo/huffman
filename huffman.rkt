#lang racket

(require data/bit-vector)

(struct node (prob data encoding)
        #:transparent)

(define (left-child n)
  (first (node-data n)))
(define (right-child n)
  (second (node-data n)))

(define (inode? n)
  (list? (node-data n)))
(define (lnode? n)
  (string? (node-data n)))

(define (prioq str)
  (define (freq-list str)
    (define (frequencies data [freq (make-immutable-hash)])
      (define (string-first str)
        (substring str 0 1))
      (define (string-rest str)
        (substring str 1))

      (define (add1-frequency frequency-hash c)
        (hash-set frequency-hash c (+ 1 (hash-ref frequency-hash c 0))))

      (if (equal? data "")
          freq
          (frequencies (string-rest data)
                       (add1-frequency freq (string-first data)))))

    (sort (hash->list (frequencies str)) <= #:key cdr))
  
  (map (lambda (i) (node (cdr i) (car i) "")) (freq-list str)))

(define (make-tree pq)

  (define (make-inode pq)
    (define (sum-probabilities lst)
      (foldl (lambda (n res)
               (+ (node-prob n) res))
             0
             lst))
    
    (sort (cons (node (sum-probabilities (take pq 2)) (take pq 2) "")
                (drop pq 2))
          <= #:key node-prob))
  
  (if (= (length pq) 1)
      (first pq)
      (make-tree (make-inode pq))))

(define (traverse-tree node inodef lnodef)
  (cond
   [(inode? node)
    (inodef (node-prob node)
            (traverse-tree (left-child node) inodef lnodef)
            (traverse-tree (right-child node) inodef lnodef))]
   [(lnode? node)
    (lnodef (node-prob node) (node-data node))]))

(define (path-to letter tree [bitlist '()])

  (define (contains? tree letter)
    (define (lnodef prob data)
      (equal? data letter))
    (define (inodef prob left right)
      (or left right))

    (traverse-tree tree inodef lnodef))
  
  (if (inode? tree)
      (cond
       [(contains? (left-child tree) letter)
        (path-to letter (left-child tree) (cons #f bitlist))]
       [(contains? (right-child tree) letter)
        (path-to letter (right-child tree) (cons #t bitlist))])
      (list->bit-vector bitlist)))

(define (calculate-total-bits hm-tree)
  (foldl (lambda (e res)
           (+ (* (node-prob e) (bit-vector-length (node-encoding e))) res))
         0
         hm-tree))

(define (msg->node-list msg)
  (let ([pq (prioq msg)])
    (map (lambda (n)
           (node (node-prob n) (node-data n)
                 (path-to (node-data n)
                          (make-tree pq))))
         pq)))

(define (node-list->encodings nl)
  (map (lambda (n)
         (cons (node-data n) (node-encoding n)))
       nl))

(define (encodings->hash enc [enc-hash (make-immutable-hash)])
  (if (null? enc)
      enc-hash
      (encodings->hash (rest enc)
                       (hash-set enc-hash
                                 (car (first enc))
                                 (cdr (first enc))))))

(define (msg->encoded-msg/string msg)
  (define codes (encodings->hash (node-list->encodings (msg->node-list msg))))

  (define (encode data [output ""])    
    (define (msg-first m)
      (substring m 0 1))
    (define (msg-rest m)
      (substring m 1))
    (define (fetch-bits c)
      (bit-vector->string (hash-ref codes c)))

    (if (equal? data "")
        output
        (encode (msg-rest data)
                (string-append output
                               (fetch-bits (msg-first data))))))

  (encode msg))

(module+ main
  (define msg "Feel the power, coursing through your veins. It's intoxicating...")

  (msg->encoded-msg/string msg))