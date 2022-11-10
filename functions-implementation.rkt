#lang racket
; Kelly Ngoc Hoang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SET EQUAL?
; Check if an element in first set is existing in the second list, one-by-one.
; Do the same thing for the other set.
(define (set-equal? set1 set2)
  (letrec ([subset? (lambda (setA setB)
                     (or (null? setA)
                         (and (member (car setA) setB)
                              (subset? (cdr setA) setB))))])
    (and (subset? set1 set2)
       (subset? set2 set1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NESTED-SET-EQUAL?
(define (nested-set-equal? set1 set2)
  (letrec ([flatten (lambda (set)
                      (cond ((null? set) '())
                            ((not (pair? set)) (list set))
                            (else (append (flatten (car set))
                                          (flatten (cdr set))))))])
           (set-equal? (flatten set1) (flatten set2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UNION
; Check if an element in first set is existing in the second list, one-by-one.
; If it does, cont. to check the rest.
; If it does not, add it to the Union list, and cont. to check the rest.
(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTERSECTION
; Check one-by-one element in a set is existing in the other set.
; If it does, add it to the Intersection list.
(define intersection
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member (car set1) set2)
           (cons (car set1)
                 (intersection (cdr set1) set2)))
          (else (intersection (cdr set1) set2)))))
              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MERGESORT
(define (mergesort lst)
  ; split in half until the list gets 1 element
  (letrec ([split (lambda (lst)
                    (if (null? lst)
                        (list '() '())
                        (if (null? (cdr lst))                          ; base case: 1 element left 
                            (list lst '())     
                            (let ((rest (split (cdr (cdr lst)))))
                              (list (cons (car lst) (car rest))
                                    (cons (car (cdr lst)) (car (cdr rest))))))))]
           ; merge them by sorting ascending
           [merge (lambda (lst1 lst2)
                    (if (null? lst1)
                        lst2
                        (if (null? lst2)
                            lst1
                            (if (< (car lst1) (car lst2))             ; get the smallest and cont. merging the rest
                                (cons (car lst1) (merge (cdr lst1) lst2))
                                (cons (car lst2) (merge (cdr lst2) lst1))))))])
           ; mergesort
           (if (null? lst)
               '()
               (if (null? (cdr lst))                                  ; base case: list has only 1 element
                   lst
                   (let ((split_lst (split lst)))
                     (let ((lst1 (car split_lst)) (lst2 (car (cdr split_lst))))
                       (merge (mergesort lst1) (mergesort lst2))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POWER SET
; Return null list if the input set is null
; For element(s), that has been went over, will be paired with those are left in the list
; Repeat that until the list got none left.
(define powerset
  (lambda (lst)
  (if (null? lst) '(())
      (let ((powerset-rest (powerset (cdr lst))))
        (append powerset-rest
              (map (lambda (subset)
                     (cons (car lst) subset))
                   powerset-rest))))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NESTED-REDUCE
(define (nested-reduce set)
  (cond [(null? set) '()]
        [(member (car set) (cdr set))
         (nested-reduce (cdr set))]
        [(list? (car set))                    ; or pair?
         (cons (nested-reduce (car set)) 
               (nested-reduce (cdr set)))]         
        [else
         (cons (car set)
               (nested-reduce (cdr set)))]))









         