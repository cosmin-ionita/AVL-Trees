#lang racket

(struct node (info left right) #:transparent)


(define empty-tree
  (node null null null)
)

(define init-node 
  (λ (value)
    (node value null null)
 ))

(define make-node
  (λ (left right value)
    (node value left right)
 ))

(define is-leaf?
  (λ (node)
    (and (null? (get-left node)) (null? (get-right node)))
  ))

(define get-value
  (λ (node)
       (node-info node)
  ))

(define get-left
  (λ (node)
    (node-left node))
  )

(define get-right
  (λ (node)
    (node-right node))
  )


(define is-node?
  (λ (node)
    (node? node))
  )

(define is-empty?
  (λ (tree)
    (if (or (equal? tree empty-tree) (null? tree))
        #t
        #f)
  ))

(define has-left?
  (λ (tree)
    (if (null? (get-left tree))
        #f
        #t))
  )


(define has-right?
  (λ (tree)
    (if (null? (get-right tree))
        #f
        #t)
  ))

(define minimum
  (λ (tree)
    (if (has-left? tree)
        (minimum (get-left tree))
        (get-value tree)
    )
  ))

(define maximum
  (λ (tree)
    (if (has-right? tree)
        (maximum (get-right tree))
        (get-value tree)
  ))
 )

(define (get-tree-height tree accumulator)
  (if (null? tree)

      accumulator
      
      (if (> (get-tree-height (node-left tree) accumulator) (get-tree-height (node-right tree) accumulator))
          (get-tree-height (node-left tree) (+ accumulator 1))
          (get-tree-height (node-right tree) (+ accumulator 1))
  
 )))

(define height
  (λ (tree)
    (get-tree-height tree 0)
  ))


(define inorder
  (λ (tree)
    (if (null? tree)
        '()
        (append (inorder (node-left tree))
                (list (node-info tree))
                (inorder (node-right tree))))
  )
)

(define preorder
  (λ (tree)
    (if (null? tree)
        '()
        (append (list (node-info tree))
                (preorder (node-left tree))
                (preorder (node-right tree)))
    )
 ))

(define postorder
  (λ (tree)
    (if (null? tree)
        '()
       (append (postorder (node-left tree))
               (postorder (node-right tree))
               (list (node-info tree))
    ))
    )
  )




(define (get-succesor-on-left-subtree tree value accumulator)

  (if (equal? (node-info tree) value)
      (car accumulator)

      (if (< value (node-info tree))
          (get-succesor-on-left-subtree (node-left tree) value (cons (node-info tree) accumulator))
          (get-succesor-on-left-subtree (node-right tree) value accumulator))

 )
)



(define (get-succesor tree value original-tree)

  (if (equal? (node-info tree) value)
        (if (not (null? (node-right tree)))   
            (minimum (node-right tree))  
            (get-succesor-on-left-subtree original-tree value '())  
         )
    
    (if (< value (node-info tree))
        (if (equal? (get-value (node-left tree)) value)
            (if (has-right? (get-left tree))
                (minimum (get-right (get-left tree)))
                (get-value tree))
            (get-succesor (node-left tree) value original-tree))
        (get-succesor (node-right tree) value original-tree)
    )
  )
)

(define successor
  (λ (tree value)
    (get-succesor tree value tree)
    )
)

(define (get-predecesor-on-left-subtree tree value accumulator)

  (if (equal? (node-info tree) value)
      (car accumulator)

      (if (< value (node-info tree))
          (get-predecesor-on-left-subtree (node-left tree) value accumulator)
          (get-predecesor-on-left-subtree (node-right tree) value (cons (node-info tree) accumulator)))

 )
)

(define (get-predecesor tree value original-tree)

  (if (equal? (node-info tree) value)
        (if (not (null? (node-left tree)))                            ;; if has left
            (maximum (node-left tree))                                ;; go on left, max right
            (get-predecesor-on-left-subtree original-tree value '())  ;; if not, start from the top and store in list all the elements greater than it
         )
    
    (if (> value (node-info tree))
        (get-predecesor (node-right tree) value original-tree)
        (get-predecesor (node-left tree) value original-tree))
    )
  )

(define predecessor
  (λ (tree value)
    (get-predecesor tree value tree))
  )


(define binary-search-tree (node 9 (node 3 (node 2 (node 1 null null) null) (node 6 (node 5 null null) (node 8 (node 7 null null) null))) (node 12 (node 11 null null) (node 15 (node 13 null null) (node 21 null null)))))



;; Rebuild the tree and return a tree with the new node inserted

(define (insert-value tree value)
  
  (if (is-empty? tree)
      (make-node null null value)

      (if (> value (get-value tree))
          (if (null? (get-right tree))
              (make-node (get-left tree) (make-node null null value) (get-value tree))
              (make-node (get-left tree) (balance (insert-value (get-right tree) value)) (get-value tree)))
              
          
          (if (null? (get-left tree))
              (make-node (make-node null null value) (get-right tree) (get-value tree))
              (make-node (balance (insert-value (get-left tree) value)) (get-right tree) (get-value tree)))
             
              
      )
  ))


(define insert
  (λ (tree value)
    (if (contains tree value)
        tree
        (balance (insert-value tree value))
  ))
)



(define (rotate-left-left tree)
  (make-node (get-left (get-left tree)) (make-node (get-right (get-left tree)) (get-right tree) (get-value tree)) (get-value (get-left tree)))
)

(define (rotate-left-right tree)

  (make-node (make-node
              (make-node (get-left (get-left tree)) (get-left (get-right (get-left tree))) (get-value (get-left tree))) (get-right (get-right (get-left tree)))
              (get-value (get-right (get-left tree)))) (get-right tree) (get-value tree))

)

(define (rotate-right-right tree)

  (make-node (make-node (get-left tree) (get-left (get-right tree)) (get-value tree)) (get-right (get-right tree)) (get-value (get-right tree)))
  
)

(define (rotate-right-left tree)

  (make-node (get-left tree)
             (make-node (get-left (get-left (get-right tree)))
                        (make-node (get-right (get-left (get-right tree))) (get-right (get-right tree)) (get-value (get-right tree)))
                        (get-value (get-left (get-right tree))))
             (get-value tree)))

(define (get-balance-factor tree)
  (- (height (get-left tree)) (height (get-right tree)))
)

(define balance
  (λ (tree)

    (if (equal? (get-balance-factor tree) 2) ;; the left is greater
        (if (equal? (get-balance-factor (get-left tree)) 1) ;; check the left son
            (rotate-left-left tree)                        ;; go on left left
            (rotate-left-left (rotate-left-right tree))    ;; otherwise, the balance factor can be -1
         )

        (if (equal? (get-balance-factor tree) -2)
            (if (equal? (get-balance-factor (get-right tree)) 1)  ;; on the right - left

                (rotate-right-right (rotate-right-left tree))
                (rotate-right-right tree)
            )

            tree)       ;; if balance factor is not 2, nor -2, then the tree is not modified
   )
  )
 )


(define (union-helper tree1 acc)

  (if (null? tree1)
      acc
      (union-helper (get-left tree1) (union-helper (get-right tree1) (insert acc (get-value tree1))))
  
  )
)

(define union
  (λ (tree1 tree2)

    (union-helper tree2 (union-helper tree1 '()))
    
    )
)

(define (intersect tree1 tree2 acc)

  (if (null? tree1)
        acc
     
        (if (contains tree2 (get-value tree1))
            ;; traverse the tree on the right, then on th left, with accumulator insertion conditioned
            (intersect (get-left tree1) tree2 (intersect (get-right tree1) tree2 (insert acc (get-value tree1))))
            (intersect (get-left tree1) tree2 (intersect (get-right tree1) tree2 acc))
        
  )
))

(define intersection
  (λ (tree1 tree2)
    (intersect tree1 tree2 '())
    
))

(define (complement-helper tree1 tree2 acc)

  (if (null? tree1)
        acc
     
        (if (not (contains tree2 (get-value tree1)))
            ;; the same traversal as intersect does, the difference is just at the above condition
            (complement-helper (get-left tree1) tree2 (complement-helper (get-right tree1) tree2 (insert acc (get-value tree1))))
            (complement-helper (get-left tree1) tree2 (complement-helper (get-right tree1) tree2 acc))
        
  )
))

(define complements
  (λ (tree1 tree2)

    (complement-helper tree1 tree2 '())
    
    )
  )

(define contains
  (λ (tree value)
    (if (or (is-empty? tree) (null? tree))
        #f
        (if (equal? (get-value tree) value)
            #t
            (if (> value (get-value tree))
                (contains (node-right tree) value)
                (contains (node-left tree) value)
             )
    )))
  )

(define (remove-leaf tree value)

  (if (equal? value (get-value tree))
      (get-right tree)

  (if (> value (get-value tree))
      (if (equal? value (get-value (get-right tree)))
          (make-node (get-left tree) null (get-value tree))
          (make-node (get-left tree) (remove-leaf (get-right tree) value) (get-value tree)))

      (if (equal? value (get-value (get-left tree)))
          (make-node null (get-right tree) (get-value tree))
          (make-node (remove-leaf (get-left tree) value) (get-right tree) (get-value tree))))
  ))

(define (remove-root tree)
  (if (null? (get-right tree))
      (get-left tree)
      (make-node (get-left tree) (remove-leaf (get-right tree) (minimum (get-right tree))) (minimum (get-right tree)))
      )
)

(define (remove-value tree value)

  (if (>= value (get-value tree))

         
         (if (equal? value (get-value tree))
             (remove-root tree)


           ;; right subtree
             
          (if (equal? value (get-value (get-right tree)))

              (if (is-empty? (get-left (get-right tree)))
                  (if (is-empty? (get-right (get-right tree)))               ;; leaf node
                      (make-node (get-left tree) null (get-value tree))
                      (make-node (get-left tree) (get-right (get-right tree)) (get-value tree)))  ;; right son

                  ;; nodes on left
                  
                  (if (is-empty? (get-right (get-right tree)))   ;; nothing on right
                      (make-node (get-left tree) (get-left (get-right tree)) (get-value tree)) ;; delete it
                      (make-node (get-left tree) (remove-root (get-right tree)) (get-value tree)))) ;; both children
                  
              (make-node (get-left tree) (remove-value (get-right tree) value) (get-value tree)))) ;; go on in finding the remove value

         ;; left subtree

          (if (equal? value (get-value (get-left tree)))

              (if (is-empty? (get-left (get-left tree)))     ;; nothing on left
                  (if (is-empty? (get-right (get-left tree)))  ;; nothing on right
                      (make-node null (get-right tree) (get-value tree))  ;; leaf node
                      (make-node (get-right (get-left tree)) (get-right tree) (get-value tree))) ;; right son

                  ;; I have right son
                  
                  (if (is-empty? (get-right (get-left tree)))   ;; see if I have right son
                      (make-node (get-left (get-left tree)) (get-right tree) (get-value tree)) ;; (if not)
                      (make-node (remove-root (get-left tree)) (get-right tree) (get-value tree))))

              (make-node (remove-value (get-left tree) value) (get-right tree) (get-value tree))) ;; go on in finding the remove value

        )
 )
             

(define remove
  (λ (tree value)
    (if (contains tree value)
        (remove-value tree value)
        tree
    )
  ))


(define (get-subsets set)
     (if (null? set)
         '(())
         
         (let ((accumulator (get-subsets (cdr set)))) 
            (append accumulator
                   (map (λ (subset) (cons (car set) subset))
                        accumulator)
                   )
           )
         )
 )

(define k-subsets
  (λ (set k)

    (filter (λ (x) (equal? (length x) k)) (get-subsets (inorder set)))
    
    )
  )


(define (perms L)
  (let rec ((SolPart '()))
    (if (= (length SolPart) (length L))
        (list SolPart)
        (apply append (map
                       (λ (e)
                         (if (member e SolPart)
                             '()
                             (rec (cons e SolPart)) 
                             )) L)))))

(define zig-zag-subsets
  (λ (set)

    (filter (λ (x)
              (or (is-zig-zag x 0) (is-zig-zag x 1))) (perms (inorder set))))
)


(define (is-zig-zag list token)
  (if (null? (cdr list))
      #t
      (if (equal? token 1)
          (if (< (car list) (cadr list))
              (is-zig-zag (cdr list) 0)
              #f)
          (if (> (car list) (cadr list))
              (is-zig-zag (cdr list) 1)
              #f))))


;;BONUS
(define parser
  (λ (expression)

    (if (number? expression)
        (make-node null null expression)    
        
        (make-node (parser (car expression))
  
                   (parser (caddr expression))

                   (cadr expression))
            )
        )
)

(define evaluate
  (λ (expr-tree)

    (if (is-leaf? expr-tree)
        (get-value expr-tree)
        
        (if (equal? (get-value expr-tree) '+)
            (+ (evaluate (get-left expr-tree)) (evaluate (get-right expr-tree)))
            (if (equal? (get-value expr-tree) '-)
                (- (evaluate (get-left expr-tree)) (evaluate (get-right expr-tree)))
                (if (equal? (get-value expr-tree) '/)
                    (/ (evaluate (get-left expr-tree)) (evaluate (get-right expr-tree)))
                    (* (evaluate (get-left expr-tree)) (evaluate (get-right expr-tree)))
                    )))
    )
  ))


