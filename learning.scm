(load "dtree.scm")
;;
;; File
;;   learning.scm
;;
;; Authors
;;   Jerod Weinman
;;     Documentation for decision-tree-learning and choose-attribute
;;   Anonymous students
;;
;; Summary
;;   Implementation of decision-tree learner
;;
;; Provides
;;   (decision-tree-learning examples attribs default)
;;   (choose-attribute examples candidates attrib-values)



;;
;; Procedure
;;   choose-attribute
;;
;; Purpose
;;   Find an optimal attribute to split on
;;
;; Parameters
;;   examples, an association list
;;   candidates, a list
;;   attrib-values, an association list
;;
;; Produces
;;   attrib, a value
;;
;; Preconditions
;;   candidates is non-empty
;;   attrib-values is non-empty
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   Each member of candidates is a key in the association list.
;;   attrib-values is an association list
;;   Each value of the association list for an example's attribute is a member 
;;      of the values for attrib-values under the same key.
;;
;; Postconditions
;;   attrib is the member of candidates with the highest information gain 
;;   (entropy minus average conditional entropy)
(define choose-attribute
    (lambda (examples candidates attributes)
        (car 
            (let left-fold-kernel ;; Scheme doesn't have built-in fold,
                                  ;;  so here is a homemade fold-left
                ([procedure
                    ;; Anonymous sub-procedure applied to folding
                    ;; Takes the given attribute and an
                    ;; attribute-information pair, and updates the
                    ;; pair to the attribute of maximum information
                    (lambda (val prev)
                        (let [(inf-gain
                                (information-gain examples val attributes))]
                            (if (> inf-gain (cdr prev))
                                (cons val inf-gain)
                                prev)))]

                ;; The algorithm runs with maximum pre-set to the first
                ;; of the attributes; by precondition candidates is nonempty
                [start (cons (car candidates)
                (information-gain examples (car candidates) attributes))]
                [remaining (cdr candidates)])

                ;; Invoke the fold-left on the above parameters
                (if (null? remaining)
                    start
                    (left-fold-kernel
                        procedure
                        (procedure (car remaining) start)
                        (cdr remaining)))))))


;;
;; Procedure
;;   decision-tree-learning
;;
;; Purpose
;;   Learn a decision tree from data
;;
;; Parameters
;;   examples, a list
;;   attribs, an association list
;;   default, a value
;;
;; Produces
;;   decision-tree, a decision-tree
;;
;; Preconditions
;;   Each entry in examples is a pair whose car is a label (any Scheme value)  
;;      whose and cdr is an association list. 
;;   Each association list has identical keys (and in the same order), 
;;      but potentially different values.
;;   attribs is an association list
;;   Each key in attribs is a key in the example association list
;;   Each value of an attribute in examples a member of the corresponding 
;;      association list values for attribs
;;
;;
;; Postconditions
;;   decision tree-is a decision tree, which is either a label or a list 
;;      whose car is an attribute and whose cdr is an association list 
;;      with attribute values as keys and decision trees as values.

(define decision-tree-learning
  (lambda (examples attribs default)
    (let helper ([ex examples]
                 [candidates (map car attribs)]
                 [def default])
    (cond
      [(null? ex)
       def]
      [(all-same-label? ex)
       (caar (label-counts ex))]
      [(null? candidates)
       (plurality-value ex)]
      [else
       (let* ([best (choose-attribute ex candidates attribs)]
              ; choose attribute to split on
              [vals (cdr (assoc best attribs))]
              ; values of chosen attribute
              [new-candidates (filter-list candidates best)])
              ; filter out chosen attribute from list of candidates
         (cons best
               (map
                (lambda (val)
                  (cons val (helper
                             (filter-examples-by-attribute-value ex best val)
                             new-candidates
                             (plurality-value ex))))
                vals)
               ; for each value of chosen attribute, create new decision tree
               ; anonymous procedure creates this decision tree
               ))]))))
          
          
    
      
       
            


