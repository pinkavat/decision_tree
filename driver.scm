;; File:
;;   driver.scm
;; Summary
;;  testing for decision-tree-learner
;; Provides:
;;  [nothing]


(load "dtree.scm")    ; Load decision-tree routines
(load "learning.scm") 

; examples for learning the rule C or (A and B)

(define test-examples
  (list 
  (list #\y (cons "property A" #\t) (cons "property B" #\t) (cons "property C" #\t))
  (list #\y (cons "property A" #\t) (cons "property B" #\t) (cons "property C" #\f))
  (list #\y (cons "property A" #\t) (cons "property B" #\f) (cons "property C" #\t))
  (list #\n (cons "property A" #\t) (cons "property B" #\f) (cons "property C" #\f))
  (list #\y (cons "property A" #\f) (cons "property B" #\t) (cons "property C" #\t))
  (list #\n (cons "property A" #\f) (cons "property B" #\t) (cons "property C" #\f))
  (list #\y (cons "property A" #\f) (cons "property B" #\f) (cons "property C" #\t))
  (list #\n (cons "property A" #\f) (cons "property B" #\f) (cons "property C" #\f))))

(define test-attribs
  (list
   (list "property A" #\t #\f)
   (list "property B" #\t #\f)
   (list "property C" #\t #\f)))

(display "we will use a toy example of learning the rule \"C or (A and B)\"")
(display ", training on all 8 possible examples.") (newline)
(display "information gain of property A is ")
(display (information-gain test-examples "property A" test-attribs)) (newline)
(display "information gain of property B is ")
(display (information-gain test-examples "property B" test-attribs)) (newline)
(display "information gain of property C is ")
(display (information-gain test-examples "property C" test-attribs)) (newline)
(display "so we expect the decision tree created to split on property C")
(display " and then property A or B, either is fine, as they are equivalent")
(display " (and will remain so after splitting on property C).")
(display " In particular, the decision path we expect it to take is ")
(display "check whether property C is true. If it is, return yes. Otherwise")
(display " check whether property A (or property B) is true. If it isn't, ")
(display "return false. Otherwise, check whether property B (or property A, ")
(display "if we had property B earlier) is true. If it is, return true.") (newline) (newline)
(display (decision-tree-learning test-examples test-attribs #\t)) (newline)
(display "this decision tree follows the decision path we expected.")


