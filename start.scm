(load "mushroom.scm") ; Load procedures for loading mushroom data
(load "dtree.scm")    ; Load decision-tree routines
(load "learning.scm")

; Load the attributes list for mushrooms
(define mushroom-attributes
  (load-mushroom-attributes "mushroom-attribs.txt"))
; Load all the mushroom examples (labels with attribute association lists)
(define mushroom-examples
  (load-mushroom-examples "mushrooms.txt" mushroom-attributes))

(define get-keys
  (lambda (alist)
    (map car alist)))

(label-counts mushroom-examples)

(plurality-value mushroom-examples)

(cdr (assoc "stalk-shape" mushroom-attributes))

(define examples-stalk-val1 (filter-examples-by-attribute-value mushroom-examples "stalk-shape" #\e))
(define examples-stalk-val2 (filter-examples-by-attribute-value mushroom-examples "stalk-shape" #\t))

(label-counts examples-stalk-val1)
(label-counts examples-stalk-val2)

(map (lambda (x)
       (label-counts
        (filter-examples-by-attribute-value mushroom-examples
                                            "gill-size" x)))
     (cdr (assoc "gill-size" mushroom-attributes)))

(information-gain mushroom-examples "stalk-shape" mushroom-attributes)
(information-gain mushroom-examples "gill-size" mushroom-attributes)

(define decision-stump
  (cons "gill-size"
        (list (cons #\b #\e)
              (cons #\n #\p))))

(define first-instance (cdar mushroom-examples))

(cdr (assoc (cdr (assoc (car decision-stump) first-instance)) (cdr decision-stump)))
      
      
      
    
