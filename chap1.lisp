(defparameter *titles*
  '(Mr Mrs Jr Miss Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name")


(defun last-name (name)
  "Select the last name from a name represented as a list"
  (let ((reversed-name (reverse name)))
    (if (member (first reversed-name) *titles*)
	(last-name (reverse (rest reversed-name)))
	(first reversed-name))))


(defun power (base exp)
  "Raise base by exp"
  (if (equalp exp 0)
      1
      (if (equalp exp 1)
	  base
	  (* base (power base (- exp 1))))))


(defun count-atoms (object)
  "Return the number of atoms represented by object"
  (if (not (listp object))
      1
      (apply #'+ (mapcar #'count-atoms object))))


(defun count-anywhere (exp match-exp)
  "Return the number of times exp appears within match-exp"
  (if (equalp exp match-exp)
      1
      (if (listp match-exp)
	  (if match-exp
	      (+ (count-anywhere exp (first match-exp))
		 (count-anywhere exp (rest match-exp)))
	      0)
	  0)))


(defun dot-product (list1 list2)
  "Returns the dot product of list1 and list2"
  (apply #'+ (mapcar #'* list1 list2))
