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
