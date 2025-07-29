(in-package :vp-trees)

(sera:-> divide-list (list (sera:-> (t) (values boolean &optional)))
         (values list list alex:non-negative-fixnum alex:non-negative-fixnum &optional))
(defun divide-list (list predicate)
  "Divide a set in two halves depending on the value of predicate
function"
  (loop with nleft  fixnum = 0
        with nright fixnum = 0
        with left  = nil
        with right = nil
        for x in list do
        (cond
          ((funcall predicate x)
           (incf nleft)
           (push x left))
          (t
           (incf nright)
           (push x right)))
        finally (return (values left right nleft nright))))

(defun median (list &optional (nleft 0) (nright 0))
  "Return median value for a list"
  (destructuring-bind (first . rest) list
    (multiple-value-bind (left-set right-set n-left-set n-right-set)
        (divide-list rest (lambda (x) (< x first)))
      (let ((nleft-upd  (+ nleft  n-left-set))
            (nright-upd (+ nright n-right-set)))
        (cond
          ((and (> nleft-upd nright-upd)
                (not (zerop n-left-set)))
           (median left-set nleft (1+ nright-upd)))
          ((and (> nright-upd nleft-upd)
                (not (zerop n-right-set)))
           (median right-set (1+ nleft-upd) nright))
          (t first))))))

(sera:-> pick-random (list)
         (values t list &optional))
(defun pick-random (list)
  "Pick a random value from a list and return this value and a new
list with this value removed."
  (let ((idx (random (length list))))
    (labels ((%pick-random (list acc n)
               (destructuring-bind (car . cdr) list
                 (if (= n idx)
                     (values car (nconc acc cdr))
                     (%pick-random
                      cdr
                      (cons car acc)
                      (1+ n))))))
      (%pick-random list nil 0))))

(sera:defstruct-read-only
    (vp-node
     (:print-function
      (lambda (object stream depth)
        (declare (ignore depth))
        (print-unreadable-object (object stream :type t :identity t))))
     (:constructor vp-node (center radius inner outer)))
  (center :type t)
  (radius :type (real 0))
  (inner  :type (or vp-node null))
  (outer  :type (or vp-node null)))

(deftype vp-tree () '(or null vp-node))

(declaim (inline vp-node-has-children-p))
(defun vp-node-has-children-p (node)
  (or (vp-node-inner node)
      (vp-node-outer node)))

(deftype metric ()
  '(sera:-> (t t) (values (real 0) &optional)))

(sera:-> make-vp-tree (list metric &key (:key function))
         (values vp-tree &optional))
(defun make-vp-tree (list distance &key (key #'identity))
  "Make vantage point tree from a set @c(list) using a distance
function @c(distance). Optional @c(key) function can be specified as a
mapping between elements in @c(list) and elements in your metric
space, so @c(ρ(x,y) = distance (key(x), key(y))) where x and y are in
the @c(list)."
  (cond
    ((null list)
     nil)
    ((null (cdr list))
     (vp-node (first list) 0 nil nil))
    (t
     (multiple-value-bind (center rest)
         (pick-random list)
       (let ((median (median
                      (mapcar
                       (lambda (x)
                         (funcall distance
                                  (funcall key center)
                                  (funcall key x)))
                       rest))))
         (multiple-value-bind (inner-set outer-set)
             (divide-list
              rest
              (lambda (x) (< (funcall distance
                                      (funcall key center)
                                      (funcall key x))
                             median)))
           (vp-node center median
                    (make-vp-tree inner-set distance :key key)
                    (make-vp-tree outer-set distance :key key))))))))

(sera:-> find (vp-tree t (real 0) metric
                       &key (:key function) (:max alex:positive-fixnum))
         (values list &optional))
(defun find (tree item threshold distance &key (key #'identity) (max most-positive-fixnum))
  "Find all items in the tree @c(tree) closer to @c(item) than
@c(threshold). @c(item) and elements of the tree must belong to a
metric space with @c(distance) as a metric function. Optional @c(key)
function can be used to calculate a distance between two objects in
the following way: @c(ρ(x,y) = distance (x, key(y))), @c(y) being an
element of the tree. Optionally @c(max) can be specified to return no
more than that number of items. In this case it is not guaranteed that
the returned items are necessarily the closest items to @c(item)."
  (labels ((%go (subtree acc n)
             (if (or (null subtree) (= n max))
                 (values acc n)
                 (let* ((center (vp-node-center subtree))
                        (item-center-dist
                         (funcall distance item
                                  (funcall key center))))
                   (multiple-value-bind (acc n)
                       (if (<= item-center-dist threshold)
                           (values (cons center acc) (1+ n))
                           (values acc n))
                     (if (not (vp-node-has-children-p subtree))
                         (values acc n)
                         (let* ((radius (vp-node-radius subtree))
                                (min-thr (- radius threshold))
                                (max-thr (+ radius threshold)))
                           (cond
                             ((< item-center-dist min-thr)
                              (%go (vp-node-inner subtree) acc n))
                             ((> item-center-dist max-thr)
                              (%go (vp-node-outer subtree) acc n))
                             (t
                              (multiple-value-call #'%go
                                (vp-node-inner subtree)
                                (%go (vp-node-outer subtree) acc n)))))))))))
    (nth-value 0 (%go tree nil 0))))

(sera:-> flatten (vp-tree) (values list &optional))
(defun flatten (tree)
  "Deconstruct VP-tree back into a list. The order of elements in the
original list is not preserved."
  (labels ((%go (node acc)
             (if (null node) acc
                 (%go (vp-node-outer node)
                      (%go (vp-node-inner node)
                           (cons (vp-node-center node) acc))))))
      (%go tree nil)))

(sera:-> nearest-neighbor (vp-tree t metric &key (:key function))
         (values t real &optional))
(defun nearest-neighbor (tree item distance &key (key #'identity))
  "Return element in the @c(tree) which is the closest element to
@c(item). @c(item) and elements of the tree must belong to a metric
space with @c(distance) as a metric function. Optional @c(key)
function can be used to calculate a distance between two objects in
the following way: @c(ρ(x,y) = distance (x, key(y))), @c(y) being an
element of the tree.."
  (labels ((%go (subtree current-best current-dist)
             (if (null subtree)
                 (values current-best current-dist)
                 (let* ((center (vp-node-center subtree))
                        (item-center-dist
                         (funcall distance item
                                  (funcall key center))))
                   (multiple-value-bind (current-dist current-best)
                       (if (< item-center-dist current-dist)
                           (values item-center-dist center)
                           (values current-dist current-best))
                     (if (not (vp-node-has-children-p subtree))
                         (values current-best current-dist)
                         (let* ((radius (vp-node-radius subtree))
                                (min-thr (- radius current-dist))
                                (max-thr (+ radius current-dist)))
                           (cond
                             ((< item-center-dist min-thr)
                              (%go (vp-node-inner subtree) current-best current-dist))
                             ((> item-center-dist max-thr)
                              (%go (vp-node-outer subtree) current-best current-dist))
                             (t
                              (multiple-value-call #'%go
                                (vp-node-outer subtree)
                                (%go (vp-node-inner subtree)
                                     current-best current-dist)))))))))))
    (%go tree nil ff:single-float-positive-infinity)))
