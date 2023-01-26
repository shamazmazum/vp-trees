(in-package :vp-trees)

(sera:-> divide-list
         (list (sera:-> (t) (values boolean &optional)))
         (values list list &optional))
(defun divide-list (list predicate)
  "Divide a set in two halves depending on the value of predicate
function"
  (let (left right)
    (mapc
     (lambda (x)
       (if (funcall predicate x)
           (push x left)
           (push x right)))
     list)
    (values left right)))

(defun median (list &optional (nleft 0) (nright 0))
  "Return median value for a list"
  (destructuring-bind (first . rest)
      list
    (multiple-value-bind (left-set right-set)
        (divide-list
         rest
         (lambda (x) (< x first)))
      (let* ((n-left-set  (length left-set))
             (n-right-set (length right-set))
             (nleft-upd  (+ nleft  n-left-set))
             (nright-upd (+ nright n-right-set)))
        (cond
          ((and (> nleft-upd nright-upd)
                (not (zerop n-left-set)))
           (median left-set nleft (1+ nright-upd)))
          ((and (> nright-upd nleft-upd)
                (not (zerop n-right-set)))
           (median right-set (1+ nleft-upd) nright))
          (t first))))))

(sera:-> pick-random
         (list)
         (values t list &optional))
(defun pick-random (list)
  "Pick a random value from a list and return this value and a new
list with this value removed."
  (let ((idx (random (length list))))
    (labels ((pick-random% (list acc n)
               (destructuring-bind (car . cdr) list
                 (if (= n idx)
                     (values car (nconc acc cdr))
                     (pick-random%
                      cdr
                      (cons car acc)
                      (1+ n))))))
      (pick-random% list nil 0))))

(sera:defstruct-read-only
    (vp-node
     (:print-function
      (lambda (object stream depth)
        (declare (ignore depth))
        (print-unreadable-object (object stream :type t :identity t)
          (format stream "CENTER: ~a RADIUS: ~a"
                  (vp-node-center object)
                  (vp-node-radius object))))))
  (center :type t)
  (radius :type (real 0))
  (inner  :type (or vp-node null))
  (outer  :type (or vp-node null)))

(declaim (inline vp-node))
(defun vp-node (center radius inner outer)
  (make-vp-node :center center
                :radius radius
                :inner  inner
                :outer  outer))

(sera:-> vp-node-leaf-p
         (vp-node)
         (values boolean &optional))
(defun vp-node-leaf-p (node)
  "True if this node is a leaf"
  (and (null (vp-node-inner node))
       (null (vp-node-outer node))))

(deftype metric ()
  '(sera:-> (t t) (values (real 0) &optional)))

(sera:-> make-vp-tree
         (list metric &key (:key function))
         (values vp-node &optional))
(defun make-vp-tree (list distance &key (key #'identity))
  "Make vantage point tree from a set @c(list) using a distance
function @c(distance). Optional @c(key) function can be specified as a
mapping between elements in @c(list) and elements in your metric
space, so @c(ρ(x,y) = distance (key(x), key(y))) where x and y are in
the @c(list)."
  (if (<= (length list) 1)
      (vp-node (first list) 0 nil nil)
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
                     (make-vp-tree outer-set distance :key key)))))))

(sera:-> search-close
         (vp-node t (real 0) metric &key (:key function))
         (values list &optional))
(defun search-close (tree item threshold distance
                     &key (key #'identity))
  "Find all items in the tree @c(tree) closer to @c(item) than
@c(threshold). @c(item) and elements of the tree must belong to a
metric space with @c(distance) as a metric function. Optional @c(key)
function can be used to calculate a distance between two objects in
the following way: @c(ρ(x,y) = distance (key(x), key(y)))."
  (let (acc)
    (labels ((%search (subtree)
               (let ((center (vp-node-center subtree)))
                 (when center
                   (let ((item-center-dist (funcall distance
                                                    (funcall key item)
                                                    (funcall key center))))
                     (when (<= item-center-dist threshold)
                       (push center acc))
                     (unless (vp-node-leaf-p subtree)
                       (let* ((radius (vp-node-radius subtree))
                              (min-thr (- radius threshold))
                              (max-thr (+ radius threshold)))
                         (cond
                           ((< item-center-dist min-thr)
                            (%search (vp-node-inner subtree)))
                           ((> item-center-dist max-thr)
                            (%search (vp-node-outer subtree)))
                           (t
                            (%search (vp-node-inner subtree))
                            (%search (vp-node-outer subtree)))))))))))
      (%search tree))
    acc))

(sera:-> flatten (vp-node) (values list &optional))
(defun flatten (tree)
  "Deconstruct VP tree back into list. The order of elements in the
original list is not preserved."
  (let (list)
    (labels ((%flatten! (node)
               (let ((center (vp-node-center node)))
                 (when center
                   (push center list)
                   (unless (vp-node-leaf-p node)
                     (%flatten! (vp-node-inner node))
                     (%flatten! (vp-node-outer node)))))))
      (%flatten! tree))
    list))
