(in-package :vp-trees)

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

(defun median (list &optional (nleft 0) (nright 0) (key #'identity))
  "Return median value for a list"
  (destructuring-bind (first . rest)
      list
    (multiple-value-bind (left-set right-set)
        (divide-list
         rest
         (lambda (x) (< (funcall key x)
                        (funcall key first))))
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

(defstruct (vp-node
             (:print-function
              (lambda (p s k)
                (declare (ignore k))
                (print-unreadable-object (p s :identity t)
                  (format s "VP-tree")))))
  center
  (radius 0  :type number)
  (inner nil :type (or null vp-node))
  (outer nil :type (or null vp-node)))

(defun vp-node-leaf-p (node)
  "True if this node is a leaf"
  (and (null (vp-node-inner node))
       (null (vp-node-outer node))))

(defun make-vp-tree (list distance &key (key #'identity))
  "Make vantage point tree from a set @c(list) using a distance
function @c(distance). Optional @c(key) function can be specified as a
mapping between elements in @c(list) and elements in your metric
space, so @c(ρ(x,y) = distance (key(x), key(y))) where x and y are in
the @c(list)."
  (if (<= (length list) 1)
      (make-vp-node :center (first list))
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
            (make-vp-node :center center
                          :radius median
                          :inner (make-vp-tree inner-set distance :key key)
                          :outer (make-vp-tree outer-set distance :key key)))))))

(defun search-close (tree item threshold distance
                     &key (key #'identity))
  "Find all items in the tree @c(tree) closer to @c(item) than
@c(threshold). @c(item) and elements of the tree must belong to a
metric space with @c(distance) as a metric function. Optional @c(key)
function can be used to calculate a distance between two objects in
the following way: @c(ρ(x,y) = distance (key(x), key(y)))."
  (let (acc)
    (labels ((do-search% (subtree)
               (let ((center (vp-node-center subtree)))
                 (if (not (null center))
                     (let ((item-center-dist (funcall distance
                                                      (funcall key item)
                                                      (funcall key center))))
                       (if (<= item-center-dist threshold)
                           (push center acc))
                       (if (not (vp-node-leaf-p subtree))
                           (let* ((radius (vp-node-radius subtree))
                                  (min-thr (- radius threshold))
                                  (max-thr (+ radius threshold)))
                             (cond
                               ((< item-center-dist min-thr)
                                (do-search% (vp-node-inner subtree)))
                               ((> item-center-dist max-thr)
                                (do-search% (vp-node-outer subtree)))
                               (t
                                (do-search% (vp-node-inner subtree))
                                (do-search% (vp-node-outer subtree)))))))))))
      (do-search% tree))
    acc))
