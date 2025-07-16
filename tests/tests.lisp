(in-package :vp-trees-tests)

(def-suite vp-trees :description "Test VP trees")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(vp-trees))))

(in-suite vp-trees)

(defconstant +median-count+ 300)
(defconstant +median-delta+ 3)
(test median-test
  (let* ((data (loop repeat +median-count+ collect (random 100000)))
         (median (vp-trees::median data))
         (count-left (length (remove-if-not (lambda (x) (< x median)) data)))
         (count-right (- +median-count+ count-left)))
    (is (< (abs (- count-left count-right))
           +median-delta+))))

;; Test VP trees in R^2[0,1] space
(defun gen-point ()
  (vector (random 1.0)
          (random 1.0)))

(defun gen-points (n)
  (loop repeat n collect (gen-point)))

(defun dist (a b)
  (sqrt
   (reduce #'+
           (map 'vector (lambda (x y) (expt (- x y) 2))
                a b))))

(defun nn (list item)
  (let (current-best (current-dist 1000000))
    (loop for x in list do
          (when (< (dist item x) current-dist)
            (setq current-best x
                  current-dist (dist item x))))
    (values current-best current-dist)))

(test find
  (loop with count  = 100000
        with radius = 0.1
        repeat 30
        for data   = (gen-points count)
        for tree   = (vp-trees:make-vp-tree data #'dist)
        for point  = (gen-point)

        for naive-search = (remove-if-not
                            (lambda (x) (<= (dist x point) radius))
                            data)
        for vp-search = (vp-trees:find tree point radius #'dist) do
        (is-true (and (subsetp naive-search vp-search)
                      (subsetp vp-search naive-search)))))

(test nearest-neighbor
  (loop with count = 100000
        repeat 30
        for data  = (gen-points count)
        for tree  = (vp-trees:make-vp-tree data #'dist)
        for point = (gen-point) do
        (multiple-value-bind (naive-best naive-dist)
            (nn data point)
          (multiple-value-bind (vp-best vp-dist)
              (vp-trees:nearest-neighbor tree point #'dist)
            (is (equalp naive-best vp-best))
            (is (= naive-dist vp-dist))))))

;; Other tests
(test flatten
  (let* ((data (gen-points 100))
         (flat (vp-trees:flatten
                (vp-trees:make-vp-tree data #'dist))))
    (is-true (and (subsetp data flat)
                  (subsetp flat data)))))
