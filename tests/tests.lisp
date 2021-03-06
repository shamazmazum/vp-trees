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
(defconstant +vp-count+ 100000)
(defconstant +vp-radius+ 0.1)
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

(test vp-test
  (let* ((data (gen-points +vp-count+))
         (tree (make-vp-tree data #'dist))
         (point (gen-point))
         (naive-search (remove-if-not
                        (lambda (x) (<= (dist x point) +vp-radius+))
                        data))
         (vp-search (search-close tree point +vp-radius+ #'dist)))
    (is-true (and (subsetp naive-search vp-search :test #'equal)
                  (subsetp vp-search naive-search :test #'equal)))))
