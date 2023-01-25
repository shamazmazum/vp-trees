vp-trees
========
[![Build Status](https://api.cirrus-ci.com/github/shamazmazum/vp-trees.svg)](https://cirrus-ci.com/github/shamazmazum/vp-trees)
![CI](https://github.com/shamazmazum/vp-trees/workflows/CI/badge.svg)

**vp-trees** is an implementation of vantage point tree data structure
in Common Lisp. It allows to perform fast (O(log N) in the best case)
fixed-radius near neighbors searches in some set of a metric space.

Look at the following example. Let's choose the space ℝ²[0, 1] and
generate some points belonging to this space:

``` lisp
(defun gen-point ()
  (vector (random 1.0)
          (random 1.0)))

(defun gen-points (n)
  (loop repeat n collect (gen-point)))
```

Then introduce a metric on this space (usual Euclidean metric):

``` lisp
(defun dist (a b)
  (sqrt
   (reduce #'+
           (map 'vector (lambda (x y) (expt (- x y) 2))
                a b))))
```

Build a tree consisting of 1000000 elements in ℝ²[0, 1]:

``` lisp
(defparameter *tree*
    (vp-trees:make-vp-tree (gen-points 1000000) #'dist))
```

Now return points which are closer than `0.1` to the origin:

``` lisp
(vp-trees:search-close *tree* #(0.0 0.0) 0.1 #'dist)
```

The advantage of VP trees is that you don't have to stick to Euclidean
metric: you may choose whatever metric you want as long as four
metric axioms hold. For example, VP trees can be used for
multidimensional spaces.

## API list

~~~~
    make-vp-tree
    flatten
    search-close
~~~~
