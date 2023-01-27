(defpackage vp-trees
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:ff   #:float-features))
  (:export #:vp-node
           #:make-vp-tree
           #:flatten
           #:search-close
           #:nearest-neighbor))
