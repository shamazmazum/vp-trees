(defpackage vp-trees
  (:use #:cl)
  (:local-nicknames (#:sera #:serapeum)
                    (#:ff   #:float-features))
  (:shadow #:find)
  (:export #:vp-node
           #:make-vp-tree
           #:flatten
           #:find
           #:nearest-neighbor))
