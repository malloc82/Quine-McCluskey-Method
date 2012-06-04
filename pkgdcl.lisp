(defpackage #:quine-mccluskey
  (:nicknames "qm")
  (:use #:cl)
  (:export #:quine-mccluskey))

(declaim (optimize (safety 3)) (optimize (debug 3)) (optimize (speed 0)))