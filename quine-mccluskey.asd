
(asdf:defsystem :quine-mccluskey
    :author ("Ritchie Cai")
    :maintainer "Ritchie Cai"
    :description "Quine-McCluskey method implementation"
    :components ((:file "pkgdcl")
                 (:file "quine-mccluskey" :depends-on ("pkgdcl"))))