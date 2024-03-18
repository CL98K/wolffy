(defsystem #:wolffy
  :description "wolffy"
  :version "0.0.1"
  :author "CL98K <917599580@QQ.COM>"
  :licence "MIT"
  :depends-on (#:uiop #:pack #:select #:alexandria #:fast-io #:kit)
  :components ((:module "src"
                :serial t
                :components ((:module "io"
                              :serial t
                              :components ((:file "package")
                                           (:file "io"))) 
                             (:module "pickle"
                              :serial t
                              :components ((:file "package")
                                           (:file "pickle")
                                           (:file "conditions")
                                           (:file "opcodes")
                                           (:file "loads")
                                           (:file "dumps")))
                             (:file "package")))))

(defsystem #:wolffy/test
  :description "wolffy test"
  :version "1.0.0"
  :author "CL98K <917599580@QQ.COM>"
  :licence "MIT"
  :depends-on (#:wolffy)
  :components ((:module "test"
                :components ((:file "io-test")
                             (:file "pickle-test")))))
