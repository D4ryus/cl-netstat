;;;; cl-netstat.asd

(asdf:defsystem #:cl-netstat
  :description "Curses Program which visualizes network traffic by reading /proc/net/dev"
  :author "d4ryus <d4ryus@teknik.io>"
  :license "LLGPL"
  :homepage "https://github.com/d4ryus/cl-netstat"
  :build-operation "deploy-op"
  :build-pathname "cl-netstat"
  :entry-point "cl-netstat:main"
  :defsystem-depends-on (:deploy)
  :depends-on (#:croatoan
               #:alexandria
               #:cl-ppcre
               #:uiop
               #:swank
               #:deploy)
  :serial t
  :components ((:file "package")
               (:file "term-rgb-map")
               (:file "cl-netstat")))
