;;;; cl-netstat.asd

(asdf:defsystem #:cl-netstat
  :description ""
  :author "d4ryus <d4ryus@openmailbox.org>"
  :license "LLGPL"
  :homepage "https://github.com/d4ryus/cl-netstat"
  :depends-on (#:croatoan
               #:alexandria
               #:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "cl-netstat")))
