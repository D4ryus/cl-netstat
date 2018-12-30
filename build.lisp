#!/usr/bin/sbcl --script

;; --script makes sbcl skip the user init file which sets up
;; quicklisp, load it explicitly and then try to require quicklisp to
;; ensure it is installed before we try to load anything
(let ((sbcl-init (merge-pathnames ".sbclrc"
                                  (user-homedir-pathname))))
  (when (probe-file sbcl-init)
    (with-open-file (stream sbcl-init :if-does-not-exist nil)
      (sb-int:load-as-source stream :context "userinit"))))

(require 'quicklisp)

(push (uiop:native-namestring
       (uiop:pathname-directory-pathname
        (uiop:current-lisp-file-pathname)))
      asdf:*central-registry*)

(ql:quickload :cl-netstat :silent t)

(setf deploy:*status-output* nil)

(let ((deploy:*status-output* t))
  (asdf:make :cl-netstat :force t))
