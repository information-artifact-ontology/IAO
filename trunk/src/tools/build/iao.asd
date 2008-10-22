;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(setf (logical-pathname-translations "obi")
      `(("branches;*.*" ,(make-pathname :directory
						    (append (butlast (pathname-directory *load-pathname*) 3)
							    '("src" "ontology"))
						    :name :wild
						    :type :wild))
	("build;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3)
							'("build"))
				     :name :wild
				     :type :wild))
	("newids;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3)
							'("build" "newids"))
				     :name :wild
				     :type :wild))
	("lisp;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 3)
						       '("src" "tools" "build"))
				    :name :wild
				    :type :wild))

	("releases;**;*.*" ,(make-pathname :directory (append (butlast (pathname-directory *load-pathname*) 4)
						       '("tags" :wild-inferiors))
				    :name :wild
				    :type :wild))
	))

(defsystem :iao
    :name "IAO Tools"
    :author "Alan Ruttenberg"
    :version "1"
    :licence "BSD"
    :components
    (
     (:file "write-purls")
     )
    :depends-on (owl))

;;;; eof
