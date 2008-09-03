(defun list-obi-uris (&optional (dest "obi:build;uri-report.txt") (kb (load-kb-jena :obi)))
  (let ((seen (make-hash-table)))
    (with-open-file (f dest :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for class in
	   (sparql '(:select (?class) (:distinct t)
		     (?class !rdfs:subClassOf !owl:Thing))
		   :use-reasoner :jena :kb kb :flatten t)
	   do (format f "Class ~a~%" (uri-full class))
	   (setf (gethash class seen) t))
      (loop for proptype in (list !owl:AnnotationProperty !owl:DatatypeProperty !owl:ObjectProperty )
	 for name = (subseq (format nil "~a" proptype) 5)
	 do
	 (loop for prop in
	      (sparql `(:select (?prop) (:distinct t) (?prop !rdf:type ,proptype))  :use-reasoner :jena :kb kb :flatten t)
	      do (format f "~a ~a~%" name (uri-full prop))
	      (setf (gethash prop seen) t)))
      (loop for instance in
	   (sparql '(:select (?thing) (:distinct t) (?thing !rdf:type !owl:Thing))  :use-reasoner :jena :kb kb :flatten t)
	   when (and instance
		     (not (gethash instance seen))
		     (not (#"matches" (uri-full instance) "urn:blank.*"))
		     (not (#"matches" (uri-full instance) ".*owl$")))
	   do (format f "Individual ~a~%"  (uri-full instance)))
      (truename dest))))