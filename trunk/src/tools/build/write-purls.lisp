(defun write-purls (kb kbprev &optional (dest "iao:build;list-purls.xml"))
  (let((kb-purls
	(sparql
	 '(:select (?thing) (:distinct t)
	   (:union
	    ((?thing ?p ?o))
	    ((?s ?thing ?o))
	    ((?s ?p ?thing)))
	   (:filter (and (isuri ?thing) (regex (str ?thing) "IAO_\\d+")))
	   )
	 :kb kb :use-reasoner :none :flatten t))
       (kbprev-purls
	(and kbprev
	     (sparql
	      '(:select (?thing) (:distinct t)
		(:union
		 ((?thing ?p ?o))
		 ((?s ?thing ?o))
		 ((?s ?p ?thing)))
		(:filter (and (isuri ?thing) (regex (str ?thing) "IAO_\\d+")))
		)
	      :kb kbprev :use-reasoner :none :flatten t))))
    (flet ((doit (out)

	     (format out "<recs>~%")
	     (loop for new in (set-difference kb-purls kbprev-purls)
		for purl = (#"replaceFirst" (uri-full new) ".*/obo/" "/obo/")
		for id = (#"replaceFirst" purl ".*/" "")
		do (format out "<rec><purl>~a</purl><url>http://sw.neurocommons.org/iaoterm/~a</url><id>ALANRUTTENBERG</id><id>IAO</id><type>User_Batch_Add</type></rec>~%" purl id ))
	     (format out "</recs>~%")
	     ))
      (if (or (eq dest t) (streamp dest))
	  (doit dest)
	  (with-open-file (out dest
			       :direction :output
			       :if-exists :supersede)
	    (doit out))
	  )
      (when (set-difference kbprev-purls kb-purls)
	(format t "Hmm, we seem to have lost some ids (deprecation lossage?): ~%~{~a~%~}"
		(set-difference kbprev-purls kb-purls)))    
      )))

