(defparameter *obi-prefixes*
  '(("xsd" "http://www.w3.org/2001/XMLSchema#")
    ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
    ("owl" "http://www.w3.org/2002/07/owl#")
    ("daml" "http://www.daml.org/2001/03/daml+oil#")
    ("dcterms" "http://purl.org/dc/terms/")
    ("dc" "http://purl.org/dc/elements/1.1/")
    ("protege" "http://protege.stanford.edu/plugins/owl/protege#")
    ("protege-dc" "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#")
    ("oboInOwl" "http://www.geneontology.org/formats/oboInOwl#")
    ("bfo" "http://www.ifomis.org/bfo/1.1#")
    ("robfo" "http://purl.org/obo/owl/ro_bfo_bridge/1.1#")
    ("snap" "http://www.ifomis.org/bfo/1.1/snap#")
    ("span" "http://www.ifomis.org/bfo/1.1/span#")
    ("ro" "http://www.obofoundry.org/ro/ro.owl#")
    ("rotoo" "http://purl.org/obo/owl/ro#")
    ("pato" "http://purl.org/obo/owl/PATO#")
    ("cell" "http://purl.org/obo/owl/CL#")
    ("chebi" "http://purl.org/obo/owl/CHEBI#")
    ("envo""http://purl.org/obo/owl/ENVO#")
    ("ncbitax""http://purl.org/obo/owl/NCBITaxon#")
    ("obi" "http://purl.obolibrary.org/obo/")
    ("caro" "http://purl.org/obo/owl/CARO#")
    ("pro" "http://purl.org/obo/owl/PRO#")
    ("so" "http://purl.org/obo/owl/SO#")
    ("go" "http://purl.org/obo/owl/GO#")
))

(defparameter *external-derived-header*
  "<?xml version=\"1.0\"?>
<rdf:RDF
  xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
  xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\"
  xmlns:owl=\"http://www.w3.org/2002/07/owl#\"
  xml:base=\"http://purl.obolibrary.org/obo/iao/dev/externalDerived.owl#\">
  <owl:Ontology rdf:about=\"\">
    <owl:versionInfo rdf:datatype=\"http://www.w3.org/2001/XMLSchema#string\"
    >$Revision: 80 $</owl:versionInfo>
  </owl:Ontology>
")

(defun parse-templates (templates-path)
  (with-open-file (f templates-path)
    (let* ((params 
	    (loop for line = (read-line f nil :eof)
	       until (or (eq line :eof) (search "==" line))
	       unless (or (char= (char line 0) #\#) (equal line ""))
	       append (all-matches line "(.*?):(.*)" 1 2) into them
	       when (eql #\= (peek-char t f)) do (return them)))
	   (prefixes (with-output-to-string (s)
		       (loop for (param value) in params
			    when (equalp param "prefix")
			    do (format s "prefix ~a~%" value))))
	   (templates nil))
      (flet ((ont+query (lines)
	       (setq lines (remove #\# (remove "" (reverse lines) :test 'equal) :key (lambda(el) (char el 0))))
	       (let ((ont (caar (all-matches (car lines) "==\\s+(.*?)\\s+==" 1)))
		     (query
		      (replace-all
		       (concatenate 'string prefixes
				    (join-with-char (rest lines) #\linefeed))
		         "alias:([a-zA-z0-9._-]*)" (lambda(alias)
					     (#"replaceFirst"
					      (second
					       (find alias params :test (lambda(what el)
									  (declare (ignore what))
									  (and (equal (car el) "alias")
									       (eql 0 (search alias (second el)))))))
					       ".*=(.*)" "$1"))
			 1)))
		 (if (assoc ont templates :test 'equalp)
		     (pushnew query (cadr (assoc ont templates :test 'equalp)) :test 'equal)
		     (push (list ont (list query)) templates))
		 )))
	(loop for line = (read-line f nil :eof)
	   with lines
	   until (eq line :eof)
	   do
	     (push line lines)
	     (when (or (eql #\= (peek-char t f nil)) (null (peek-char t f nil)))
	       (ont+query lines) (setq lines nil))
	   finally (when lines (ont+query lines))))
      (values params templates))))

(defun combine-template-query-results (results output-path)
  (with-open-file (f output-path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string *external-derived-header* f)
    (loop for rdf in results
       for lines = (split-at-char rdf #\linefeed)
       do
       (loop for line in (butlast (cddr lines))
	  do (format f "~a~%" line)))
    (format f "<owl:AnnotationProperty rdf:about=\"http://purl.obolibrary.org/obo/IAO_0000115\"/><owl:AnnotationProperty rdf:about=\"http://purl.obolibrary.org/obo/OBI_0000111\"/>")
    (format f "</rdf:RDF>~%")))

(defun combine-template-query-results (results output-path)
  (with-open-file (f output-path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write-string *external-derived-header* f)
    (loop for rdf in results
       do
	 (write-string (#"replaceAll" (#"replaceFirst" (#"replaceAll" rdf "(?i)</{0,1}rdf:rdf.*?>" "") "<\\?xml.*?\\?>" "") "purl.obolibrary.org" "purl.obolibrary.org") f))
    (format f "</rdf:RDF>~%")))

(defun clean-rdf (path prefixmapping)
  (let* ((file (maybe-url-filename path))
	 (model (#"createOntologyModel" 'modelfactory (get-java-field 'OntModelSpec "OWL_MEM"))))
    (let ((base "http://purl.obolibrary.org/obo/iao/dev/externalDerived.owl"))
      (#"read" model (new 'io.bufferedinputstream (#"getInputStream" (#"openConnection" (new 'java.net.url file)))) base)
      (loop for (prefix namespace) in prefixmapping
	 do (#"setNsPrefix" (#"getPrefixMapping" (#"getGraph" model)) prefix namespace))
      (let ((writer (#"getWriter" model "RDF/XML-ABBREV")))
	(#"setProperty" writer "showXmlDeclaration" "true")
	(#"setProperty" writer "xmlbase" base)
	(#"setProperty" writer "relativeURIs" "")
	(#"write" writer model (new '|FileOutputStream| path) base)
	))))

(defparameter *repos-still-has-obofoundry-urls* nil)

(defun create-external-derived (&key
				(kb (load-kb-jena "iao:branches;external.owl"))
				(templates-path "iao:lisp;external-templates.txt")
				(output-path (merge-pathnames
					      "externalDerived.owl"
					      (truename "iao:branches;")))

				(endpoint nil)
				(debug nil))
  (let ((*sparql-always-trace* (or *sparql-always-trace* debug)))
    (declare (special *sparql-always-trace*))
    (let ((classes 
	   (sparql  '(:select (?class ?where ?parent) () 
		     (?class !rdf:type !owl:Class)
		     (?class !obi:OBI_0000283 ?where)
		     (?class !rdfs:subClassOf ?parent)) 
		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
		   :kb kb)))
      (format t "There are ~a external classes~%" (length classes))
      (multiple-value-bind (params templates) (parse-templates templates-path)
	(let ((endpoint (or (second (assoc "default endpoint" params :test 'equal)) endpoint)))
	  (assert endpoint () "What endpoint should I use?")
	  (format t "Using endpoint: ~a~%" endpoint)
	  (let ((rdfs 
		 (append
		  (loop for query in (cadr (assoc "Once Only" templates :test 'equalp))
		       do (if debug (print-db query))
		     collect (get-url endpoint :post `(("query" ,query)) :persist nil :dont-cache t :force-refetch t))
		  (loop for (class where) in classes
		     append
		     (loop for (ont-pattern queries) in templates
			when (#"matches" (uri-full where)  (format nil "(?i)~a" ont-pattern))
			append
			(loop for query in queries
			   for filled-query = (#"replaceAll" query "_ID_GOES_HERE_" (format nil "<~a>" (uri-full class)))
			   do (if *repos-still-has-obofoundry-urls* 
				  (setq filled-query (#"replaceAll" filled-query "purl.obolibrary.org" "purl.obofoundry.org")))
			   collect (if debug
				       (progn
					 (print-db filled-query)
					 (print (get-url endpoint :post `(("query" ,filled-query)) :persist nil :dont-cache t :force-refetch t)))
				       (get-url endpoint :post `(("query" ,filled-query)) :persist nil :dont-cache t :force-refetch t))))))))
	    (let ((basic-info
		   (with-output-to-string (s)
		     (loop for (class nil parent) in classes
			do (format s "<owl:Class rdf:about=~s></owl:Class>~%"
				   (uri-full class) )))))
	      (combine-template-query-results (cons basic-info (if *repos-still-has-obofoundry-urls*
								   (mapcar (lambda(e) (#"replaceAll" e "purl.obofoundry.org" "purl.obolibrary.org"))
									   rdfs)
								   rdfs))
						    output-path))
	    (clean-rdf (namestring (truename output-path)) *obi-prefixes*)
	    nil
	    ))))))




      



