(defun obo-format-time (&optional (time-value (get-universal-time)))
  "Format a time how the obo format likes to see it"
  (multiple-value-bind (second minute hour day month year dow dst zone)
      (decode-universal-time time-value)
    (declare (ignore dow dst))
    (format nil "~2,'0D:~2,'0D:~4,'0D ~2,'0D:~2,'0D"
	    day month year  hour minute
	    )))

(defun localname (uri)
  "The part of the URI after the last / or #"
  (let ((full (uri-full uri)))
    (#"replaceFirst" full ".*[/#](.*)" "$1")))

(defun ontology-of (uri)
  "The part of the URI before the last / or #"
  (let ((full (uri-full uri)))
    (let ((candidate (#"replaceFirst" full "(.*)[/#](.*)" "$1")))
      (if (equal candidate "http://purl.obolibrary.org/obo")
	  (format nil "~a/~a.owl" candidate (string-downcase (#"replaceAll" (localname uri) "_|\\d" "")))
	  (if (#"matches" candidate "http://purl.org/obo/owl/.*")
	      (format nil "~a.owl" candidate)
	      (if (#"matches" candidate "http://www.ifomis.org/bfo/.*")
		  (#"replaceAll" candidate "(snap)|(span)$" "")
		  (if (#"matches" candidate "http://www.obofoundry.org/ro/ro.owl.*")
		      "http://purl.org/obo/owl/OBO_REL.owl"
		      (warn "don't know ontology of ~a uri" uri)
		      )))))))

(defun localname-namespaced (uri)
  "Abbreviate a URI using the OBO conventions for namespaces."
  (let* ((full (uri-full uri))
	 (local  (#"replaceFirst" full ".*/(.*)" "$1")))
    (setq local (#"replaceFirst" local "([A-Za-z]{2,7})_" "$1:"))
    (if (is-obi-uri uri)
	local
	(if (#"matches" local "^ro\\.owl#.*")
	    ; Relation ontology
	    (concatenate 'string "OBO_REL:" (#"replaceFirst" local ".*#(.*)" "$1"))
	    (if (#"matches" local "^MGEDOntology\\.owl.*")
		; MGED
		(concatenate 'string "MGED:" (#"replaceFirst" local ".*#(.*)" "$1"))
		(if (#"matches" local "[A-Z]+.owl#msi_.*")
		    ; the MSI ontologies. These shouldn't actually be here, having been replaced by OBI ids before this stage.
		    ; While debugging warn about them and give them a fake id.
		    (progn
		      ;(format *error-output* "Found (and replaced) MSI id ~a~%" local)
		      (#"replaceFirst" local "[A-Z]+.owl#msi_(.*)" "OBI:X$1"))
		    (let ((clean (#"replaceFirst" local "#" ":")))
		      (#"replaceFirst" clean "(\\S+):\\S+?:(\\d+)" "$1:$2")))
		    )))))
	
(defvar *all-prefixes* nil)

(defun definition-source-dbxref (class kb)
  "Generate the dbxref for a definition. Involves various munging to map OBI's more broad range of definition sources. Return empty list since definitions need such even if there are no references"
  (let ((definition-source (sparql `(:select (?source) (:distinct t)
					     (,class !definition-source ?source))
				   :use-reasoner :none :kb kb :flatten t)))
    (if definition-source	     ; note there may be more than one
	(values
	 (with-output-to-string (s)
	   (format s " [~{~a~^, ~}]" ;; dbxref entries are comma separated
		   (loop for defs in definition-source
		      for is-msi = (or (search "MSI.owl" defs) (search "NMR.owl" defs) (search "CHROM.owl" defs)) ; special case the MSI ontologies. Prototype.
		      collect 
		      (if is-msi
			  (format nil "MSI:~a ~s" (caar (all-matches defs ".owl#msi_(.*)" 1)) defs) ; reformat as MSI:xxxxxx, as they all share the same id prefix in the source.
			  ;; not msi. See if we have a term = foo:xxxxx. Allow ones that are in the xrf file to pass. 
			  (let ((match (car (all-matches defs "(?i)^(TERM:\\s*){0,1}(\\S+):\\s*(\\S+?)(\\s*#\\s*(.*)){0,1}$" 2 3 5))))
			    (if (and match (not (equalp (car match) "SEP"))) ; For now sep: ids are not registed in the GO
				(let ((prefix (car match)) (suffix (second match)) (comment (third match))) ; we've matched
				  (setq prefix (string-upcase prefix)) ; normalize to upcase prefix
				  (pushnew prefix *all-prefixes* :test 'equalp) ; for debugging: Keep track of which prefixes we've used
				  (when (equal prefix "PMID") (setq prefix "PUBMED")) ; you say PUBMED I say PMID
				  (format nil "~a:~a ~a" prefix suffix
					  (if (member comment '("" nil) :test 'equal)
					      ""
					      (format nil "\"~a\"" comment))))
				;; check for embedded ISBNs (should be fixed in source)
				(let ((has-isbn (caar (all-matches defs "ISBN-\\d+:\\s+([0-9-]*)" 1))))
				  (if has-isbn
				      (format nil "ISBN:~a ~s" has-isbn defs)
				      ;; otherwise, we are not a term and use OBO:sourced and the string for definition source
				      (format nil "OBI:sourced ~s" (#"replaceFirst" defs "OBI_" "OBI:" )))))))))))
	(loop for defs in definition-source
	   for is-msi = (or (search "MSI.owl" defs) (search "NMR.owl" defs) (search "CHROM.owl" defs)) ; special case the MSI ontologies. Prototype.
	   when is-msi
	   collect (format nil "xref_analog: MSI:~a ~s~%" (caar (all-matches defs ".owl#msi_(.*)" 1)) defs))
	)
    ;; no definition source. Return empty list
    (if (#"matches" (uri-full class)  ".*/OBI_.*")
	" [OBI:sourced \"OBI Consortium http://purl.obolibrary.org/obo/obi\"]"
	(progn 
	  (format nil " [OBI:imported ~s]" (ontology-of class))
	  ))))
		     

(defun generate-iao-obo ( &key (kb (load-kb-jena "iao:branches;iao-main.owl"))
		     (classes-matching ".*([A-Za-z]+)_\\d+") ; ignored atm
		     (properties-matching ".*IAO_\\d+") ; ignored atm
		     (path "iao:branches;iao.obo")) 
  (with-open-file (f  path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let ((*current-labels* (rdfs-labels kb)) ; speed optimization for labels and comments
          (*current-comments* (rdfs-comments kb)))
      (macrolet ((rdfs-label (e) `(gethash ,e *current-labels*)) 
		 (rdfs-comment (e) `(gethash ,e *current-comments*)))
					; write canned header
	(format f "format-version: 1.2
date: ~a
saved-by: iao
auto-generated-by: 
default-namespace: IAO
idspace: OBO_REL http://www.obofoundry.org/ro/ro.owl# \"OBO Relation ontology official home on OBO Foundry\"
idspace: snap http://www.ifomis.org/bfo/1.1/snap# \"BFO SNAP ontology (continuants)\"
idspace: span http://www.ifomis.org/bfo/1.1/span# \"BFO SPAN ontology (occurrents)\"
idspace: OBI http://purl.obolibrary.org/obo/OBI_ \"Ontology for Biomedical Investigations\"
remark: This file is a conversion of the iao-main.owl file, which remains the authoritative version

" (obo-format-time))
	(loop for class in (set-difference (descendants !owl:Thing kb) (list !protegeowl:DIRECTED-BINARY-RELATION !protegeowl:PAL-CONSTRAINT))
	   for obsolete = (member !obsolete-class (ancestors class kb))
	   when t      ;(#"matches" (uri-full class) classes-matching)
	   do 
	   ;; write term ID and name
	   (format f "[Term]~%id: ~a~%name: ~a~%"
		   ;;(#"replaceFirst"  (#"replaceFirst" (uri-full class) ".*(OBI|CHEBI|CL|NCBITaxon)(_\\d+)" "$1:$1$2") "OBI:" "")
		   (localname-namespaced class)
		   (or (rdfs-label class) (localname class)))
	   ;; write definition and dbxref for them
	   (let ((comment (rdfs-comment class)))
	     (unless (or (null comment) (equal comment ""))
	       (multiple-value-bind (dbxrefs analogs) (definition-source-dbxref class kb)
		   (format f "def:~s~a~%"  (#"replaceAll" (#"replaceAll" comment "\\n" " " )
							  "\\\\" "\\\\\\\\" )
			   dbxrefs)
		 (loop for xra in analogs do (write-string xra f))
		 )))
	   ;; put obsolete marked for obsolete classes
	   (if obsolete
	       (format f "is_obsolete: true~%")
	       (loop for super in (parents class kb)
		  do (format f "is_a: ~a ! ~a~%"  (localname-namespaced super) (rdfs-label super))))
	   (terpri f))
	;; now write out the list of properties we will use (we don't actually write their values out yet in the above)
	(loop for proptype in (list !owl:AnnotationProperty !owl:DatatypeProperty !owl:ObjectProperty )
	   do
	   (loop for prop in (sparql `(:select (?prop) (:distinct t) (?prop !rdf:type ,proptype))  :use-reasoner :jena :kb kb :flatten t)
	      for name = (localname-namespaced prop)
	      when t ;(#"matches" (uri-full prop) properties-matching)
	      do
	      (unless (or (#"matches" name ".*[A-Z]{4,}.*") ; protege noise is all upper case
			  (#"matches" name "^protege.*")
			  (#"matches" (uri-full prop) "(http://purl.org/dc/.*)|(.*oboInOwl.*)")
			  (member name '("OBO_REL:relationship") :test 'equal)
			  )
		(format f "[Typedef]~%id: ~a~%name: ~a~%"
			(localname-namespaced prop)
			(or (rdfs-label prop) (localname prop)))
		(let ((comment (rdfs-comment prop)))
		  (unless (or (null comment) (equal comment ""))
		    (if (#"matches" comment "(?s).*beta.*") (print comment))
		    (multiple-value-bind (xrefs analogs) (definition-source-dbxref prop kb)
		      (format f "def:\"~a\"~a~%"
			      (#"replaceAll" (#"replaceAll" comment "\\n" " " ) "\\\\" "\\\\\\\\" )
			      xrefs
			      )
		      (loop for xra in analogs do (write-string xra f))
		      )))
		;; compute and write inverse relation
		(let ((inverses
		       (mapcar 'aterm-to-sexp
			       (set-to-list
				(#"getInverses" (kb-kb kb) (get-entity prop kb)) ))))
		  (loop for inverse in inverses do
		       (format f "inverse_of: ~a ! ~a~%" (localname-namespaced inverse) (rdfs-label inverse))))
		;; compute and write superproperties
		(let ((supers
		       (mapcar 'aterm-to-sexp
			       (apply 'append (mapcar 'set-to-list 
						      (set-to-list
						       (#"getSuperProperties" (kb-kb kb) (get-entity prop kb))))))))
		  (loop for super in supers 
		     unless (eq super !ro:relationship)
		     do (format f "is_a: ~a ! ~a~%" (localname-namespaced super) (or (rdfs-label super) (localname super)))))
		(terpri f))
	      ))))))

