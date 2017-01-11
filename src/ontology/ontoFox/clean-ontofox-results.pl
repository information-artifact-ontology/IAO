# Copy until the first xml comment, which leads into the annotation property section
while (<>) 
  { last if /[<][!]-- / ;
    # Add versionIRI
    if (m'<owl:Ontology rdf:about="http://purl.obolibrary.org/obo/iao/import-OBO.owl"/>')
      { s'OBO.owl"/>'OBO.owl">';
	print $_;
	print '    <owl:versionIRI rdf:resource="http://purl.obolibrary.org/obo/iao/dev/import-OBO.owl"/>'."\n";
	print '  </owl:Ontology>'."\n";
      }
    else {
      print $_}
  }
# Skip over that section
while (<>) {last if /[<][!]-- /;}
print "<!-- ";
# Remove a stale definition from OBI
while (<>)
{ next if /a planned process that realizes the concretization of a study design/ ; print $_}
