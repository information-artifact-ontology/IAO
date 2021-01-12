# information artifact ontology (IAO)

The Information Artifact Ontology (IAO) is a new ontology of information entities, originally driven by work by the [OBI](http://obi-ontology.org/) digital entity and realizable information entity branch.

The IAO mailing-list is available at http://groups.google.com/group/information-ontology

### Files

|Version|Filename|PURL Link|
|---|---|---|
|Release|iao.owl|http://purl.obolibrary.org/obo/iao.owl| 
|Development|iao-edit.owl|http://purl.obolibrary.org/obo/iao/dev/iao-edit.owl| 

The development version IAO (iao-edit.owl) imports all ontologies/files needed for the full version of the IAO ontology:
 - [Basic Formal Ontology](http://purl.obolibrary.org/obo/bfo.owl): latest release
 - [OBO Metadata Ontology](http://purl.obolibrary.org/obo/omo.owl): latest release
 - [Relation Ontology Core subset](http://purl.obolibrary.org/obo/ro/core.owl): latest release
 - [import-OBO.owl](http://purl.obolibrary.org/obo/iao/dev/import-OBO.owl): reused terms in external ontologies, retrieved using ontoFox tool
 - [externalByHand.owl](http://purl.obolibrary.org/obo/iao/dev/externalByHand.owl): terms or axioms associated with terms in external ontologies, added manually
 - [obsolete.owl](http://purl.obolibrary.org/obo/iao/dev/obsolete.owl): obsolete classes and properties

OBO Metadata Ontology (OMO) was part of the IAO, developed in a separate OWL file. It contains [metadata properties](https://github.com/information-artifact-ontology/IAO/wiki/OntologyMetadata) that have been widely used by many OBO Foundry ontologies. In early 2020, it was registered as an indepentent ontology. Please check the OBO Metadata Ontology (OMO) on the [ontology-metadata repository](https://github.com/information-artifact-ontology/ontology-metadata).

A reasonable place to start browsing the IAO is at the terms [information content entity](http://purl.obolibrary.org/obo/IAO_0000030) or [data item](http://purl.obolibrary.org/obo/IAO_0000027) or the property [is about](http://purl.obolibrary.org/obo/IAO_0000136).

### Conference calls

IAO conference calls take place on an occasional basis, usually via Skype. Information about those is posted on our [mailing-list](http://groups.google.com/group/information-ontology), and agenda and minutes are available [here](https://github.com/information-artifact-ontology/IAO/wiki/Meeting_notes).
Anybody is welcome to join at any time.

### Credits

The IAO is a descendant of the [DENRIE branch](http://ashby.csail.mit.edu/presentations/DenrieReport.pdf) of the [OBI Project](http://obi-ontology.org/), edited by [Chris Stoeckert](http://www.cbil.upenn.edu/~stoeckrt/home.html) and other [IAO team members](https://github.com/information-artifact-ontology/IAO/wiki/AdditionalCredits). 

### Presentations and Meetings

The [first workshop](https://web.archive.org/web/20170712204335/http://neurocommons.org/page/First_IAO_workshop) on the IAO took place in Boston at the MIT Stata Center, June 9, 2008. 

In July 2009, a tutorial [From Basic Formal Ontology to the Information Artifact Ontology](http://www.bioontology.org/wiki/index.php/From_BFO_to_IAO) was presented, colocated at the [ICBO: International Conference on Biomedical Ontology](http://icbo.buffalo.edu/). Slides that include a section on topics in the IAO are [here](http://icbo.buffalo.edu/Presentations/Ruttenberg.pdf). Other [presentations may be of interest](http://icbo.buffalo.edu/Presentations/).

In July 2011, a workshop was held co-located with ICBO, dealing with adverse event representation based on IAO and [OGMS](http://purl.obolibrary.org/obo/ogms). See http://purl.org/net/aeicbo2011. A [working session](https://github.com/information-artifact-ontology/IAO/wiki/WorkingSessionICBO2011) was organized on Friday July 29th 2011.

In May 2012, [a meeting discussing a signature discovery ontology](http://ncorwiki.buffalo.edu/index.php/Basic_Formal_Ontology_and_the_Signature_Discovery_Ontology), much of which was concerned with future development of IAO.

A [working session on IAO](https://github.com/information-artifact-ontology/IAO/wiki/WorkingSessionICBO2012) was held on Saturday July 21st, 2012, 6-8pm during the ICBO 2012 conference.

### Projects known to be using IAO

 - [The Ontology for Biomedical Investigations](http://ob-ontology.org)
 - [The Oral Health and Disease Ontology](http://code.google.com/p/ohd-ontology)
 - [The OWL format for OBO ontologies](http://code.google.com/p/oboformat/)
 - [KIAO](http://bio-ontologies.knowledgeblog.org/149)
 - [the Ontology for General Medical Science](https://github.com/OGMS/ogms)

and as listed on IAO's [BioPortal](http://bioportal.bioontology.org/ontologies/1393) page:
 - [An Ontology for Drug Discovery Investigations (DDI)](http://purl.org/ddi/home)
 - [Adverse Event Reporting Ontology (AERO)  ](http://purl.obolibrary.org/obo/aero)
 - [ISA software suite ](http://isa-tools.org/)
 - [NCBO Resource Index ](http://www.bioontology.org/resources-index)
 - [Ontology of Data Mining (OntoDM) ](http://kt.ijs.si/panovp/OntoDM/)
 - [Neural ElectroMagnetic Ontologies (NEMO) ](http://nemo.nic.uoregon.edu/)
 - [Influenza Research Database (IRD) ](http://bioportal.bioontology.org/ontologies/www.fludb.org)
 - [OntoCAT ](http://www.ontocat.org/)
 - [eagle-i ](https://www.eagle-i.org/home/)
