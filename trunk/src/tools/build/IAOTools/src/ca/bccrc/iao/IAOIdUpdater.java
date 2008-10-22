
package ca.bccrc.iao;

import java.net.URI;
import java.net.URISyntaxException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashSet;


import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.model.AddAxiom;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyChangeException;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLOntologyStorageException;
import org.semanticweb.owl.model.UnknownOWLOntologyException;
import org.semanticweb.owl.util.OWLEntityRenamer;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;


/**
 * @author mcourtot
 *
 */
public class IAOIdUpdater {


	//	create a list to store changes in


	public static NumberFormat uriIDFormat = new java.text.DecimalFormat("0000000");

	public static ArrayList<URI> listUsedIDs = new ArrayList<URI>();
	public static ArrayList<URI> listToModifyIDs = new ArrayList<URI>();

	/**
	 * @param ontology
	 * @return the list of IDs already in use
	 */
	public static void IDUsedChecker(OWLOntology ontology){

		//this covers datatype- and object- properties, individuals and classes
		for(OWLEntity singleClass : ontology.getReferencedEntities()) {

			URI singleClassURI = singleClass.getURI();

			if (singleClassURI.toString().matches("http://purl.obofoundry.org/obo/(.*)"))
			{

				//check if ID correct format
				if (singleClass.toString().matches("IAO_[0-9]{7}")){
					listUsedIDs.add(singleClass.getURI());
				}
				else {
					listToModifyIDs.add(singleClass.getURI());
				}

			}
		}


		//of course, annotations are different..
		for (URI annotationURI : ontology.getAnnotationURIs()){
			listUsedIDs.add(annotationURI);
		}


	}


	/**
	 * @param uriCount - the ID we want to assign
	 * @return the next free ID
	 * @throws URISyntaxException
	 */
	public static int getNextFreeID(int uriCount) throws URISyntaxException	{


		String newID = uriIDFormat.format(new Integer(uriCount));

		URI uriName = new URI("http://purl.obofoundry.org/obo/IAO_"+newID);

		if (!listUsedIDs.contains(uriName)){
			//this ID is free, return it
			return uriCount;
		}

		else {
			//ID was already taken
			//increment counter
			uriCount++;
			//retry with this new ID
			return getNextFreeID(uriCount);
		}

	}



	/**
	 * @param args
	 */
	public static void main(String[] args){



		//Create OWLOntologyManager which manages a set of ontologies

		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();


//		original file to load
		URI physicalURI = URI.create("file:/Users/mcourtot/Desktop/projects/IAO/SVN/trunk/src/ontology/IAO.owl");
//		where to save the resulting file			
		URI physicalURI2 = URI.create("file:/Users/mcourtot/Desktop/IAO_IDsOK.owl");


		// Now ask the manager to load the ontology
		OWLOntology ontology = null;
		try {
			ontology = manager.loadOntologyFromPhysicalURI(physicalURI);
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		}

		//create the converter, first of all add ontology to a set for the constructor
		java.util.Set<OWLOntology> ontologySet = new HashSet<OWLOntology>();
		ontologySet.add(ontology);			
		OWLEntityRenamer classRenamer = new OWLEntityRenamer(manager, ontologySet);

		OWLDataFactory df = manager.getOWLDataFactory();

		//build the list of already used IDs, and list of IDs to modify
		IDUsedChecker(ontology);

		int uriCount = 1;
		//browse the ones to modify
		for (URI toModify : listToModifyIDs) {
			System.out.println("to modify: "+toModify.toString());
			try {
				uriCount = getNextFreeID(1);
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}

			String newID = uriIDFormat.format(new Integer(uriCount));
			URI uriName = null;
			try {
				uriName = new URI("http://purl.obofoundry.org/obo/IAO_"+newID);
			} catch (URISyntaxException e) {
				e.printStackTrace();
			}

			/* keep the current ID as label if we don't already have one - eg http://purl.obofoundry.org/obo/String: we want to add String as label for the class	 */
			OWLClass classToModify = df.getOWLClass(toModify);
			//check if the class we are modifying already has a label
			if(classToModify.getAnnotations(ontology, OWLRDFVocabulary.RDFS_LABEL.getURI()).size() == 0 )
			{
				//we get the URI of the class and add it as label
				OWLAnnotation labelAnno = df.getOWLLabelAnnotation(classToModify.toString(), "en");
				OWLAxiom ax = df.getOWLEntityAnnotationAxiom(classToModify, labelAnno);

				try {
					manager.applyChange(new AddAxiom(ontology, ax));
				} catch (OWLOntologyChangeException e1) {
					e1.printStackTrace();
				}
			}

			/* apply the changes */
			try {
				manager.applyChanges(classRenamer.changeURI(toModify, uriName));
			} catch (OWLOntologyChangeException e) {
				e.printStackTrace();
			}
			//we add the newly assigned ID to the list of already used IDs
			listUsedIDs.add(uriName);
		}

		//if we want to check visually the list of IDs that are used
		/*for (Iterator it=listUsedIDs.iterator(); it.hasNext(); ) {
			System.out.println("used "+it.next());			
		}*/


		/* time to wrap up and save the file */
		try {
			manager.saveOntology(ontology, physicalURI2);


		} catch (UnknownOWLOntologyException e) {
			e.printStackTrace();
		} catch (OWLOntologyStorageException e) {
			e.printStackTrace();
		}




	}

}
