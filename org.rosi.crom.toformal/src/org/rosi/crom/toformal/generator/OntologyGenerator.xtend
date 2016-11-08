package org.rosi.crom.toformal.generator

import crom_l1_composed.Model
import java.text.SimpleDateFormat
import java.util.ArrayList
import java.util.Calendar
import java.util.HashSet
import java.util.Set
import org.eclipse.core.runtime.IPath
import org.rosi.crom.toformal.builder.CROMVisitor
import org.rosi.crom.toformal.builder.CROModel

class OntologyGenerator extends AbstractCROMGenerator {

	val dateFormat = new SimpleDateFormat("YYYY-MM-dd")
	val cal = Calendar.getInstance()	
	val crom = new CROModel

	new() {
		super("owl")
	}
	
	override generate(IPath path, Model model) {
			val visitor = new CROMVisitor
			var modelname = path.toFile.name.replace(".crom","")
			if (modelname.isEmpty)
				modelname = "CROMOntology"
			visitor.visit(crom, model)
			checkCompartmentInheritance
			return generate(modelname)
	}
	
	private def checkCompartmentInheritance() {
		if (!crom.ctinh.empty)
	    	throw new CROMOntologyGeneratorException(
	    		"Compartment inheritance is not supported by the ontology generator!\n"
	    		+ crom.ctinh.join("\n"))
	}



	private def printOWLcomment(String message) {
		return "\n\n\n\n\n"
			 + "#\n"
			 + "# " + message + "\n"
			 + "#\n" 
	}

	/**
	 * This method adds the prefix to a string, so it becomes a valid IRI.
	 * 
	 * @param str Input string
	 * @return the complete IRI.
	 */
	private def makeIRI(String str) '''rosi:«str»'''
//	private def makeIRI(String str) '''<rosi:«str»>'''
	
	/**
	 * A macro for creating the IRI for "rigidity"
	 */
	private def rigidity() '''«makeIRI("rigidity")»'''
	

	
	
	
	
	
	
	
	/**
	 * This method prints the header of the ontology.
	 */
	private def printHeader(String modelname){
	 	return "#\n"
	 		 + "# This ontology is automatically written by the FRaMED OWL Ontology generator\n"
	 		 + "#\n"
	 		 + "# The following features are not supported:\n"
			 + "#       - Compartment inheritance, because too many weird things can happen, which are not checked in FRaMED\n"
			 + "#\n"
			 + "#       - Compartments that play a role in another compartment, because on object level it would enforce a new\n"
			 + "#         compartment instance, i.e. a new individual on meta level\n"
			 + "#\n"
			 + "#       - intra relationship constraints like reflexive or symmetric, since actually that are constraints on the\n"
			 + "#         objects that play the roles and not on the roles themselves.\n"
	 		 + "#\n"
	 		 + "Prefix: owl: <http://www.w3.org/2002/07/owl#>\n"
	 		 + "Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
	 		 + "Prefix: xml: <http://www.w3.org/XML/1998/namespace>\n"
	 		 + "Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n"
	 		 + "Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
	 		 + "\n"
	 		 + "Prefix: rosi: <http://www.rosi-project.org/ontologies#>\n"
	 		 + "\n"
	 		 + "Ontology: <http://www.rosi-project.org/ontologies/" + dateFormat.format(cal.getTime()) + "/" + modelname + ">\n"
	 		 + "\n"
	 		 + "\n"
	 }	
	
	/**
	 * This method prints the OWL datatype and OWL annotation declarations.
	 */
	private def printAnnotationsAndDatatypes() {
		return printOWLcomment("Used OWL datatypes")
			 + "\n"
			 + "Datatype: xsd:boolean\n"
			 + "\n"
			 + "\n"
			 + "#\n"
			 + "# Used OWL annotations\n"
			 + "#\n"
			 + "\n"
			 + "AnnotationProperty: " + makeIRI("rigidity") + "\n"
			 + "    Range: <http://www.w3.org/2001/XMLSchema#boolean>\n"
			 + "\n"
			 + "AnnotationProperty: " + makeIRI("isMeta") +"\n"
			 + "    Range: <http://www.w3.org/2001/XMLSchema#boolean>\n"
			 + "\n"
			 + "AnnotationProperty: rdfs:isDefinedBy\n"
			 + "\n"
	}
	
	/**
	 * This method prints all axioms for owl:Thing. Note here that axioms without annotation refer
	 * to the meta level, while axioms with annotation refer to the object level. It also prints
	 * some other general axioms and class declarations.
	 */
	private def printOWLThingAndOthers() {
		return printOWLcomment("General axioms and declarations, independent of the CROModel")
			+ "Class: owl:Thing\n"
			+ "    SubClassOf:\n"
			+ "        " + makeIRI("CompartmentTypes") + "\n"
			+ "    DisjointUnionOf:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "         " + makeIRI("NaturalTypes") + ", " + makeIRI("RoleTypes") + ", {"+ makeIRI("cardinalityCounter") + "}\n"
			+ "\n"
			+ "Class: owl:Nothing\n"
			+ "\n"
			+ "Class: " + makeIRI("Object") + "\n"
			+ "    EquivalentTo:\n"
			+ "        owl:Nothing,\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        owl:Thing\n"
			+ "\n"
			+ "Individual: " + makeIRI("cardinalityCounter") + "\n"
			
	}
	
	/*
 	* This method retrieves all role types that could be played in a given compartment type
 	*/
	private def Set<String> getParticipatingRoleTypes(String compartmentType) {	
		return crom.fills
			.filter[ entry | entry.key.value == compartmentType]
			.map[ entry | entry.value ]
			.toSet
	}
	
	/**
	 * This method retrieves the set of all natural types that can play a certain role type in a
	 * certain compartment type.
	 */
	 private def Set<String> getFillerTypes(String roleType, String compType) {
	 	val result = new HashSet()
	 	crom.fills.filter[ entry | entry.key.value == compType ]
	 	             .filter[ entry | entry.value == roleType ]
	 	             .forEach[ entry | result.add(entry.key.key)]
	 	return result
	 }
	
	/**
	 * This method creates the output for the compartment types that occur in the CROModel. It does
	 * not handle the inheritance relation between compartment types.
	 */
	private def printCompartmentTypes() {
		return printOWLcomment("The declaration of all compartment types that occur in the model")
			+ "Class: " + makeIRI("CompartmentTypes") + "\n"
			+ "   SubClassOf:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        owl:Nothing\n"
			+ crom.ct.join("    DisjointUnionOf: ", ", ", "\n", [ elem | makeIRI(elem) ])
			+ "\n"
			+ crom.ct.join("\n", [ compType | 
			      "Class: " + makeIRI(compType) + "\n"
			    + crom.rt
			    	.filter[ roleType | !getFillerTypes(roleType,compType).empty ]
			    	.join("", "\n", "\n", [ roleType | "    SubClassOf: " + makeIRI("Plays" + roleType + "In" + compType) ])
			    + crom.rt
			    	.filter[ roleType | !getParticipatingRoleTypes(compType).contains(roleType) ]
			    	.join("", "\n", "\n", [ roleType | "    SubClassOf: " + makeIRI("Plays" + roleType + "IsBottom") ])
			    + crom.rst
			        .filter[ relType | crom.rel.containsKey(relType -> compType)]
			        .join("", "\n", "\n", [ relType | 
			              "    SubClassOf: " + makeIRI(relType + "DomainIn" + compType) + "\n"
			            + "    SubClassOf: " + makeIRI(relType + "RangeIn" + compType) ])
			    + crom.rst
			    	.filter[ relType | !crom.rel.containsKey(relType -> compType)]
			    	.join("", "\n", "\n", [relType | "    SubClassOf: " + makeIRI(relType + "IsBottom") ])
			    	
			    ]) + "\n"
	}

	private def AnnotatedDisJointUnionOf() {
		return "    DisjointUnionOf:\n"
			 + "        Annotations: rdfs:label \"objectGlobal\"\n"
			 + "        "
	}
	
	private def AnnotatedInheritance(String superType) {
		return "\n"
			 + "        Annotations: rdfs:label \"objectGlobal\"\n"
			 + "        " + makeIRI(superType) 
	}

	private def Set<String> getNaturalSubTypes(String superType){
		return crom.ntinh.filter[ entry | entry.value == superType ]
			.map[ entry | entry.key ]
			.toSet
	}
	
	private def Set<String> getTopLevelNaturalTypes() {
		return crom.nt
			.filter[ naturalType | !crom.ntinh.map[ entry | entry.key].contains(naturalType)]
			.toSet
	}
	
	/**
	 * This method creates the output for the natural types that occur in the CROModel. It also
	 * handles the inheritance relation between natural types
	 */
	private def printNaturalTypes() {
		return printOWLcomment("The declaration of all natural types that occur in the model")
			+ "Class: " + makeIRI("NaturalTypes") + "\n"
			+ "    SubClassOf:\n"
			+ "        owl:Nothing\n"
			+ getTopLevelNaturalTypes().join(AnnotatedDisJointUnionOf(), ", ", "\n", [ x | makeIRI(x) ])
			+ crom.rt.join("\n", [ roleType | "    SubclassOf:\n"
				+ "        Annotations: rdfs:label \"objectGlobal\"\n"
				+ "        " + makeIRI("plays") + " max 1 " + makeIRI(roleType) ]) +"\n"
			+ "\n"
		    + crom.nt.join("", "\n", "\n", [ naturalType |  "Class: " + makeIRI(naturalType) + "\n"
		        + "    Annotations: " + rigidity() + " true\n"
	            + crom.ntinh.filter[entry | entry.key == naturalType]
					.join("    SubClassOf: ", ", ", "\n", [ entry | AnnotatedInheritance(entry.value)])
    			+ getNaturalSubTypes(naturalType)
    				.join("DisjointClasses:\n"
    					+ "    Annotations: rdfs:label \"objectGlobal\"\n"
    					+ "    ", ", ", "\n", [ x | makeIRI(x) ])
    	        ])
	}
	

	/**
	 * This method creates the output for the role types that occur in the CROModel.
	 */
	private def printRoleTypes() {
		return printOWLcomment("The declaration of all role types that occur in the model")
			+ "\n"
			+ "Class: " + makeIRI("RoleTypes") + "\n"
			+ "    SubClassOf:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        " + makeIRI("Object") + "\n"
			+ "    SubClassOf:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        inverse (" + makeIRI("plays") + ") exactly 1 owl:Thing\n"
			+ "    EquivalentTo:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        inverse (" + makeIRI("roleCount") + ") exactly 1 {" + makeIRI("cardinalityCounter") + "}\n"
			+ crom.rt.join(AnnotatedDisJointUnionOf(), ", ", "\n", [ x | makeIRI(x) ] )
			+ "\n"
			+ "ObjectProperty: " + makeIRI("roleCount") + "\n"
			+ "\n"
	        + crom.rt.join("\n\n", [ roleType | 
	        	  "Class: " + makeIRI(roleType) + "\n"
	        	+ "    Annotations: " + rigidity() + " false\n"
	        	+ "Class: " + makeIRI("Plays" + roleType) + "\n"
	        	+ "    EquivalentTo:\n"
			    + "        Annotations: rdfs:label \"objectGlobal\"\n"
	        	+ "        " + makeIRI("plays") + " some " + makeIRI(roleType) + "\n"
	        	+ "    SubClassOf:\n"
	        	+ "        Annotations: rdfs:isDefinedBy " + makeIRI("Plays" + roleType + "IsBottom") +"\n"
	        	+ "        owl:Nothing\n"
	        	+ crom.ct.filter[ compType | !getFillerTypes(roleType,compType).empty ]
	                        .join("\n", [ compType |
	        		      "    SubClassOf:\n"
	                    + "        Annotations: rdfs:isDefinedBy " + makeIRI("Plays" + roleType + "In" + compType) + "\n"
	        	        + "        " + getFillerTypes(roleType, compType).join(" or ", [ nt | makeIRI(nt) ]) + "\n"
	        	        + "Class: " + makeIRI("Plays" + roleType + "In" + compType) ]) + "\n"
	        	+ "Class: " + makeIRI("Plays" + roleType + "IsBottom") +"\n"
	        	  ]) + "\n"
	}

	/**
	 * This method introduces the plays relation and the correct fills relation as domain range
	 * constraints dependent on the contexts
	 */
	private def printPlays() {
		return printOWLcomment("The declaration of the plays relation as OWL object property")
			+ "\n"
			+ "ObjectProperty: owl:bottomObjectProperty\n"
			+ "\n"
			+ "ObjectProperty: " + makeIRI("plays") + "\n"
			+ "    Domain:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        " + makeIRI("NaturalTypes") + "\n"
			+ "    Range:\n"
			+ "        Annotations: rdfs:label \"objectGlobal\"\n"
			+ "        " + makeIRI("RoleTypes") + "\n"
			+ "    SubPropertyOf:\n"
			+ "        owl:bottomObjectProperty\n"
			+ "\n"
	}

	



	/**
	 * This method prints all relationshipTypes and its constraints.
	 */
	 private def printRelationshipTypes() {
	 	return printOWLcomment("The declaration of all relationship types that occur in the model")
	 		+ ""
	 		+ crom.rst.join("\n",[ relType | 
	 			  "Class: " + makeIRI(relType + "IsBottom") + "\n"
	 			+ crom.ct
	 			    .filter[ compType | crom.rel.containsKey(relType -> compType)]
	 			    .join("\n", [compType | {
	 			    	  "Class: " + makeIRI(relType + "DomainIn" + compType) + "\n"
	 			    	+ "Class: " + makeIRI(relType + "RangeIn" + compType) + "\n"
	 			    }])
	 			+ "ObjectProperty: " + makeIRI(relType) + "\n"
	 			+ "    SubPropertyOf:\n"
	 			+ "        owl:bottomObjectProperty\n"
	 			+ crom.ct
	 			    .filter[ compType | crom.rel.containsKey(relType -> compType)]
	 			    .join("\n", [ compType | 
	 			    	  "    Domain:\n"
	 			    	+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "DomainIn" + compType) + "\n"
	 			    	+ "        " + makeIRI(crom.rel.get(relType -> compType).key) + "\n"
	 			    	+ "    Domain:\n"
	 			    	+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "IsBottom") + "\n"
	 			    	+ "        owl:Nothing\n"
	 			    	+ "    Range:\n"
	 			    	+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "RangeIn" + compType) + "\n"
	 			    	+ "        " + makeIRI(crom.rel.get(relType -> compType).value)]) + "\n"
//	 			+ crom.ct
//	 			    .filter[ compType | crom.card.containsKey(relType -> compType)]
//	 			    .join("\n", [compType | "" + crom.card.get(relType -> compType).key.lower ]) 
	 		])
	 }


	private def String generate(String modelname) '''
«printHeader(modelname)»

«printAnnotationsAndDatatypes()»

«printOWLThingAndOthers()»

«printCompartmentTypes()»

«printNaturalTypes()»

«printRoleTypes()»

«printPlays()»

«printRelationshipTypes()»

# TODO:
# gibt es bei rel jedes rst immer nur einmal? ja, wenn es keine ctinh gibt, im allg nein
# Thema Rollengruppen, wo steht fills
# occurence constraints
# cardinal constraints: [RT1] 2..5 -------rst1-------- 1..* [RT1] bedeutet jede Rolle vom Typ RT1 hat rst1-Verbindungen zu min 2 und max 5 anderen Rollen -> Leserichtung genau anders herum

#
# crom.nt: «crom.nt»
# crom.ct: «crom.ct»
# crom.rt: «crom.rt»
# crom.rst: «crom.rst»
#
# crom.ntinh: «crom.ntinh»
# crom.ctinh: «crom.ctinh»
# crom.fills: «crom.fills»
# crom.newFills: «crom.newFills»
# crom.rel: «crom.rel»
#
# crom.card: «crom.card»
# crom.rolec: «crom.rolec»
# crom.intra: «crom.intra»
# crom.inter: «crom.inter»
#
#
# Contraint model:
# rolec
# card
# intra
# inter
#
#
'''	

	public def newFills(CROModel model) {
		val r = new ArrayList<Pair<String, String>>
		for (e : model.fills)
			r.add(e.key.key -> e.value)
		return r
	}

//	public def parts(CROModel model) {
//		val parts = new HashMap<String, List<String>>
//		for (e : model.fills) {
//			if (! parts.containsKey(e.key.value))
//				parts.put(e.key.value, new ArrayList<String>)
//			parts.get(e.key.value).add(e.value)
//		}
//		return parts
//	}

}
class CROMOntologyGeneratorException extends RuntimeException {
	new(String message) {
		super("\n" + message)
	}
}