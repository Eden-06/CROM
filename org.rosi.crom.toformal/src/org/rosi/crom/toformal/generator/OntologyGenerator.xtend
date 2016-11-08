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
	var CROModel crom

	new() {
		super("owl")
		crom = new CROModel
	}
	
	override generate(IPath path, Model model) {	
		crom = new CROModel
		val visitor = new CROMVisitor
		var modelname = path.toFile.name.replace(".crom","")
		if (modelname.isEmpty)
			modelname = "CROMOntology"
		visitor.visit(crom, model)
		checkCompartmentInheritance
		checkNaturalTypes
//		checkRoleTypes
		return generate(modelname)
	}
	
	private def void checkCompartmentInheritance() {
		if (!crom.ctinh.empty)
	    	throw new CROMOntologyGeneratorException(
	    		"Compartment inheritance is not supported by the ontology generator!\n"
	    		+ crom.ctinh.join("\n"))
	}
	
	private def void checkNaturalTypes() {
		if (crom.nt.length != naturalTypes.length)
			throw new CROMOntologyGeneratorException(
				"Multiple natural types with identical names are detected!\n"
				+ "Names must be unique!")
	}

	// TODO How to check if the same role types appear in one CT?
//	private def checkRoleTypes() {	
//		compartmentTypes.forEach[ compType | {
//			println(crom.fills)
//			println(compType)
//			println(getParticipatingRoleTypes(compType).length)
//			println(crom.fills.filter[ entry | entry.key.value == compType]
//					.map[ entry | entry.value ].length)
//			if (getParticipatingRoleTypes(compType).length
//				!= crom.fills.filter[ entry | entry.key.value == compType]
//					.map[ entry | entry.value ].length)
//				throw new CROMOntologyGeneratorException(
//					"Multiple role types with identical names with one compartment type are detected!\n"
//					+ "Within one compartment type names must be unique!")
//		}]
//	}

	private def String printOWLcomment(String message) '''
	
	
	
	
	#
	# «message»
	#
	'''

	/**
	 * This method adds the prefix to a string, so it becomes a valid IRI.
	 * 
	 * @param str Input string
	 * @return the complete IRI.
	 */
	private def String makeIRI(String str) '''rosi:«str»'''
//	private def makeIRI(String str) '''<rosi:«str»>'''
	
	/** A macro for creating the IRI for "rigidity" */
	private def String rigidity() '''«makeIRI("rigidity")»'''
	
	/** Convert crom.ct from ArrayList<String> to Set<String> */
	private def Set<String> getCompartmentTypes() { return crom.ct.toSet }
	
	/** Convert crom.rst from ArrayList<String> to Set<String> */
	private def Set<String> getRelationshipTypes() { return crom.rst.toSet }
	
	/** Convert crom.rt from ArrayList<String> to Set<String> */
	private def Set<String> getRoleTypes() { return crom.rt.toSet }
	
	/** Convert crom.nt from ArrayList<String> to Set<String> */
	private def Set<String> getNaturalTypes() { return crom.nt.toSet }
	
	/** 
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
		return crom.fills
			.filter[ entry | entry.key.value == compType ]
			.filter[ entry | entry.value == roleType ]
			.map[ entry | entry.key.key ]
			.toSet
	 }

	/** A Macro for creating an annotated disjoint union axiom	 */
	private def String AnnotatedDisJointUnionOf() {
		return "DisjointUnionOf:\n"
			 + "	Annotations: rdfs:label \"objectGlobal\"\n"
			 + "	"
	}
	
	/** A macro for creating an annotated subclass axiom */
	private def String AnnotatedSubClassOf(String superType) {
		return "SubClassOf:\n"
			 + "	Annotations: rdfs:label \"objectGlobal\"\n"
			 + "	" + makeIRI(superType) 
	}
	
	/** A macro for creating an annotated subclass axiom */
	private def String AnnotatedDisjointClasses() {
		return "DisjointClasses:\n"
			 + "	Annotations: rdfs:label \"objectGlobal\"\n"
			 + "	" 
	}
	
	/**
	 * This method retrieves the set of all subtypes of a given natural type. 
	 */
	private def Set<String> getNaturalSubTypes(String superType){
		return crom.ntinh.filter[ entry | entry.value == superType ]
			.map[ entry | entry.key ]
			.toSet
	}
	
	/** 
	 * This method retrieves the set of all natural types that don't have a supertype.
	 */
	private def Set<String> getTopLevelNaturalTypes() {
		return naturalTypes
			.filter[ naturalType | !crom.ntinh.map[ entry | entry.key].contains(naturalType)]
			.toSet
	}
	
	
	
	
	/**
	 * This method prints the header of the ontology.
	 */
	private def String printHeader(String modelname) '''
	#
	# This ontology is automatically written by the FRaMED OWL Ontology generator
	#
	# The following features are not supported:
	#       - Compartment inheritance, because too many weird things can happen, which are not checked in FRaMED
	#
	#       - Compartments that play a role in another compartment, because on object level it would enforce a new
	#         compartment instance, i.e. a new individual on meta level
	#
	#       - intra relationship constraints like reflexive or symmetric, since actually that are constraints on the
	#         objects that play the roles and not on the roles themselves.
	#
	Prefix: owl: <http://www.w3.org/2002/07/owl#>
	Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	Prefix: xml: <http://www.w3.org/XML/1998/namespace>
	Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
	Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
	
	Prefix: rosi: <http://www.rosi-project.org/ontologies#>
	
	Ontology: <http://www.rosi-project.org/ontologies/«dateFormat.format(cal.getTime())»/«modelname»>
	'''
	
	/**
	 * This method prints the OWL datatype and OWL annotation declarations.
	 */
	private def printAnnotationsAndDatatypes() '''
	«printOWLcomment("Used OWL datatypes")»
	
	Datatype: xsd:boolean
	
	#
	# Used OWL annotations
	#
	
	AnnotationProperty: «makeIRI("rigidity")»
	    Range: <http://www.w3.org/2001/XMLSchema#boolean>
	
	AnnotationProperty: «makeIRI("isMeta")»
	    Range: <http://www.w3.org/2001/XMLSchema#boolean>
	
	AnnotationProperty: rdfs:isDefinedBy
	'''
	
	/**
	 * This method prints all axioms for owl:Thing. Note here that axioms without annotation refer
	 * to the meta level, while axioms with annotation refer to the object level. It also prints
	 * some other general axioms and class declarations.
	 */
	private def printOWLThingAndOthers() '''
	«printOWLcomment("General axioms and declarations, independent of the CROModel")»
	Class: owl:Thing
	    SubClassOf:
	        «makeIRI("CompartmentTypes")»
	    DisjointUnionOf:
	        Annotations: rdfs:label "objectGlobal"
	         «makeIRI("NaturalTypes")», «makeIRI("RoleTypes")», {«makeIRI("cardinalityCounter")»}
	
	Class: owl:Nothing
	
	Class: «makeIRI("Object")»
	    EquivalentTo:
	        owl:Nothing,
	        Annotations: rdfs:label "objectGlobal"
	        owl:Thing
	
	Individual: «makeIRI("cardinalityCounter")»
	'''
	
	/**
	 * This method creates the output for the compartment types that occur in the CROModel. It does
	 * not handle the inheritance relation between compartment types.
	 */
	private def printCompartmentTypes() '''
	«printOWLcomment("The declaration of all compartment types that occur in the model")»
	
	Class: «makeIRI("CompartmentTypes")»
	   SubClassOf:
	        Annotations: rdfs:label "objectGlobal"
	        owl:Nothing
	«compartmentTypes.join("    DisjointUnionOf: ", ", ", "\n", [ elem | makeIRI(elem) ])»
	
	«compartmentTypes.join("\n", [ compType | '''
		Class: «makeIRI(compType)»
			«roleTypes.filter[ roleType | !getFillerTypes(roleType,compType).empty ]
			    .join("", "\n", "\n", [ roleType | "SubClassOf: " + makeIRI("Plays" + roleType + "In" + compType) ])»
			«roleTypes.filter[ roleType | !getParticipatingRoleTypes(compType).contains(roleType) ]
			    .join("", "\n", "\n", [ roleType | "SubClassOf: " + makeIRI("Plays" + roleType + "IsBottom") ])»
			«relationshipTypes.filter[ relType | crom.rel.containsKey(relType -> compType)]
				.join("", "\n", "\n", [ relType | '''
			    	SubClassOf: «makeIRI(relType + "DomainIn" + compType)»
			    	SubClassOf: «makeIRI(relType + "RangeIn" + compType)»''' ])»
			«relationshipTypes.filter[ relType | !crom.rel.containsKey(relType -> compType)]
			    .join("", "\n", "\n", [relType | '''
			    	SubClassOf: «makeIRI(relType + "IsBottom")»''' ])» ''' ])»
	'''

	/**
	 * This method creates the output for the natural types that occur in the CROModel. It also
	 * handles the inheritance relation between natural types
	 */
	private def printNaturalTypes() '''
	«printOWLcomment("The declaration of all natural types that occur in the model")»
	
	Class: «makeIRI("NaturalTypes")»
	    SubClassOf:
	        owl:Nothing
		«getTopLevelNaturalTypes().join(AnnotatedDisJointUnionOf(), ", ", "\n", [ x | makeIRI(x) ])»
		«roleTypes.join("\n", [ roleType | '''
		SubclassOf:
			Annotations: rdfs:label "objectGlobal"
			«makeIRI("plays")» max 1 «makeIRI(roleType)»''' ])»
	
	«naturalTypes.join("", "\n", "\n", [ naturalType |  '''
		Class: «makeIRI(naturalType)»
			Annotations: «rigidity()» true
			«crom.ntinh.filter[entry | entry.key == naturalType]
				.join("", "\n", "\n", [ entry | AnnotatedSubClassOf(entry.value)])»
		«getNaturalSubTypes(naturalType)
    		.join(AnnotatedDisjointClasses, ", ", "\n", [ x | makeIRI(x) ])»'''])»
	'''

	/**
	 * This method creates the output for the role types that occur in the CROModel.
	 */
	private def printRoleTypes() '''
	«printOWLcomment("The declaration of all role types that occur in the model")»
	
	Class: «makeIRI("RoleTypes")»
	    SubClassOf:
	        Annotations: rdfs:label "objectGlobal"
	        «makeIRI("Object")»
	    SubClassOf:
	        Annotations: rdfs:label "objectGlobal"
	        inverse («makeIRI("plays")») exactly 1 owl:Thing
	    EquivalentTo:
	        Annotations: rdfs:label "objectGlobal"
	        inverse («makeIRI("roleCount")») exactly 1 {«makeIRI("cardinalityCounter")»}
		«roleTypes.join(AnnotatedDisJointUnionOf(), ", ", "\n", [ x | makeIRI(x) ] )»
	ObjectProperty: «makeIRI("roleCount")»
	
	«roleTypes.join("\n\n", [ roleType | '''
		Class: «makeIRI(roleType)»
		    Annotations: «rigidity()» false
		Class: «makeIRI("Plays" + roleType)»
		    EquivalentTo:
		        Annotations: rdfs:label "objectGlobal"
		        «makeIRI("plays")» some «makeIRI(roleType)»
		    SubClassOf:
		        Annotations: rdfs:isDefinedBy «makeIRI("Plays" + roleType + "IsBottom")»
		        owl:Nothing
		«compartmentTypes.filter[ compType | !getFillerTypes(roleType,compType).empty ]
		    	.join("\n", [ compType | '''
		    		SubClassOf:
		    			Annotations: rdfs:isDefinedBy «makeIRI("Plays" + roleType + "In" + compType)»
		    			«getFillerTypes(roleType, compType).join(" or ", [ nt | makeIRI(nt) ])»
		    	Class: «makeIRI("Plays" + roleType + "In" + compType)»'''])»
		Class: «makeIRI("Plays" + roleType + "IsBottom")»''' ])»
	'''

	/**
	 * This method introduces the plays relation and the correct fills relation as domain range
	 * constraints dependent on the contexts
	 */
	private def printPlays() '''
	«printOWLcomment("The declaration of the plays relation as OWL object property")»
	
	ObjectProperty: owl:bottomObjectProperty
	
	ObjectProperty: «makeIRI("plays")»
	    Domain:
	        Annotations: rdfs:label "objectGlobal"
	        «makeIRI("NaturalTypes")»
	    Range:
	        Annotations: rdfs:label "objectGlobal"
	        «makeIRI("RoleTypes")»
	    SubPropertyOf:
	        owl:bottomObjectProperty
	'''

	/**
	 * This method prints all relationshipTypes and its constraints.
	 */
	private def printRelationshipTypes() {
		return printOWLcomment("The declaration of all relationship types that occur in the model")
	 		+ ""
	 		+ relationshipTypes.join("\n",[ relType | 
	 			  "Class: " + makeIRI(relType + "IsBottom") + "\n"
	 			+ compartmentTypes
	 			    .filter[ compType | crom.rel.containsKey(relType -> compType)]
	 			    .join("", "\n", "\n", [compType | 
	 			    	  "Class: " + makeIRI(relType + "DomainIn" + compType) + "\n"
	 			    	+ "Class: " + makeIRI(relType + "RangeIn" + compType) ])
	 			+ "ObjectProperty: " + makeIRI(relType) + "\n"
	 			+ "    SubPropertyOf:\n"
	 			+ "        owl:bottomObjectProperty\n"
	 			+ "    Domain:\n"
	 			+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "IsBottom") + "\n"
	 			+ "        owl:Nothing\n"
	 			+ compartmentTypes
	 			    .filter[ compType | crom.rel.containsKey(relType -> compType)]
	 			    .join("\n", [ compType | 
	 			    	  "    Domain:\n"
	 			    	+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "DomainIn" + compType) + "\n"
	 			    	+ "        " + makeIRI(crom.rel.get(relType -> compType).key) + "\n"
	 			    	+ "    Range:\n"
	 			    	+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "RangeIn" + compType) + "\n"
	 			    	+ "        " + makeIRI(crom.rel.get(relType -> compType).value)]) + "\n"
//	 			+ compartmentTypes
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
# crom.nt: «naturalTypes»
# crom.ct: «compartmentTypes»
# crom.rt: «roleTypes»
# crom.rst: «relationshipTypes»
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