package org.rosi.crom.toformal.generator

import crom_l1_composed.Model
import java.text.SimpleDateFormat
import java.util.ArrayList
import java.util.Calendar
import java.util.Set
import org.eclipse.core.runtime.IPath
import org.rosi.crom.toformal.builder.CROMVisitor
import org.rosi.crom.toformal.builder.CROModel
import org.rosi.crom.toformal.builder.Cardinality
import java.util.List
import org.rosi.crom.toformal.builder.RoleGroup
import java.util.Map

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
		return printOntology(modelname)
	}
	
	/**
	 * This method checks whether in the given CROM an inheritance relation between compartment
	 * types is present, and if so throws an exception. 
	 */
	private def void checkCompartmentInheritance() {
		if (!crom.ctinh.empty)
	    	throw new CROMOntologyGeneratorException(
	    		"Compartment inheritance is not supported by the ontology generator!\n"
	    		+ crom.ctinh.join("\n"))
	}
	
	/**
	 * This method checks whether in the given CROM all names of natural types are distinct, and if
	 * not so throws an exception. 
	 */
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

	/**
	 * A macro for a comment in an OWL file, used to structure the ontology document.
	 */
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
	private def String makeIRI(String str) {
		return "rosi:" + str
	}
	
	/** 
	 * A macro for creating the IRI for "rigidity".
	 */
	private def String rigidity() {
		return makeIRI("rigidity")
	}
	
	/** 
	 * Convert crom.ct from ArrayList<String> to Set<String>.
	 */
	private def Set<String> getCompartmentTypes() {
		return crom.ct.toSet
	}
	
	/** 
	 * Convert crom.rst from ArrayList<String> to Set<String>.
	 */
	private def Set<String> getRelationshipTypes() {
		return crom.rst.toSet
	}
	
	/** 
	 * Convert crom.rt from ArrayList<String> to Set<String>.
	 */
	private def Set<String> getRoleTypes() {
		return crom.rt.toSet
	}
	
	/** 
	 * Convert crom.nt from ArrayList<String> to Set<String>.
	 */
	private def Set<String> getNaturalTypes() {
		return crom.nt.toSet
	}
	
	/** 
	 * This method retrieves all role types that could be played in a given compartment type.
	 */
	private def Set<String> getParticipatingRoleTypes(String compartmentType) {	
		return crom.fills
			.filter[ entry | entry.key.value.equals(compartmentType) ]
			.map[ entry | entry.value ]
			.toSet
	}
	
	/**
	 * This method retrieves the set of all natural types that can play a certain role type in a
	 * certain compartment type.
	 */
	private def Set<String> getFillerTypes(String roleType, String compType) {
		return crom.fills
			.filter[ entry | entry.key.value.equals(compType) ]
			.filter[ entry | entry.value.equals(roleType) ]
			.map[ entry | entry.key.key ]
			.toSet
	 }

	/** 
	 * A Macro for creating an annotated disjoint union axiom.
	 */
	private def String AnnotatedDisJointUnionOf() {
		return "DisjointUnionOf:\n"
			 + "	Annotations: rdfs:label \"objectGlobal\"\n"
			 + "	"
	}
	
	/** 
	 * A macro for creating an annotated subclass axiom.
	 */
	private def String AnnotatedSubClassOf(String superType) {
		return "SubClassOf:\n"
			 + "	Annotations: rdfs:label \"objectGlobal\"\n"
			 + "	" + makeIRI(superType) 
	}
	
	/** 
	 * A macro for creating an annotated subclass axiom
	 */
	private def String AnnotatedDisjointClasses() {
		return "DisjointClasses:\n"
			 + "	Annotations: rdfs:label \"objectGlobal\"\n"
			 + "	" 
	}
	
	/**
	 * This method retrieves the set of all subtypes of a given natural type. 
	 */
	private def Set<String> getNaturalSubTypes(String superType){
		return crom.ntinh.filter[ entry | entry.value.equals(superType) ]
			.map[ entry | entry.key ]
			.toSet
	}
	
	/** 
	 * This method retrieves the set of all natural types that don't have a super type.
	 */
	private def Set<String> getTopLevelNaturalTypes() {
		return naturalTypes
			.filter[ naturalType | !crom.ntinh.map[ entry | entry.key].contains(naturalType)]
			.toSet
	}
	
	/**
	 * Given a cardinality constraint and the according relationship type, and compartment type,
	 * this method creates the appropriate sub class axiom.
	 */
	private def String getAxiomIfConstraining(Cardinality card, String relType, String compType, String domOrRan) {
		return (if (card.lower > 0 || card.upper != -1)
				  "    SubClassOf:\n"
				+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "CardinalConstraintOf" + domOrRan + "In" + compType) + "\n"
				+ "        " 
				else "")
			+ (if (card.lower > 0)
				makeIRI(relType) + " min " + crom.card.get(relType -> compType).value.lower + " owl:Thing"
				else "")
			+ (if (card.lower > 0 && card.upper != -1)
				", "
				else "")
			+ (if (card.upper != -1)
				makeIRI(relType) + " max " + crom.card.get(relType -> compType).value.upper + " owl:Thing"
				else "")
			+ (if (card.lower > 0 || card.upper != -1)
				"\n"
				else "")
	}
	
	/**
	 * This method looks into the cardinality constraints of relationship types whose domain is the
	 * role type <code>roleType</code> in the compartment type <code>compType</code>. It then
	 * constructs the according subclass axioms for the cardinality constraints over all such
	 * relationship types. 
	 */
	private def String getRelationshipTypCardinalityOfDomain(String roleType, String compType) {
		return relationshipTypes.filter[ relType | crom.rel.containsKey(relType -> compType)]
			.filter[ relType | crom.rel.get(relType -> compType).key.equals(roleType) ]
			.join("", [ relType | getAxiomIfConstraining(crom.card.get(relType -> compType).value, relType, compType, "Domain") ])
	}
	
	/**
	 * This method creates the class declarations used in <code>getRelationshipTypCardinalityOfDomain</code>.
	 */
	private def String getRelationshipTypCardinalityOfDomainClassDef(String roleType, String compType, String defi) {
		return relationshipTypes.filter[ relType | crom.rel.containsKey(relType -> compType)]
			.filter[ relType | crom.rel.get(relType -> compType).key.equals(roleType) ]
			.join("",  [ relType | defi + makeIRI(relType + "CardinalConstraintOfDomainIn" + compType) + "\n" ])
	}
	
	/**
	 * This method looks into the cardinality constraints of relationship types whose range is the
	 * role type <code>roleType</code> in the compartment type <code>compType</code>. It then
	 * constructs the according subclass axioms for the cardinality constraints over all such
	 * relationship types. 
	 */
	private def String getRelationshipTypCardinalityOfRange(String roleType, String compType) {
		return relationshipTypes.filter[ relType | crom.rel.containsKey(relType -> compType)]
			.filter[ relType | crom.rel.get(relType -> compType).value.equals(roleType) ]
			.join("\n", [ relType | getAxiomIfConstraining(crom.card.get(relType -> compType).key, relType, compType, "Range")])
	}
	
	/**
	 * This method creates the class declarations used in <code>getRelationshipTypCardinalityOfRange</code>.
	 */
	private def String getRelationshipTypCardinalityOfRangeClassDef(String roleType, String compType, String defi) {
		return relationshipTypes.filter[ relType | crom.rel.containsKey(relType -> compType)]
			.filter[ relType | crom.rel.get(relType -> compType).value.equals(roleType) ]
			.join("", [ relType | defi + makeIRI(relType + "CardinalConstraintOfRangeIn" + compType) + "\n" ])
	}
	
	/**
	 * This method checks whether an object is a role group.
	 */
	private def Boolean isRoleGroup(Object obj) {
		return (obj.class.equals(RoleGroup))
	}
	
	/**
	 * This method tests whether an object is a role type, i.e. it checks whether the object is a
	 * string and whether it is contained in <code>roleTypes</code>.
	 */
	private def Boolean isRoleType(Object obj) {
		return (obj.class.equals(String) && roleTypes.contains(obj))
	}
	
	/**
	 * This method retrieves these occurrence constraints that talk about role types.
	 */
	private def Map<String,List<Pair<Cardinality,String>>> getOccurrenceConstraintsForRoleTypes() {
		return crom.rolec.filter[ compType, listOfConstraints | listOfConstraints.map[ e | e.value.isRoleType].contains(true) ]
			.mapValues[ listOfConstraints | listOfConstraints
				.filter[ e | e.value.isRoleType ]
				.map[ e | e.key -> e.value as String ]
				.toList]
	}
	
	/**
	 * This method retrieves these occurrence constraints that talk about role groups.
	 */
	private def Map<String,List<Pair<Cardinality,RoleGroup>>> getOccurrenceConstraintsForRoleGroups() {
		return crom.rolec.filter[ compType, listOfConstraints | listOfConstraints.map[ e | e.value.isRoleGroup].contains(true) ]
			.mapValues[ listOfConstraints | listOfConstraints
				.filter[ e | e.value.isRoleGroup ]
				.map[ e | e.key -> e.value as RoleGroup ]
				.toList]
	}
	
	/**
	 * This method retrieves the cardinality of the occurrence constraint for a certain role type
	 * in a compartment. If there is none, it returns <code>null</code>.
	 */
	private def Cardinality getOccurrenceConstraint(String compType, String roleType) {
		if (!occurrenceConstraintsForRoleTypes.containsKey(compType))
			return null
		return occurrenceConstraintsForRoleTypes
			.filter[ key, value | key.equals(compType) ]
			.get(compType)
			.filter[ entry | entry.value.equals(roleType) ]
			.map[ entry | entry.key ]
			.head
	}
	
	/**
	 * Given an occurrence constraint and the according role type and compartment type,
	 * this method creates the appropriate class assertion axiom.
	 */
	private def String getTypeAssertionIfConstraining(String roleType, String compType) {
		val card = getOccurrenceConstraint(compType, roleType)
		if (card == null) return null
		return (if (card.lower > 0 || card.upper != -1)
				  "    Types:\n"
				+ "        Annotations: rdfs:isDefinedBy " + makeIRI(roleType + "OccurrenceConstraintIn" + compType) + "\n"
				+ "        " 
				else "")
			+ (if (card.lower > 0)
				makeIRI("count") + " min " + card.lower + " " + makeIRI(roleType)
				else "")
			+ (if (card.lower > 0 && card.upper != -1)
				", "
				else "")
			+ (if (card.upper != -1)
				makeIRI("count") + " max " + card.upper + " " + makeIRI(roleType)
				else "")
			+ (if (card.lower > 0 || card.upper != -1)
				"\n"
				else "")
	}
	
	/**
	 * This method creates the class declarations used in <code>getOccurrenceTypeAssertionIfConstraining</code>.
	 */
	private def String getOccurrenceTypeAssertionClasses(String roleType, String compType, String defi) {
		val card = getOccurrenceConstraint(compType, roleType)
		if (card == null) return null
		return (if (card.lower > 0 || card.upper != -1)
			defi + makeIRI(roleType + "OccurrenceConstraintIn" + compType) + "\n" else "")
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
	private def String printAnnotationsAndDatatypes() '''
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
	private def String printOWLThingAndOthers() '''
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
	'''
	
	/**
	 * This method creates the output for the compartment types that occur in the CROModel. It does
	 * not handle the inheritance relation between compartment types.
	 */
	private def String printCompartmentTypes() '''
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
						SubClassOf: «makeIRI(relType + "IsBottom")»''' ])»
				«roleTypes.join("", [roleType |	getRelationshipTypCardinalityOfDomainClassDef(roleType, compType, "SubClassOf: ") ])»
				«roleTypes.join("", [roleType |	getRelationshipTypCardinalityOfRangeClassDef( roleType, compType, "SubClassOf: ") ])»
				«if (occurrenceConstraintsForRoleTypes.containsKey(compType))
					roleTypes.filter[ roleType | occurrenceConstraintsForRoleTypes.get(compType).map[ entry | entry.value].contains(roleType) ]
							.join("", [ roleType | getOccurrenceTypeAssertionClasses(roleType, compType, "SubClassOf: ") ]) else ""» 
				    	''' ])»
	'''

	/**
	 * This method creates the output for the natural types that occur in the CROModel. It also
	 * handles the inheritance relation between natural types.
	 */
	private def String printNaturalTypes() '''
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
				«crom.ntinh.filter[entry | entry.key.equals(naturalType) ]
					.join("", "\n", "\n", [ entry | AnnotatedSubClassOf(entry.value)])»
			«getNaturalSubTypes(naturalType)
    			.join(AnnotatedDisjointClasses, ", ", "\n", [ x | makeIRI(x) ])»'''])»
	'''

	/**
	 * This method creates the output for the role types that occur in the CROModel.
	 */
	private def String printRoleTypes() '''
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
	        inverse («makeIRI("count")») exactly 1 {«makeIRI("cardinalityCounter")»}
		«roleTypes.join(AnnotatedDisJointUnionOf(), ", ", "\n", [ x | makeIRI(x) ] )»
	
	«roleTypes.join("\n\n", [ roleType | '''
		Class: «makeIRI(roleType)»
		    Annotations: «rigidity()» false
		«compartmentTypes.join("", [ compType | getRelationshipTypCardinalityOfDomain(roleType, compType) ])»
		«compartmentTypes.join("", [ compType | getRelationshipTypCardinalityOfRange(roleType, compType) ])»
		«compartmentTypes.join("", [ compType | getRelationshipTypCardinalityOfDomainClassDef(roleType, compType, "Class: ") ])»
		«compartmentTypes.join("", [ compType | getRelationshipTypCardinalityOfRangeClassDef(roleType, compType, "Class: ") ])»
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
	 * constraints dependent on the contexts.
	 */
	private def String printPlays() '''
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
	private def String printRelationshipTypes() '''
	«printOWLcomment("The declaration of all relationship types that occur in the model")»
	
	«relationshipTypes.join("\n\n", [ relType | '''
		Class: «makeIRI(relType + "IsBottom")»
		«compartmentTypes.filter[ compType | crom.rel.containsKey(relType -> compType)]
			.join("", "\n", "\n", [compType | '''
					Class: «makeIRI(relType + "DomainIn" + compType)»
					Class: «makeIRI(relType + "RangeIn" + compType)»''' ])»
		ObjectProperty: «makeIRI(relType)»
		    SubPropertyOf:
		        owl:bottomObjectProperty
		    Domain:
		        Annotations: rdfs:isDefinedBy «makeIRI(relType + "IsBottom")»
		        owl:Nothing
		    «compartmentTypes.filter[ compType | crom.rel.containsKey(relType -> compType)]
		    	.join("\n", [ compType | '''
		    		Domain:
		    			Annotations: rdfs:isDefinedBy «makeIRI(relType + "DomainIn" + compType)»
		    		    «makeIRI(crom.rel.get(relType -> compType).key)»
		    		Range:
		    		    Annotations: rdfs:isDefinedBy «makeIRI(relType + "RangeIn" + compType)»
		    		    «makeIRI(crom.rel.get(relType -> compType).value)»''' ])»''' ])»
	'''

	/**
	 * This method prints all the occurrence constraints.
	 */
	private def String printOccurrenceConstraints() '''
		«printOWLcomment("The declaration of the counter nominal and all the occurrence constraints.")»
		
		ObjectProperty: «makeIRI("count")»
		
		Individual: «makeIRI("cardinalityCounter")»
			«compartmentTypes.filter[ compType | occurrenceConstraintsForRoleTypes.containsKey(compType) ]
				.join("", [compType |
					roleTypes.filter[ roleType | occurrenceConstraintsForRoleTypes.get(compType).map[ entry | entry.value].contains(roleType) ]
						.join("", [ roleType | getTypeAssertionIfConstraining(roleType, compType) ]) ])»
		«compartmentTypes.filter[ compType | occurrenceConstraintsForRoleTypes.containsKey(compType) ]
			.join("", [compType |
				roleTypes.filter[ roleType | occurrenceConstraintsForRoleTypes.get(compType).map[ entry | entry.value].contains(roleType) ]
					.join("", [ roleType | getOccurrenceTypeAssertionClasses(roleType, compType, "Class: ") ]) ])»
	'''



	/** 
	 * This method actually prints the whole ontology document in Manchester OWL syntax.
	 */
	private def String printOntology(String modelname) '''
		«printHeader(modelname)»
		«printAnnotationsAndDatatypes»
		«printOWLThingAndOthers»
		«printCompartmentTypes»
		«printNaturalTypes»
		«printRoleTypes»
		«printPlays»
		«printRelationshipTypes»
		«printOccurrenceConstraints»

		# TODO:
		# Thema Rollengruppen, wo steht fills
		# occurence constraints von Rollengruppen

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
		
		# occurrenceRoleTypes: 
		# «occurrenceConstraintsForRoleTypes»
		# «occurrenceConstraintsForRoleTypes.filter[ key, value | key.equals("CompartmentType3") ]»
		# «occurrenceConstraintsForRoleTypes.filter[ key, value | key.equals("CompartmentType3")]
			.get("CompartmentType3").filter[ entry | entry.value.equals("RT5") ].map[ entry | entry.key ].get(0)»
		# occurrenceRoleGroups: «occurrenceConstraintsForRoleGroups»
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