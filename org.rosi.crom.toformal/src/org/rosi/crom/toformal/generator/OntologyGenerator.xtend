package org.rosi.crom.toformal.generator

import crom_l1_composed.Model
import java.text.SimpleDateFormat
import java.util.ArrayList
import java.util.Calendar
import java.util.List
import java.util.Map
import java.util.Set
import org.eclipse.core.runtime.IPath
import org.rosi.crom.toformal.builder.CROMVisitor
import org.rosi.crom.toformal.builder.CROModel
import org.rosi.crom.toformal.builder.Cardinality
import org.rosi.crom.toformal.builder.RoleGroup
import java.util.HashMap
import org.apache.commons.lang.StringUtils

class OntologyGenerator extends AbstractCROMGenerator {

	val dateFormat = new SimpleDateFormat("YYYY-MM-dd")
	val cal = Calendar.getInstance()
	var int numberOfRoleGroups
	val int headingWidth = 100
	
	var CROModel crom
	var Set<String> compartmentTypes
	var Set<String> naturalTypes
	var Set<String> roleTypes
	var Set<String> relationshipTypes
	var HashMap<String, RoleGroup> rgNames
	var Set<RoleGroup> roleGroups

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
		setUp()
		return printOntology(modelname)
	}
	
	
	

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods used in the setup																	 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

	private def void setUp() {
		
		numberOfRoleGroups = 0
		rgNames = new HashMap()
		
		retrieveCompartmentTypes
		retrieveNaturalTypes
		retrieveRoleTypes
		retrieveRelationshipTypes
		retrieveRoleGroupNames
		checkCompartmentInheritance
		checkNaturalTypes
		checkRelCardConnection
		checkInterRSTConstraintsEmpty
		checkIntraRSTConstraintsEmpty
//		checkRoleTypes
		
	}

	/** 
	 * Convert crom.ct from ArrayList<String> to Set<String>.
	 */
	private def void retrieveCompartmentTypes() {
		compartmentTypes = crom.ct.toSet
	}
	
	/** 
	 * Convert crom.rst from ArrayList<String> to Set<String>.
	 */
	private def void retrieveRelationshipTypes() {
		relationshipTypes = crom.rst.toSet
	}
	
	/** 
	 * Convert crom.rt from ArrayList<String> to Set<String>.
	 */
	private def void retrieveRoleTypes() {
		roleTypes = crom.rt.toSet
	}
	
	/** 
	 * Convert crom.nt from ArrayList<String> to Set<String>.
	 */
	private def void retrieveNaturalTypes() {
		naturalTypes = crom.nt.toSet
	}
	
	/**
	 * This method constructs the HashMap that identifies role groups by their (generated) names.
	 */
	private def void retrieveRoleGroupNames() {
		allRoleGroups.forEach[ roleGroup | rgNames.put(createNewRoleGroupName, roleGroup)]
		roleGroups = rgNames.values.toSet
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

	

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods to ensure certain properties of the crom before constructing the ontology			 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

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

	/**
	 * This method checks whether the key sets of the cardinal constraints and the relationship
	 * domain and range constraints are equal.
	 */
	private def void checkRelCardConnection() {
		if (!crom.rel.keySet.equals(crom.card.keySet))
			throw new CROMOntologyGeneratorException(
				"The key sets of the cardinal constraints and relationship domain and range constraints must be the same!"
			)
	}
	
	/**
	 * This method checks whether there are no inter relationship type constraints, and if so
	 * throws an exception since the ontology generator does not support these kind of constraints.
	 */
	 private def void checkInterRSTConstraintsEmpty() {
	 	if (!crom.inter.empty)
	 		throw new CROMOntologyGeneratorException(
	 			"Inter relationship type constraints, like relationship type implications, are not supported!"
	 		)
	 }
	
	/**
	 * This method checks whether there are no intra relationship type constraints, and if so
	 * throws an exception since the ontology generator does not support these kind of constraints.
	 */
	 private def void checkIntraRSTConstraintsEmpty() {
	 	if (!crom.intra.empty)
	 		throw new CROMOntologyGeneratorException(
	 			"Intra relationship type constraints, like irreflexive relationship type constraints, are not supported!"
	 		)
	 }

	

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Macros, i.e. methods without parameters or side effects that generate strings				 ///
////////////////////////////////////////////////////////////////////////////////////////////////////	

	/**
	 * A macro for a section comment in an OWL file, used to structure the ontology document.
	 */
	private def String section(String title) '''
		
		
		
		
		«StringUtils.repeat("#", headingWidth)»
		# «title» «if (title.length+4 <= headingWidth) StringUtils.repeat(" ", headingWidth - title.length - 4) + "#" else ""»
		«StringUtils.repeat("#", headingWidth)»
		
	'''

	/**
	 * A macro for a two line section comment in an OWL file, used to structure the ontology
	 * document.
	 */
	private def String section(String title, String secondLine) '''
		
		
		
		
		«StringUtils.repeat("#", headingWidth)»
		# «title» «if (title.length+4 <= headingWidth) StringUtils.repeat(" ", headingWidth - title.length - 4) + "#" else ""»
		# «secondLine» «if (secondLine.length+4 <= headingWidth) StringUtils.repeat(" ", headingWidth - secondLine.length - 4) + "#" else ""»
		«StringUtils.repeat("#", headingWidth)»
		
	'''

	/**
	 * A macro for a subsection in an OWL file, used to structure the ontology document.
	 */
	private def String subsection(String title) '''
		### «title» ###
		####«StringUtils.repeat("#",title.length)»####
		
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
	 * This method adds the prefix to the name of a role group, so it becomes a valid IRI.
	 * 
	 * @param roleGroup Input role group
	 * @return the complete IRI.
	 */
	private def String makeIRI(RoleGroup roleGroup) {
		return makeIRI(roleGroup.name)
	}
	
	/** 
	 * A macro for creating the IRI for "rigidity".
	 */
	private def String rigidity() {
		return makeIRI("rigidity")
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
	
	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Some auxiliary methods																		 ///
////////////////////////////////////////////////////////////////////////////////////////////////////	

	/**
	 * This method tests whether an object is a role type, i.e. it checks whether the object is a
	 * string and whether it is contained in <code>roleTypes</code>.
	 */
	private def Boolean isRoleType(Object obj) {
		return (obj.class.equals(String) && roleTypes.contains(obj))
	}
	
	/**
	 * This method checks whether an object is a role group.
	 */
	private def Boolean isRoleGroup(Object obj) {
		return (obj.class.equals(RoleGroup))
	}
		
	/**
	 * Given a role group, this method retrieves the associated name.
	 */
	private def String getName(RoleGroup roleGroup) {
		if (rgNames.containsValue(roleGroup))
			rgNames.filter[ name, rg | rg.equals(roleGroup) ].entrySet.head.key
		else ""
	}
	
	/**
	 * Given a cardinality is it constraining at all?
	 */
	private def Boolean isConstraining(Cardinality card) {
		return (card.lower > 0 || card.upper != -1)
	}
	 
	/**
	 * Given a cardinality is the lower bound constraining?
	 */
	private def Boolean isMinConstraining(Cardinality card) {
		return card.lower > 0
	}
	  
	/**
	 * Given a cardinality is the upper bound constraining?
	 */
	private def Boolean isMaxConstraining(Cardinality card) {
		return card.upper != -1
	}
	  
	/**
	 * Given a cardinality is the lower and the upper bound constraining?
	 */
	private def Boolean isMinMaxConstraining(Cardinality card) {
		return (card.lower > 0 && card.upper != -1)
	}
	   
	/**
	 * This method creates a generic axiom with number restrictions based on a cardinality.
	 * 
	 * @param prefix The first part of the axiom, either a subclass axiom or a class assertion
	 * @param metaConcept the meta concept used in the annotation of the object axiom
	 * @param card the cardinality
	 * @param dlRole the OWL object property used, i.e. <code>plays</code>, <code>count</code> or similar
	 * @param qualNumRestr the expression in the qualified number restriction, can be <code>owl:Thing</code> or a role type/group.
	 * @param compType the compartment type in which the object axiom should hold.
	 * 
	 * @return the correctly formatted owl axiom.
	 */
	private def String getCardAxiom(String prefix,
									String metaConcept,
									String prefix2, 
									Cardinality card,
									String dlRole,
									String qualNumRestr,
									String compType
	){
		if (card == null)
			return ""
		return (if (card.constraining)
				prefix
				+ "        Annotations: rdfs:isDefinedBy " + makeIRI(metaConcept) + "\n"
				+ "        " + prefix2
				else "")
			+ (if (card.minConstraining)
				"(" + dlRole + " min " + card.lower + " " + qualNumRestr + ")"
				else "")
			+ (if (card.minMaxConstraining)
				" and "
				else "")
			+ (if (card.maxConstraining)
				"(" + dlRole + " max " + card.upper + " " + qualNumRestr + ")"
				else "")
			+ (if (card.constraining)
				"\n\n"
				+ "Class: " + makeIRI(metaConcept) + "\n"
				+ "\n"
				+ "Class: " + makeIRI(compType) + "\n"
				+ "    SubClassOf:\n"
				+ "        " + makeIRI(metaConcept) + "\n"
				+ "\n"
				else "")
	}
	
	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods concerning natural type inheritance													 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

	/** 
	 * This method retrieves the set of all natural types that don't have a super type.
	 */
	private def Set<String> getTopLevelNaturalTypes() {
		return naturalTypes
			.filter[ naturalType | !crom.ntinh.map[ entry | entry.key].contains(naturalType)]
			.toSet
	}

	/**
	 * This method retrieves the set of all sub types of a given natural type. 
	 */
	private def Set<String> getNaturalSubTypes(String superType){
		return crom.ntinh.filter[ entry | entry.value.equals(superType) ]
			.map[ entry | entry.key ]
			.toSet
	}	
	

	

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods concerning the fills relation														 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

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
	 
	 
	 
	 
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods concerning role groups																 ///
////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * This method retrieves all top level role groups, i.e. role groups that are not nested within
	 * other role groups. These top level role groups also have occurrence constraints. 
	 */
	private def Set<RoleGroup> getTopLevelRoleGroups() {
		return occurrenceConstraintsForRoleGroups
			.mapValues[ listOfConstraints | listOfConstraints.map[ entry | entry.value ].toSet ]
			.entrySet
			.map[ entry | entry.value ]
			.flatten
			.toSet
	}	

	/**
	 * Given a role group <code>rg</code> this method retrieves all role groups that are nested in <code>rg</code>.
	 */
	private def Set<RoleGroup> getNestedRoleGroups(RoleGroup roleGroup) {
		return (roleGroup.elements
			.filter[ elem | elem.isRoleGroup ]
			.map[ elem | elem as RoleGroup ]
			.map[ elem | elem.nestedRoleGroups + #{elem} ]
			.flatten + #{roleGroup}).toSet
	}
	
	/**
	 * Given a role group <code>rg</code> this method retrieves all role types that are nested in <code>rg</code>.
	 */
	private def Set<String> getRoleGroupAtoms(RoleGroup roleGroup) {
		return (roleGroup.elements
				.filter[ elem | elem.isRoleType ]
				.map[ elem | elem as String ]
			+ roleGroup.elements
				.filter[ elem | elem.isRoleGroup ]
				.map[ elem | elem as RoleGroup ]
				.map[ elem | elem.roleGroupAtoms ]
				.flatten
			).toSet
	}

	/**
	 * This method retrieves all role groups that appear in the CROM.
	 */
	private def Set<RoleGroup> getAllRoleGroups() {
		topLevelRoleGroups.map[ roleGroup | roleGroup.nestedRoleGroups ].flatten.toSet
	}
	
	/**
	 * This method generates a fresh generic name for a role group and increases the counter of
	 * created role group names by 1.
	 */
	private def String createNewRoleGroupName() {
		numberOfRoleGroups++
		return "GeneratedRoleGroup" + numberOfRoleGroups
	}
	
	/**
	 * Given a role group this method retrieves the names of all its elements, i.e. role types are
	 * retrieved directly and if another role group is an element then the name is retrieved.
	 */
	private def Set<String> getElementNames(RoleGroup rg) {
		(rg.elements.filter[roleType].map[ elem | elem as String]
		+ rg.elements.filter[roleGroup].map[ elem | (elem as RoleGroup).name]).toSet
	}
	
	/**
	 * This method returns <code>true</code> if the given role groups appears in the given
	 * compartment type, and <code>false</code> otherwise.
	 */
	private def Boolean contains(String compType, RoleGroup roleGroup){

		if (!occurrenceConstraintsForRoleGroups.containsKey(compType)) return false

		return occurrenceConstraintsForRoleGroups
			.get(compType)
			.map[ elem | elem.value.nestedRoleGroups.contains(roleGroup) ]
			.contains(true)
	}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods for cardinal constraints of relationship types										 ///
////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * For a given relationship type, compartment type and role type this method returns the
	 * necessary axioms to ensure the cardinal constraints. If there are none for the given
	 * parameters, it return an empty string.  
	 */
	private def String getAllCardinalAxioms(String relType, String compType, String roleType) {
		return 
			if (crom.card.containsKey(relType -> compType)) {
				return 
					(if (crom.rel.get(relType -> compType).key.equals(roleType)) // roleType ----relType----> ...
						getCardAxiom(
							"Class: " + makeIRI(roleType) + "\n" + "    SubClassOf:\n",
							relType + "CardinalConstraintOfDomainIn" + compType,
							"",
							crom.card.get(relType -> compType).value,
							makeIRI(relType),
							"owl:Thing",
							compType)
					else (if (crom.rel.get(relType -> compType).value.equals(roleType)) // ... ----relType----> roleType
						getCardAxiom(
							"Class: " + makeIRI(roleType) + "\n" + "    SubClassOf:\n",
							relType + "CardinalConstraintOfRangeIn" + compType,
							"",
							crom.card.get(relType -> compType).key,
							"inverse (" + makeIRI(relType) + ")",
							"owl:Thing",
							compType)
					else ""))
			} else ""
	}
	
	
	
	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods for occurrence constraints															 ///
////////////////////////////////////////////////////////////////////////////////////////////////////	
	
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
	 * or role group in a compartment. If there is none, it returns <code>null</code>.
	 */
	private def Cardinality getOccurrenceConstraintCardinality(String compType, String roleTypeOrGroup) {
		
		if (rgNames.containsKey(roleTypeOrGroup))
			(if (occurrenceConstraintsForRoleGroups.containsKey(compType))
				return occurrenceConstraintsForRoleGroups
					.filter[ ct, rg | ct.equals(compType) ]
					.get(compType)
					.filter[ entry | entry.value.equals(rgNames.get(roleTypeOrGroup)) ]
					.map[ entry | entry.key ]
					.head)
		else
			(if (occurrenceConstraintsForRoleTypes.containsKey(compType))
				return occurrenceConstraintsForRoleTypes
					.filter[ key, value | key.equals(compType) ]
					.get(compType)
					.filter[ entry | entry.value.equals(roleTypeOrGroup) ]
					.map[ entry | entry.key ]
					.head)
	}
	
	
		
	
	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods for debugging / play ground															 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

	private def void debug(){
		println(modelInfo)
	}

	/**
	 * This methods prints some information about the CROM which can be used for example as debug output.
	 */
	private def String modelInfo() '''
		
		CROM:
		=====
		
		===Natural types:===
		«if (naturalTypes.empty) "none"
			else naturalTypes.join("",",\n","\n", [ nt | nt ])»
		
		===Compartment types:===
		«if (compartmentTypes.empty) "none"
			else compartmentTypes.join("",",\n","\n", [ ct | ct ])»
		
		===Role Types:===
		«if (roleTypes.empty) "none"
			else roleTypes.join("",",\n","\n", [ rt | rt ])»
		
		===Relationship types:===
		«if (relationshipTypes.empty) "none"
			else relationshipTypes.join("",",\n","\n", [ rst | rst ])»
		
		===Natural type inheritance:===
		«if (crom.ntinh.empty) "none"
			else crom.ntinh.join("", ",\n", "\n", [elem | elem.key + " -> " + elem.value])»
		
		===Compartment type inheritance:===
		«if (crom.ctinh.empty) "none"
			else crom.ctinh.join("", ",\n", "\n", [elem | elem.key + " -> " + elem.value])»
		
		===Fills relation:===
		«if (crom.fills.empty) "none"
			else crom.fills.join("", "\n", "\n", [ elem | elem.key.key + " -> " + elem.value + " in " + elem.key.value])»
		
		===Domain and range of relationship types:===
		«crom.rel.entrySet.join("", "\n", "\n",
			[elem | elem.key.key + ": " + elem.value.key + " -> " + elem.value.value + " in " + elem.key.value ])»
		
		Constraint Model:
		=================
		
		===Cardinality constraints for relationship types:===
		«if (crom.card.empty) "none"
			else crom.card.entrySet.join("", "\n", "\n", 
				[elem | elem.key.key + " in " + elem.key.value + ": " + elem.value.key + " ----> " + elem.value.value ])»
		
		===Occurrence constraints for role types and role groups===
		«if (crom.rolec.empty) "none"
			else crom.rolec.entrySet.join("", "\n", "\n", 
				[ elem | elem.value.join("In " + elem.key + ":\n", "\n", "\n", [ x | x.key + " -> " + x.value])])»
		
		===Occurrence constraints, filtered for role groups (with their generated names)===
		«if (occurrenceConstraintsForRoleGroups.empty) "none"
			else occurrenceConstraintsForRoleGroups.entrySet.join("", "\n", "\n", 
				[ elem | elem.value.join("In " + elem.key + ":\n", "\n", "\n", [ x | x.key + " -> " + x.value.name])])»
		
		===Top level role groups===
		«topLevelRoleGroups.join("", "\n", "\n", [ rg | rg.name ])»
		
		===Names of role groups===
		«rgNames.entrySet.join("\n", [ entry | entry.key + " --> " + entry.value])»
		
		===Intra relationship type constraints:===
		«if (crom.intra.empty) "none"
			else crom.intra»
		
		===Inter relationship type constraints:===
		«if (crom.inter.empty) "none"
			else crom.inter»
	'''


	
	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods for printing the ontology															 ///
////////////////////////////////////////////////////////////////////////////////////////////////////	
	
	
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
		«section("Used OWL datatypes")»
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
		«section("General axioms and declarations, independent of the CROModel")»
		Class: owl:Thing
			SubClassOf:
				«makeIRI("CompartmentTypes")»
			DisjointUnionOf:
				Annotations: rdfs:label "objectGlobal"
				«makeIRI("NaturalTypes")»,
				«makeIRI("RoleTypes")»,
				«makeIRI("RoleGroups")»,
				{«makeIRI("occurrenceCounter")»}
		
		Class: owl:Nothing
		
		Class: «makeIRI("CompartmentTypes")»
		
		Class: «makeIRI("NaturalTypes")»
		
		Class: «makeIRI("RoleTypes")»
		
		Class: «makeIRI("RoleGroups")»
		
		Individual: «makeIRI("occurrenceCounter")»
	'''
	
	/**
	 * This method creates the output for the compartment types that occur in the CROModel. It does
	 * not handle the inheritance relation between compartment types.
	 */
	private def String printCompartmentTypes() '''
		«section("The declaration of all compartment types that occur in the model")»
		
		Class: «makeIRI("CompartmentTypes")»
			SubClassOf:
				Annotations: rdfs:label "objectGlobal"
				owl:Nothing
		«compartmentTypes.join("    DisjointUnionOf:\n        ", ",\n        ", "\n", [ makeIRI ])»
		
		«compartmentTypes.join("\n\n", [ compType | "Class: " + makeIRI(compType) ])»
	'''

	/**
	 * This method creates the output for the natural types that occur in the CROModel. It also
	 * handles the inheritance relation between natural types.
	 */
	private def String printNaturalTypes() '''
		«section("The declaration of all natural types that occur in the model")»
		
		Class: «makeIRI("NaturalTypes")»
			SubClassOf:
				owl:Nothing
		
		«naturalTypes.join("", "\n\n", "\n", [ naturalType |  '''
			Class: «makeIRI(naturalType)»
				Annotations: «rigidity()» true'''])»
	'''

	/**
	 * This method handles all inheritance relations between natural types.
	 */
	private def String printNaturalTypeInheritance() '''
		«section("Natural type inheritance")»
		Class: «makeIRI("NaturalTypes")»
			«getTopLevelNaturalTypes().join(AnnotatedDisJointUnionOf(), ", ", "\n", [makeIRI] )»
			
		«naturalTypes.join("", "\n", "\n", [ naturalType |  '''
			Class: «makeIRI(naturalType)»
				«crom.ntinh.filter[entry | entry.key.equals(naturalType) ]
					.join("", "\n", "\n", [ entry | AnnotatedSubClassOf(entry.value)])»
			
			«getNaturalSubTypes(naturalType)
				.join(AnnotatedDisjointClasses, ", ", "\n\n", [makeIRI])»'''])»
	'''

	/**
	 * This method creates the output for the role types that occur in the CROModel.
	 */
	private def String printRoleTypes() '''
	«section("The declaration of all role types that occur in the model")»	
	Class: «makeIRI("RoleTypes")»
		«roleTypes.join(AnnotatedDisJointUnionOf(), ",\n    ", "\n", [makeIRI] )»
	
	Class: «makeIRI("RoleTypes")»
				SubClassOf:
					Annotations: rdfs:label "objectGlobal"
					inverse («makeIRI("plays")») exactly 1 owl:Thing
	
	«roleTypes.join("\n\n\n", [ roleType | '''
		«subsection(roleType)»
		Class: «makeIRI(roleType)»
			Annotations: «rigidity()» false
		
		Class: «makeIRI("Plays" + roleType)»
			EquivalentTo:
				Annotations: rdfs:label "objectGlobal"
				«makeIRI("plays")» some «makeIRI(roleType)»
				
		Class: «makeIRI("NaturalTypes")»
			SubClassOf:
				Annotations: rdfs:label "objectGlobal"
				«makeIRI("plays")» max 1 «makeIRI(roleType)»''' ])»
	'''
	
	/**
	 * This method handles the fills-relation as domain range constraints dependent on the
	 * contexts.
	 */
	private def String printFills() '''
		«section("fills relation")»
		«roleTypes.join("\n", [ roleType | '''
		«subsection(roleType)»
		
		Class: «makeIRI("Plays" + roleType)»
			SubClassOf:
				Annotations: rdfs:isDefinedBy «makeIRI("Fills" + roleType + "IsBottom")»
				owl:Nothing
			«compartmentTypes.filter[ compType | !getFillerTypes(roleType,compType).empty ]
				.join("", "\n", "\n", [ compType | '''
				SubClassOf:
					Annotations: rdfs:isDefinedBy «makeIRI("Fills" + roleType + "In" + compType)»
					«getFillerTypes(roleType, compType).join(" or ", [ nt | makeIRI(nt) ])»'''])»
		
		«compartmentTypes.filter[ compType | !getFillerTypes(roleType,compType).empty ]
			.join("", "\n", "\n\n", [ compType | "Class: " + makeIRI("Fills" + roleType + "In" + compType)])»
		Class: «makeIRI("Fills" + roleType + "IsBottom")»
		
		«compartmentTypes.filter[ compType | !getFillerTypes(roleType,compType).empty ]
			.join("", "\n\n", "\n", [ compType | '''
				Class: «makeIRI(compType)»
					SubClassOf: «makeIRI("Fills" + roleType + "In" + compType)»'''])»
		
		«compartmentTypes.filter[ compType | getFillerTypes(roleType,compType).empty ]
			.join("", "\n\n", "\n", [ compType | '''
				Class: «makeIRI(compType)»
					SubClassOf: «makeIRI("Fills" + roleType + "IsBottom")»'''])»
	 	''' ])»
	 '''

	/**
	 * This method introduces the plays relation.
	 */
	private def String printPlays() '''
	«section("The declaration of the plays relation as OWL object property")»
	
	ObjectProperty: owl:bottomObjectProperty
	
	ObjectProperty: «makeIRI("plays")»
		Domain:
			Annotations: rdfs:label "objectGlobal"
			«makeIRI("NaturalTypes")»
		Range:
			Annotations: rdfs:label "objectGlobal"
			«makeIRI("RoleTypes")» or «makeIRI("RoleGroups")»
		SubPropertyOf:
			owl:bottomObjectProperty
			
	'''

	/**
	 * This method prints all relationshipTypes and its constraints.
	 */
	private def String printRelationshipTypes() '''
	«section("The declaration of all relationship types that occur in the model")»
	
	«relationshipTypes.join("\n\n", [ relType | '''
		«subsection(relType)»
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
						«makeIRI(crom.rel.get(relType -> compType).value)»''' ])»
		
		Class: «makeIRI(relType + "IsBottom")»
		«compartmentTypes.filter[ compType | crom.rel.containsKey(relType -> compType)]
			.join("", "\n", "\n", [compType | '''
				Class: «makeIRI(relType + "DomainIn" + compType)»
				Class: «makeIRI(relType + "RangeIn" + compType)»''' ])»
		
		«compartmentTypes.filter[ compType | crom.rel.containsKey(relType -> compType)]
			.join("", "\n\n", "\n\n", [compType | '''
				Class: «makeIRI(compType)»
					SubClassOf: «makeIRI(relType + "DomainIn" + compType)»
					SubClassOf: «makeIRI(relType + "RangeIn" + compType)»''' ])»
		«compartmentTypes.filter[ compType | !crom.rel.containsKey(relType -> compType)]
			.join("", "\n\n", "\n", [compType | '''
				Class: «makeIRI(compType)»
					SubClassOf: «makeIRI(relType + "IsBottom")»''' ])»''' ])»
	'''

	/**
	 * This method creates the axioms for the cardinal constraints of the relationship types.
	 */
	private def String printCardinalConstraints() '''
		«section("Cardinal constraints of relationship types")»
		
		«relationshipTypes.join("\n", [ relType | '''
			«subsection(relType)»
			«compartmentTypes.join("", [ compType | '''
				«roleTypes.join("", [ roleType | getAllCardinalAxioms(relType, compType, roleType) ])»
			'''])»
		'''])»
	'''

	/**
	 * This method prints all the occurrence constraints.
	 */
	private def String printOccurrenceConstraints() '''
		«section("The declaration of the counter nominal and all the occurrence constraints.")»
		
		ObjectProperty: «makeIRI("count")»
		
		Class: «makeIRI("RoleTypes")»
			SubClassOf:
				Annotations: rdfs:label "objectGlobal"
				inverse («makeIRI("count")») exactly 1 {«makeIRI("occurrenceCounter")»}
		
		
		Class: «makeIRI("RoleGroups")»
			SubClassOf:
				Annotations: rdfs:label "objectGlobal"
				inverse («makeIRI("count")») exactly 1 {«makeIRI("occurrenceCounter")»}
				
		Individual: «makeIRI("occurrenceCounter")»
		
		«compartmentTypes.filter[ compType | occurrenceConstraintsForRoleTypes.containsKey(compType) ]
			.join("", [ compType | roleTypes
				.filter[ roleType | occurrenceConstraintsForRoleTypes.get(compType)
					.map[ entry | entry.value].contains(roleType) ]
				.join("\n", [ roleType |  
					getCardAxiom(
						subsection(roleType + ", " + compType)
							+ "Individual: " + makeIRI("occurrenceCounter") + "\n"
							+ "    Types:\n",
						roleType + "OccurrenceConstraintIn" + compType,
						"",
						getOccurrenceConstraintCardinality(compType, roleType),
						makeIRI("count"),
						makeIRI(roleType),
						compType)
				])])»
		
		«compartmentTypes.filter[ compType | occurrenceConstraintsForRoleGroups.containsKey(compType) ]
			.join("", [ compType | roleGroups
				.filter[ roleGroup | occurrenceConstraintsForRoleGroups.get(compType)
					.map[ entry | entry.value].contains(roleGroup) ]
				.join("\n", [ roleGroup | 
					getCardAxiom(
						subsection(roleGroup.name + ", " + compType)
							+ "Individual: " + makeIRI("occurrenceCounter") + "\n"
							+ "    Types:\n",
						roleGroup.name + "OccurrenceConstraintIn" + compType,
						"",
						getOccurrenceConstraintCardinality(compType, roleGroup.name),
						makeIRI("count"),
						makeIRI(roleGroup.name),
						compType)
				])])»
	'''

	/**
	 * This method prints all axioms related to role groups.
	 */
	private def String printRoleGroups() '''
		«section("The declaration of all role groups that appear in the CROM.")»
		Class: «makeIRI("RoleGroups")»
			«roleGroups.join(AnnotatedDisJointUnionOf(), ",\n	", "\n", [makeIRI] )»
			
		Class: «makeIRI("RoleGroups")»
			SubClassOf:
				Annotations: rdfs:label "objectGlobal"
				inverse («makeIRI("plays")») exactly 1 owl:Thing
		
		«roleGroups.join("\n\n\n", [roleGroup | '''
			«subsection(roleGroup.name)»
			Class: «makeIRI(roleGroup)»
				Annotations: «rigidity()» false
			
			Class: «makeIRI("Plays" + roleGroup.name)»
				EquivalentTo:
					Annotations: rdfs:label "objectGlobal"
					«makeIRI("plays")» some «makeIRI(roleGroup)»
			
			Class: «makeIRI("NaturalTypes")»
				SubClassOf:
					Annotations: rdfs:label "objectGlobal"
					«makeIRI("plays")» max 1 «makeIRI(roleGroup)»
			
			Class: «makeIRI(roleGroup.name + "Elements")»
				DisjointUnionOf:
					Annotations: rdfs:label "objectGlobal"
					«roleGroup.elementNames.join(", ", [makeIRI])»
			
			Class: «makeIRI(roleGroup.name + "Atoms")»
				EquivalentTo:
					Annotations: rdfs:label "objectGlobal"
					«roleGroup.roleGroupAtoms.join(" or ", [makeIRI])»
			
			«compartmentTypes.filter[ compType | compType.contains(roleGroup) ]
				.join("\n\n", [ compType | getCardAxiom(
					"Class: " + makeIRI("Plays" + roleGroup.name) + "\n" + "    EquivalentTo:\n",
					"Satisfy" + roleGroup.name + "In" + compType,
					makeIRI("NaturalTypes") + " and ",
					roleGroup.card,
					makeIRI("plays"),
					makeIRI(roleGroup.name + "Elements"),
					compType) ])»
			
			«if (topLevelRoleGroups.contains(roleGroup)) '''
				Class: «makeIRI("MustPlay" + roleGroup.name)»
					EquivalentTo:
						Annotations: rdfs:label "objectGlobal"
						«roleGroup.roleGroupAtoms.join(" or ", [ roleType | makeIRI("Plays" + roleType)])»
					SubClassOf:
						Annotations: rdfs:label "objectGlobal"
						«makeIRI("Plays" + roleGroup.name)»
			''' else ""»
	 	'''])»
	 '''

	/**
	 * This method creates the closing comment for the generated OWL file.
	 */
	private def String printEndOfFile() '''
		«section("This is the end of the automatically generated OWL File.",
			"If you want to add any axioms manually you should do it below here.")»
	'''

	/** 
	 * This method actually prints the whole ontology document in Manchester OWL syntax.
	 */
	private def String printOntology(String modelname) '''
		«debug»
		«printHeader(modelname)»
		«printAnnotationsAndDatatypes»
		«printOWLThingAndOthers»
		«printCompartmentTypes»
		«printNaturalTypes»
		«printRoleTypes»
		«printPlays»
		«printNaturalTypeInheritance»
		«printFills»
		«printRelationshipTypes»
		«printRoleGroups»
		«printCardinalConstraints»
		«printOccurrenceConstraints»
		«printEndOfFile»
	'''	
}


/**
 * This exception is thrown when the context ontology should be generated, but the CROModel
 * contains unsupported elements.
 */
class CROMOntologyGeneratorException extends RuntimeException {
	new(String message) {
		super("\n" + message)
	}
}