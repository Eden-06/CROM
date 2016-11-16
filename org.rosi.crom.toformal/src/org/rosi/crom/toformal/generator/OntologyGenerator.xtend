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
	var int numberOfRoleGroups = 0
	val HashMap<String, RoleGroup> rgNames = new HashMap()
	val int headingWidth = 100
	
	var CROModel crom
	var Set<String> compartmentTypes
	var Set<String> naturalTypes
	var Set<String> roleTypes
	var Set<String> relationshipTypes

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
		retrieveCompartmentTypes
		retrieveNaturalTypes
		retrieveRoleTypes
		retrieveRelationshipTypes
		retrieveRoleGroupNames
		checkCompartmentInheritance
		checkNaturalTypes
		checkRelCardConnection
//		checkRoleTypes
		return printOntology(modelname)
	}
	
	
	

////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods used in the setup																	 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

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
	private def Set<RoleGroup> getRoleGroupElements(RoleGroup roleGroup) {
		return (roleGroup.elements
			.filter[ elem | elem.isRoleGroup ]
			.map[ elem | elem as RoleGroup ]
			.map[ elem | elem.roleGroupElements + #{elem} ]
			.flatten + #{roleGroup}).toSet
	}

	/**
	 * This method retrieves all role groups that appear in the CROM.
	 */
	private def Set<RoleGroup> getAllRoleGroups() {
		topLevelRoleGroups.map[ roleGroup | roleGroup.roleGroupElements ].flatten.toSet
	}
	
	/**
	 * This method generates a fresh generic name for a role group and increases the counter of
	 * created role group names by 1.
	 */
	private def String createNewRoleGroupName() {
		numberOfRoleGroups++
		return "GeneratedRoleGroup-" + numberOfRoleGroups
	}
	

	


	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods for cardinal constraints of relationship types										 ///
////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Given a cardinality constraint and the according relationship type, and compartment type,
	 * this method creates the appropriate sub class axiom.
	 */
	private def String getCardinalAxiomIfConstraining(Cardinality card, String relType, 
						String compType, String roleType, String domOrRan) {
		val relTypeIRI = 
			if (domOrRan=="Range") "inverse (" + makeIRI(relType) + ")"
			else makeIRI(relType)
		return (if (card.lower > 0 || card.upper != -1)
				  "Class: " + makeIRI(roleType) + "\n"
				+ "    SubClassOf:\n"
				+ "        Annotations: rdfs:isDefinedBy " + makeIRI(relType + "CardinalConstraintOf" + domOrRan + "In" + compType) + "\n"
				+ "        " 
				else "")
			+ (if (card.lower > 0)
				relTypeIRI + " min " + card.lower + " owl:Thing"
				else "")
			+ (if (card.lower > 0 && card.upper != -1)
				", "
				else "")
			+ (if (card.upper != -1)
				relTypeIRI + " max " + card.upper + " owl:Thing"
				else "")
			+ (if (card.lower > 0 || card.upper != -1)
				"\n\n"
				+ "Class: " + makeIRI(relType + "CardinalConstraintOf" + domOrRan + "In" + compType) + "\n"
				+ "\n"
				+ "Class: " + makeIRI(compType) + "\n"
				+ "    SubClassOf:\n"
				+ "        " + makeIRI(relType + "CardinalConstraintOf" + domOrRan + "In" + compType) + "\n"
				+ "\n"
				else "")
	}
	
	/**
	 * For a given relationship type, compartment type and role type this method returns the
	 * necessary axioms to ensure the cardinal constraints. If there are none for the given
	 * parameters, it return an empty string.  
	 */
	private def String getAllCardinalAxioms(String relType, String compType, String roleType) {
		return 
			if (crom.card.containsKey(relType -> compType)) {
				val carddom = crom.card.get(relType -> compType).value
				val cardran = crom.card.get(relType -> compType).key
				return 
					(if (crom.rel.get(relType -> compType).key.equals(roleType)) // roleType ----relType----> ...
						getCardinalAxiomIfConstraining(carddom, relType, compType, roleType, "Domain")
					else (if (crom.rel.get(relType -> compType).value.equals(roleType))
						getCardinalAxiomIfConstraining(cardran, relType, compType, roleType, "Range")
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
	 * This method retrieves the cardinality of the occurrence constraint for a certain role group
	 * in a compartment. If there is none, it returns <code>null</code>.
	 */
	private def Cardinality getOccurrenceConstraint(String compType, RoleGroup roleGroup) {
		return null
	}
	
	/**
	 * Given a role type and compartment type, this method retrieves the correct occurrence
	 * constraint and creates the appropriate class assertion axiom if the constraint is actually
	 * constraining.
	 */
	private def String getTypeAssertionIfConstraining(String roleType, String compType) {
		val card = getOccurrenceConstraint(compType, roleType)
		if (card == null) return ""
		return (if (card.lower > 0 || card.upper != -1)
				  subsection(roleType + ", " + compType)
				+ "Individual: " + makeIRI("cardinalityCounter") + "\n"
				+ "    Types:\n"
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
				"\n\n"
				+ "Class: " + makeIRI(roleType + "OccurrenceConstraintIn" + compType) + "\n"
				+ "\n"
				+ "Class: " + makeIRI(compType) + "\n"
				+ "    " + "SubClassOf: " + makeIRI(roleType + "OccurrenceConstraintIn" + compType) + "\n"
				+ "\n"
				else "")
	}
	
	/**
	 * Given a role group and compartment type, this method retrieves the correct occurrence
	 * constraint and creates the appropriate class assertion axiom if the constraint is actually
	 * constraining.
	 */
	private def String getTypeAssertionIfConstraining(RoleGroup roleGroup, String compType) {
		return ""
	}
	
		
	
	
////////////////////////////////////////////////////////////////////////////////////////////////////
/// Methods for debugging / play ground															 ///
////////////////////////////////////////////////////////////////////////////////////////////////////

	private def void debug(){
		
		val rg = topLevelRoleGroups.get(0)
		
		println(topLevelRoleGroups)
		println(allRoleGroups)
		println(rg)
		println(rg.roleGroupElements)
		retrieveRoleGroupNames
		println(rgNames)
		//tmp.forEach[rg | println(rg)]
	}



	
	private def newFills(CROModel model) {
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
		         «makeIRI("NaturalTypes")», «makeIRI("RoleTypes")», {«makeIRI("cardinalityCounter")»}
		
		Class: owl:Nothing
		
		Class: «makeIRI("CompartmentTypes")»
		
		Class: «makeIRI("NaturalTypes")»
		
		Class: «makeIRI("RoleTypes")»
		
		Individual: «makeIRI("cardinalityCounter")»
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
		«compartmentTypes.join("    DisjointUnionOf: ", ", ", "\n", [ makeIRI ])»
		
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
		    			.join(AnnotatedDisjointClasses, ", ", "\n", [makeIRI])»'''])»
	'''

	/**
	 * This method creates the output for the role types that occur in the CROModel.
	 */
	private def String printRoleTypes() '''
	«section("The declaration of all role types that occur in the model")»	
	Class: «makeIRI("RoleTypes")»
		«roleTypes.join(AnnotatedDisJointUnionOf(), ",\n    ", "\n", [makeIRI] )»
	
	
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
				.join("", "\n", "\n", [ compType | "Class: " + makeIRI("Fills" + roleType + "In" + compType)])»
			Class: «makeIRI("Fills" + roleType + "IsBottom")»
			
			«compartmentTypes.filter[ compType | !getFillerTypes(roleType,compType).empty ]
				.join("", "\n", "\n", [ compType | '''
					Class: «makeIRI(compType)»
						SubClassOf: «makeIRI("Fills" + roleType + "In" + compType)»'''])»
			«compartmentTypes.filter[ compType | getFillerTypes(roleType,compType).empty ]
				.join("", "\n", "\n", [ compType | '''
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
	        «makeIRI("RoleTypes")»
	    SubPropertyOf:
	        owl:bottomObjectProperty
	        
	Class: «makeIRI("RoleTypes")»
		    SubClassOf:
		        Annotations: rdfs:label "objectGlobal"
		        inverse («makeIRI("plays")») exactly 1 owl:Thing
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
		    EquivalentTo:
				Annotations: rdfs:label "objectGlobal"
				inverse («makeIRI("count")») exactly 1 {«makeIRI("cardinalityCounter")»}
				
		«compartmentTypes.filter[ compType | occurrenceConstraintsForRoleTypes.containsKey(compType) ]
			.join("", [compType | roleTypes
				.filter[ roleType | occurrenceConstraintsForRoleTypes.get(compType)
					.map[ entry | entry.value].contains(roleType) ]
				.join("\n", [ roleType | getTypeAssertionIfConstraining(roleType, compType)])])»
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
«««		«debug»
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
		«printCardinalConstraints»
		«printOccurrenceConstraints»
		«printEndOfFile»

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
}
class CROMOntologyGeneratorException extends RuntimeException {
	new(String message) {
		super("\n" + message)
	}
}