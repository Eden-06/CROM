package org.rosi.crom.toformal.generator

import org.rosi.crom.toformal.builder.CROModel
import java.util.HashMap
import crom_l1_composed.Model
import org.rosi.crom.toformal.builder.CROMVisitor
import org.eclipse.core.runtime.IPath
import java.util.Set

class RSQLGenerator extends AbstractCROMGenerator {

	new() {
		super("rsql")
	}
	
	override generate(IPath path, Model model) {
			val crom = new CROModel
			val visitor = new CROMVisitor
			visitor.visit(crom, model)
			return generate(crom)
	}
	/**
	 * Standard mappings from Java to H2 datatypes.
	 */
	private val mapping=newHashMap( 
		"String" -> "VARCHAR",
		"bool" -> "BOOLEAN", "Boolean" -> "BOOLEAN",
		"int" -> "INT", "Integer" -> "INT",
		"Double" -> "DOUBLE", "double" -> "DOUBLE",
		"float" -> "FLOAT", "Float" -> "FLOAT",
		"Time" -> "TIME",
		"Date" -> "DATE",
		"DateTime" -> "DATETIME")
	/*
	 * "...[]" -> "ARRAY"
	 */	
    private val ARRAY="ARRAY"
    
    private val keywords=#{"cross", "current_date", "current_time", "current_timestamp", "distinct", "except", "exists", "false", "fetch", "for", "from", "full", "group", "having", "inner", "intersect", "is", "join", "like", "limit", "minus", "natural", "not", "null", "offset", "on", "order", "primary", "rownum", "select", "sysdate", "systime", "systimestamp", "today", "true", "union", "unique", "where"}

	def mkColumnDef(Pair<String, String> pair){
		if (! mapping.containsKey(pair.value))
			return "//"+pair.key+" "+pair.value
		if (pair.value.endsWith("[]"))
			return pair.key+" "+ARRAY
		return pair.key+" "+mapping.get(pair.value)	
	}
	
	/**
	 * 	Data Types are generated like NATURAL TYPES, however require at least one primary key column.
	 *  CREATE ('NATURALTYPE' | 'NT') ntToBeCreated '(' COLUMN_DEFINITION (',' COLUMN_DEFINITION)* ')'
	 */
	def mkCreateDT(CROModel model, String dt)'''
		CREATE NATURALTYPE «dt» ( /* Declaration of a Data Type requires primary keys */
    		«IF (model.fields.containsKey(dt))»
    			«FOR e : (model.fields.get(dt)) SEPARATOR ','»
    			«mkColumnDef(e)»
    			«ENDFOR»
    		«ENDIF»
		);
	'''
	
	/**
	 * 	CREATE ('NATURALTYPE' | 'NT') ntToBeCreated '(' 'oid BIGINT IDENTITY' ',' COLUMN_DEFINITION (',' COLUMN_DEFINITION)* ')'
	 */
    def mkCreateNT(CROModel model, String nt)'''
    	CREATE NATURALTYPE «nt» (
    		oid BIGINT IDENTITY«IF (! model.fields.empty)»,«ENDIF»
    		«IF (model.fields.containsKey(nt))»
    			«FOR e : (model.fields.get(nt)) SEPARATOR ','»
    			«mkColumnDef(e)»
    			«ENDFOR»
    		«ENDIF»
    	);
    '''
    
    /**
     * 	('COMPARTMENTTYPE' | 'CT') ctToBeCreated '(' COLUMN_DEFINITION (',' COLUMN_DEFINITION)* ')'
     */
    def mkCreateCT(CROModel model, String ct)'''
        CREATE COMPARTMENTTYPE «ct» (
        	cid BIGINT IDENTITY«IF (! model.fields.empty)», /* should be refined */«ENDIF»
        	«IF (model.fields.containsKey(ct))»
        		«FOR e : (model.fields.get(ct)) SEPARATOR ','»
        		«mkColumnDef(e)»
        		«ENDFOR»
    		«ENDIF»
        );
    '''
	
	/**
	 * 	CREATE ('ROLETYPE' | 'RT') rtToBeCreated '(' COLUMN_DEFINITION (',' COLUMN_DEFINITION)* ')' 
	 * 'PLAYED BY' '(' playedByNts (',' playedByNts)* ')'
     * 'PART OF' ctName 
     * 
     * WARNING: RSQL does not support the specification of a role type belonging to multiple compartment types. 
	 */
	def mkCreateRT(CROModel model, String ct, String rt)'''
		CREATE ROLETYPE «rt» (
			«IF model.fields.containsKey(ct+'.'+rt)»
    			«FOR e:model.fields.get(ct+'.'+rt) SEPARATOR ','»
    			«mkColumnDef(e)»
    			«ENDFOR»
    		«ENDIF»
			)
			«IF ! model.playedby(rt,ct).empty»PLAYED BY ( «model.playedby(rt,ct).join(", ")» )«ENDIF»
			PART OF «ct»;
	'''
	
	/**
	 * CREATE_RST_STATEMENT::= ('RELATIONSHIPTYPE' | 'RST') rstToBeCreated 
	 * ('(' COLUMN_DEFINITION ( ',' COLUMN_DEFINITION )* ')' )? 
	 * 'CONSISTING OF' ( RELATION_PARTICIPANT_EXPRESSION 'AND' RELATION_PARTICIPANT_EXPRESSION )
	 * 
	 * RELATION_PARTICIPANT_EXPRESSION ::= '(' rtName 'BEING' ( '0' | '1' ) '..' ( '1' | '*' ) ')'
	 */
	def mkCreateRST(CROModel model,String ct,String rst)'''
		CREATE RELATIONSHIPTYPE «rst»
			CONSISTING OF «model.rel.get(rst->ct).key» AND «model.rel.get(rst->ct).value»; //TODO: fix missing cardinalities
	'''
	
	/**
	 * Collects all natural and compartment types filled by the given role type in a given compartment type.
	 */
	def playedby(CROModel model, String rt, String ct) {
		return model.fills.filter[e|e.key.value==ct && e.value==rt].map[e|e.key.key]
	}

	/**
	 * Check whether there is a role type rt that is part of multiple compartment types
	 */
	def hasAmbiguousRoleTypes(CROModel model) {
		return model.partofs.values.exists[v|v.size>1]
	}
	
	/**
	 * Check whether there is a relationship type rst that is part of multiple compartment types
	 */
	def hasAmbiguousRelationshipTypes(CROModel model) {
		return model.rstofs.values.exists[v|v.size>1]
	}
	
	/**
	 * Returns all identifiers in your model that are SQL keywords.
	 */
	def getIdentifiersAsKeywords(CROModel model) {
		val identifiers=newArrayList()
		identifiers.addAll(model.dt+model.nt+model.ct+model.rt+model.rst)
		for (l:model.fields.values){
			identifiers.addAll(l.map[f|f.key])
		}
		val r=identifiers.map[e|e.toLowerCase]
		r.retainAll(keywords)
		return 	r	
	}
	
	/**
	 * Return map of role types to the set of compartment types they are contained in
	 */
	def partofs(CROModel model){
		val HashMap<String,Set<String>> partofs = newHashMap
		for (e:model.fills){
			val ct = e.key.value
			val rt = e.value
			if (! partofs.containsKey(rt)) partofs.put(rt, newHashSet())
			partofs.get(rt).add(ct);
		}
		return partofs
	}
	
	/**
	 * Return map of relationship types to the set of compartment types they are contained in
	 */
	def rstofs(CROModel model){
		val HashMap<String,Set<String>> rstofs = newHashMap
		for (e:model.rel.keySet){
			val rst = e.key
			val ct = e.value
			if (! rstofs.containsKey(rst)) rstofs.put(rst, newHashSet())
			rstofs.get(rst).add(ct);
		}
		return rstofs
	}
	
	private def String generate(CROModel builder) '''
		/* Generated RSQL code */
		
		«IF ! (builder.ctinh + builder.ntinh + builder.dtinh).empty»
		/* WARNING: The model containes Inheritance that is not supported by RSQL */
		«ENDIF»
		«IF builder.hasAmbiguousRoleTypes()»
		/* WARNING: The model contains the following ambiguous role types that are not supported by RSQL:
			«FOR e : builder.partofs.filter[rt,v|v.size>1].entrySet»
			* «e.key» is part of «e.value.join(", ")»
			«ENDFOR»
		*/
		«ENDIF»
		«IF builder.hasAmbiguousRelationshipTypes()»
		/* WARNING: The model contains the following ambiguous relationship types that are not supported by RSQL:
			«FOR e : builder.rstofs.filter[rt,v|v.size>1].entrySet»
			* «e.key» is defined within «e.value.join(", ")»
			«ENDFOR»
		 */
		«ENDIF»
		«IF ! builder.getIdentifiersAsKeywords().empty»
		/* WARNING: The model contains the following SQL keywords as identifiers:
		 * « builder.getIdentifiersAsKeywords().join(", ")»
		 */
		«ENDIF»
		
		«IF !builder.dt.empty»
			/* Data Types */
			«FOR dt:builder.dt»
				«mkCreateDT(builder,dt)»
			«ENDFOR»
		«ENDIF»
		
		«IF !builder.nt.empty»
			/* Natural Types */
			«FOR nt:builder.nt»
				«mkCreateNT(builder,nt)»
			«ENDFOR»
		«ENDIF»
		
		«IF !builder.ct.empty»
			/* Compartment Types */
			«FOR ct:builder.ct»
				«mkCreateCT(builder,ct)»
			«ENDFOR»
		«ENDIF»
		
		«IF !builder.rt.empty»
			/* Role Types */
			«FOR e:builder.partofs.entrySet»
				«FOR ct:e.value»
				«mkCreateRT(builder,ct,e.key)»
				«ENDFOR»
			«ENDFOR»
		«ENDIF»
		
		«IF !builder.rst.empty»
			/* Relationship Types */
			«FOR e:builder.rstofs.entrySet»
				«FOR ct:e.value»
				«mkCreateRST(builder,ct,e.key)»
				«ENDFOR»
			«ENDFOR»
		«ENDIF»
 	'''
	
}
