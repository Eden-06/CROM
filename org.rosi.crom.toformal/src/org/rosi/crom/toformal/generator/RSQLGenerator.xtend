package org.rosi.crom.toformal.generator

import java.util.List
import org.rosi.crom.toformal.builder.RoleGroup
import org.rosi.crom.toformal.builder.CROModel
import java.util.ArrayList
import java.util.HashMap
import crom_l1_composed.Model
import org.rosi.crom.toformal.builder.CROMVisitor
import org.eclipse.core.runtime.IPath

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

//	"...[]" -> "ARRAY"

	def mkColumnDef(Pair<String, String> pair){
		if (! mapping.containsKey(pair.value))
			return "#"+pair.key+": "+pair.value
		return pair.key+": "+mapping.get(pair.value)	
	}
	
	/**
	 * 	CREATE ('NATURALTYPE' | 'NT') ntToBeCreated '(' COLUMN_DEFINITION (',' COLUMN_DEFINITION)* ')'
	 */
    def mkCreateNT(CROModel model, String nt)'''
    	CREATE NATURALTYPE «nt» (
    		«IF (model.fields.containsKey(nt))»
    			«FOR e : (model.fields.get(nt)) SEPARATOR ','»
    			«mkColumnDef(e)»
    			«ENDFOR»
    		«ENDIF»
    	);
    '''
	
	private def String generate(CROModel builder) '''
		/* Generated RSQL code */
		
		# Natural Types
		«FOR nt:builder.nt»
		«mkCreateNT(builder,nt)»
		«ENDFOR»
		# Compartment Types
		
		# Role Types
		
		# Relationship Types
 	'''

}
