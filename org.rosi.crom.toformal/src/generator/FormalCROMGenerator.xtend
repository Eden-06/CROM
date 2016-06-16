package generator

import java.util.List
import builder.RoleGroup
import builder.CROModel
import java.util.ArrayList
import java.util.HashMap

class FormalCROMGenerator extends AbstractCROMGenerator {

	new() {
		super("py")
	}

	private def mklist(List<String> list) '''[«list.map[v|"\"" + v + "\""].join(",")»]'''

	private def mkpair(Pair<String, String> pair) '''("«pair.key»","«pair.value»")'''

	public def fills(CROModel model) {
		val r = new ArrayList<Pair<String, String>>
		for (e : model.fills)
			r.add(e.key.key -> e.value)
		return r
	}

	public def parts(CROModel model) {
		val parts = new HashMap<String, List<String>>
		for (e : model.fills) {
			if (! parts.containsKey(e.key.value))
				parts.put(e.key.value, new ArrayList<String>)
			parts.get(e.key.value).add(e.value)
		}
		return parts
	}


	def dispatch CharSequence mkrolegroup(RoleGroup rg) '''RoleGroup([«rg.elements.map[e|mkrolegroup(e)].join(",")»],«rg.
		card.lower»,«if (rg.card.upper == -1) "inf"	else rg.card.upper»)'''

	def dispatch CharSequence mkrolegroup(String o) '''"«o.toString»"'''

	/**
	 * Currently not used
	 */
	def getdt(CROModel builder) { return mklist(builder.dt) }

	def getnt(CROModel builder) { return mklist(builder.nt) }

	def getrt(CROModel builder) { return mklist(builder.rt) }

	def getct(CROModel builder) { return mklist(builder.ct) }

	def getrst(CROModel builder) { return mklist(builder.rst) }

	def getfills(CROModel builder) '''
		[«builder.fills().map[v|mkpair(v)].join(",")»]
	'''

	def getparts(CROModel builder) '''
		{«builder.parts().entrySet.map[e|"\"" + e.key + "\": " + mklist(e.value)].join(",")»}
	'''

	def getrel(CROModel builder) '''
		{«builder.rel.entrySet.map[e|"\"" + e.key.key + "\": " + mkpair(e.value)].join(",")»}
	'''

	def getrolec(CROModel builder) '''
		{«builder.rolec.entrySet.map[v|
			"\"" + v.key + "\": [" + v.value.map[e|"(" + e.key + "," + mkrolegroup(e.value) + ")"].join(",") + "]"].join(",")»}
	'''

	def getcard(CROModel builder) '''
		{«builder.card.entrySet.map[v|"\"" + v.key.key + "\": (" + v.value.key + "," + v.value.value + ")"].join(",")»}
	'''

	def getintra(CROModel builder) '''
		[«builder.intra.map[v|"(\"" + v.key.key + "\"," + v.value + ")"].join(",")»]
	'''

	def getinter(CROModel builder) '''
		{«builder.inter.entrySet.map[v|"(\"" + v.key.key.key + "\","+v.value+",\""+ v.key.value + "\")"].join(",")»}
	'''

	/**
	 * Currently not used
	 */
	def getdtinh(CROModel builder) '''
		[«builder.dtinh.map[v|mkpair(v)].join(",")»]
	'''

	/**
	 * Currently not used
	 */
	def getntinh(CROModel builder) '''
		[«builder.ntinh.map[v|mkpair(v)].join(",")»]
	'''

	/**
	 * Currently not used
	 */
	def getctinh(CROModel builder) '''
		[«builder.ctinh.map[v|mkpair(v)].join(",")»]
	'''

	override String generate(CROModel builder) '''
#!/usr/bin/env python
# -*- coding: UTF-8 -*-
from crom import *

print "=== Model ==="
NT=«builder.getnt»
RT=«builder.getrt»
CT=«builder.getct»
RST=«builder.getrst»
fills=«builder.getfills»
parts=«builder.getparts»
rel=«builder.getrel»

model=CROM(NT,RT,CT,RST,fills,parts,rel)
          
print model
if model.wellformed():
	print " The model is a wellformed CROM"
else:
	print " The model is not wellformed"
print

print "=== Constraint Model ==="

rolec=«builder.getrolec»
card=«builder.getcard»
intra=«builder.getintra»
# not supported yet
implication="-|>"
exclusion=">-<"
inter=«builder.getinter»

cm=ConstraintModel(rolec,card,intra)

print cm
if cm.compliant(model):
	print "The constraint model is compliant to the CROM bank"
else:
	print "The constraint model is not compliant to the CROM bank"

print
 	'''
}
