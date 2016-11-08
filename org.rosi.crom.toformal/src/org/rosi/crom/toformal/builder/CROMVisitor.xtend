package org.rosi.crom.toformal.builder

import java.util.List
import crom_l1_composed.RoleType
import java.util.ArrayList
import crom_l1_composed.Model
import crom_l1_composed.NaturalType
import crom_l1_composed.CompartmentType
import crom_l1_composed.Relationship
import crom_l1_composed.Fulfillment
import crom_l1_composed.Part
import org.eclipse.emf.ecore.EObject
import crom_l1_composed.Irreflexive
import crom_l1_composed.Reflexive
import crom_l1_composed.IntraRelationshipConstraint
import crom_l1_composed.InterRelationshipConstraint
import crom_l1_composed.RoleConstraint
import crom_l1_composed.Acyclic
import crom_l1_composed.Cyclic
import crom_l1_composed.Total
import crom_l1_composed.RelationshipExclusion
import crom_l1_composed.RelationshipImplication
import crom_l1_composed.AbstractRoleRef
import crom_l1_composed.RoleImplication
import crom_l1_composed.RoleProhibition
import crom_l1_composed.RoleEquivalence
import crom_l1_composed.NaturalInheritance
import crom_l1_composed.DataInheritance
import crom_l1_composed.CompartmentInheritance
import crom_l1_composed.RoleInheritance
import crom_l1_composed.Group
import crom_l1_composed.DataType

class CROMVisitor {

	def dispatch List<String> atoms(crom_l1_composed.RoleGroup a) {
		val r = new ArrayList<String>()
		for (s : a.elements)
			r.addAll(s.atoms());
		return r
	}

	def dispatch List<String> atoms(AbstractRoleRef ref) {
		return ref.ref.atoms()
	}

	def dispatch List<String> atoms(RoleType a) {
		return #[a.name]
	}

	def dispatch Object visitRoleGroup(crom_l1_composed.RoleGroup rg) {
		val es = rg.elements.map[e|visitRoleGroup(e)]
		val l = rg.lower
		val u = rg.upper
		return new RoleGroup(l, u, es)
	}

	def dispatch Object visitRoleGroup(AbstractRoleRef ref) {
		val r = ref.ref
		if(r != null) return visitRoleGroup(r)
	}

	def dispatch Object visitRoleGroup(RoleType rt) {
		return rt.name
	}

	def dispatch void visit(CROModel builder, Model model) {
		for (e : model.elements)
			visit(builder, e)
		for (r : model.relations)
			visit(builder, r)
	}
	
	def dispatch void visit(CROModel builder, Group group) {
		for (e : group.elements)
			visit(builder, e)
		for (r : group.relations)
			visit(builder, r)
	}

	def dispatch void visit(CROModel builder, NaturalType nt) {
		builder.nt.add(nt.name)
		builder.fields.put(nt.name, nt.attributes.map[a|a.name -> a.type.name])
	}
	
	def dispatch void visit(CROModel builder, DataType dt) {
		builder.dt.add(dt.name)
		builder.fields.put(dt.name, dt.attributes.map[a|a.name -> a.type.name])
	}

	def dispatch void visit(CROModel builder, RoleType rt) {

		// builder.rt.add(rt.name)
		throw new UnsupportedOperationException("A RoleType must always be defined as part of a CompartmentType")
	}

	def dispatch void visit(CROModel builder, CompartmentType ct) {
		builder.ct.add(ct.name)
		for (p : ct.parts)
			visit(builder, p, ct.name)
		for (rst : ct.relationships)
			visit(builder, rst, ct.name)
		for (c : ct.constraints)
			visit(builder, c, ct.name)
		builder.fields.put(ct.name, ct.attributes.map[a| a.name->a.type.name ])
	}

	def dispatch void visit(CROModel builder, Relationship rst, String ct) {
		builder.rst.add(rst.name)
		builder.rel.put(rst.name -> ct, rst.first.holder.name -> rst.second.holder.name)
		builder.card.put(rst.name -> ct,
			new Cardinality(rst.first.lower, rst.first.upper) -> new Cardinality(rst.second.lower, rst.second.upper))
	}

	def dispatch void visit(CROModel builder, Fulfillment fills) {
		//CROM currently does not directly support compartment dependent fills.
		//Hence, it must be inferred from the model.
		val r = switch fills.filled {
			crom_l1_composed.RoleGroup: (fills.filled as crom_l1_composed.RoleGroup)
			RoleType: (fills.filled as RoleType)
			default: null
		}
		if (r != null && r.eContainer != null && r.eContainer.eContainer != null) {
			val ct = ((r.eContainer.eContainer) as CompartmentType).name
			val ot = fills.filler.name
			val rs = fills.filled.atoms()
			for (rt : rs)
				builder.fills.add(ot -> ct -> rt)
		}

	}

	def dispatch void visit(CROModel builder, Part part, String ct) {
		if (ct != part.whole.name)
			throw new IllegalStateException(
				"Given CompartmentType " + ct + " does not match container CompartmentType " + part.whole.name);
		val c = new Cardinality(part.lower, part.upper)
		val r = part.role.atoms()
		builder.rt.addAll(r)

		val rg = visitRoleGroup(part.role)
		if (! builder.rolec.containsKey(ct))
			builder.rolec.put(ct, new ArrayList<Pair<Cardinality, Object>>)
		builder.rolec.get(ct).add(c -> rg)
		
		val rts=collectRoles(part.role)
		for (rt:rts){
			builder.fields.put( ct+"."+rt.name, rt.attributes.map[a|a.name->a.type.name])
		}				
	}

	def dispatch List<RoleType> collectRoles(RoleType rt) {
		return #[rt]
	}
	
	def dispatch List<RoleType> collectRoles(AbstractRoleRef ref) {
		//Do nothing if you hit a role ref
		return new ArrayList<RoleType>
	}
	
	def dispatch List<RoleType> collectRoles(crom_l1_composed.RoleGroup rg) {
		val rts=new ArrayList<RoleType>
		for(r:rg.elements){
			rts.addAll(collectRoles(r))
		}
		return rts
	}	

	def dispatch void visit(CROModel builder, RoleConstraint rc, String ct) {

		val f = visitRoleGroup(rc.first)
		val s = visitRoleGroup(rc.second)
		val rg = switch rc {
			RoleImplication: new RoleGroup(1, 2, #[new RoleGroup(0, 0, #[f]), s])
			RoleProhibition: new RoleGroup(1, 1, #[f, s])
			RoleEquivalence: new RoleGroup(1, 2, #[new RoleGroup(0, 0, #[f, s]), new RoleGroup(2, 2, #[f, s])])
		}
		if (! builder.rolec.containsKey(ct))
			builder.rolec.put(ct, new ArrayList<Pair<Cardinality, Object>>)
		builder.rolec.get(ct).add(new Cardinality(0, -1) -> rg)
	}

	def dispatch void visit(CROModel builder, IntraRelationshipConstraint intra, String ct) {
		val rst = intra.relation.name
		val c = switch intra {
			Irreflexive: "irreflexive"
			Reflexive: "reflexive"
			Acyclic: "acyclic"
			Cyclic: "cyclic"
			Total: "total"
			default: null
		}
		if (c != null)
			builder.intra.add(rst -> ct -> c)
	}

	def dispatch void visit(CROModel builder, InterRelationshipConstraint inter, String ct) {

		//This is currently not supported
		val f = inter.first.name
		val s = inter.second.name
		val c = switch inter {
			RelationshipImplication: "implication"
			RelationshipExclusion: "exclusion"
			default: null
		}
		if (c != null)
			builder.inter.put(f -> ct -> s, c)
	}

	def dispatch void visit(CROModel builder, DataInheritance dtinh) {
		builder.dtinh.add(dtinh.sub.name -> dtinh.getSuper.name);
	}

	def dispatch void visit(CROModel builder, NaturalInheritance ntinh) {
		builder.ntinh.add(ntinh.sub.name -> ntinh.getSuper.name);
	}

	def dispatch void visit(CROModel builder, CompartmentInheritance ctinh) {
		builder.ctinh.add(ctinh.sub.name -> ctinh.getSuper.name);
	}

	def dispatch void visit(CROModel builder, RoleInheritance rtinh) {
		throw new UnsupportedOperationException("RoleInheritance is not supported")
	}

	def dispatch void visit(CROModel builder, EObject object) {
		//throw new UnsupportedOperationException("TODO: auto-generated method stub")
	}

}
