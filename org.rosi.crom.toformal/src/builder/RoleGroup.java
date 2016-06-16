package builder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class RoleGroup {
	public final Cardinality card;
	public final List<Object> elements=new ArrayList<Object>();
	
	public RoleGroup(int lower, int upper, List<Object> elems) {
		super();
		this.card=new Cardinality(lower,upper);
		add(elems);		
	}
	public RoleGroup(int lower, int upper, Object... elems) {
		super();
		this.card=new Cardinality(lower,upper);
		add(elems);		
	}
	public RoleGroup(Cardinality card, List<Object> elems) {
		super();
		if (card==null)
			throw new IllegalArgumentException("The given cardinality is null");
		this.card=card;
		add(elems);		
	}
	public RoleGroup(Cardinality card, Object... elems) {
		super();
		if (card==null)
			throw new IllegalArgumentException("The given cardinality is null");
		this.card=card;
		add(elems);		
	}
	public List<Object> getElements(){	return elements; }
	private void add(Object... elems){
		if (elems==null || elems.length==0) return;
		add(Arrays.asList(elems));
	}
	private void add(List<Object> elems){
		if (elems==null) return;
		for (Object o :elems)
			if (o instanceof RoleGroup)
				elements.add(o);
			else
				elements.add(o.toString());
	}
	@Override
	public String toString() {		
		Object[] a=elements.toArray();
		String s=(a.length>0 ? a[0].toString() : "");
		for (int i=1; i<a.length; i++) {s+=","+a[i].toString();	}
		if (card.upper==-1) return "RoleGroup(["+s+"],"+card.lower+",inf)";
		return "RoleGroup(["+s+"],"+card.lower+","+card.upper+")";
	}
}
