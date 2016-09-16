package org.rosi.crom.toformal.builder;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class Cardinality{
	private final Pattern pattern=Pattern.compile("([0-9]+)..(([*])|([0-9]+))");
	public final int lower;
	public final int upper;
	public Cardinality(int lower, int upper) {
		if (lower<0  || (upper!=-1 && lower>upper))
			throw new IllegalArgumentException("The given lower and upper bounds do not correspond to a valid Cardianilty");
	
		this.lower = lower;
		this.upper = upper;
	}
	public Cardinality() {
		this.lower = 0;
		this.upper = -1;
	}
	public Cardinality(String card) {
		if (card==null || card.isEmpty())
			throw new IllegalArgumentException("The given cardinality string is null or empty");
		Matcher m=pattern.matcher(card);
		if (! m.find())
			throw new IllegalArgumentException("The given string does not match the cardinality pattern\n\t"+pattern.toString());
		this.lower=Integer.parseInt(m.group(1));
		if (m.group(4)!=null)
			this.upper=Integer.parseInt(m.group(4));
		else // the alternative of the regexp (*) matched
			this.upper=-1;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 37;
		int result = 1;
		result = prime * result + lower;
		result = prime * result + upper;
		return result;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Cardinality))
			return false;
		Cardinality other = (Cardinality) obj;
		if (lower != other.lower)
			return false;
		if (upper != other.upper)
			return false;
		return true;
	}
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return (upper==-1 ? "("+lower + ",inf)" : "("+lower + ","+upper+")" );
	}		
}