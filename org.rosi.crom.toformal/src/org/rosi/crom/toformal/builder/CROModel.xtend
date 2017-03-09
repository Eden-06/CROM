package org.rosi.crom.toformal.builder

import java.util.ArrayList
import java.util.HashMap
import java.util.List

class CROModel {

	/**
	 * Set of data types
	 */
	public val dt = new ArrayList<String>
	/**
	 * $\prec_{DT} \subset DT \times DT$
	 */
	public val dtinh = new ArrayList<Pair<String, String>>
	/**
	 * Set of natural types
	 */
	public val nt = new ArrayList<String>
	/**
	 * $\prec_{NT} \subset NT \times NT$
	 */
	public val ntinh = new ArrayList<Pair<String, String>>
	/**
	 * Set of compartment types
	 */
	public val ct = new ArrayList<String>
	/**
	 * $\prec_{CT} \subset CT \times CT$
	 */
	public val ctinh = new ArrayList<Pair<String, String>>
	/**
	 * Set of role types
	 */
	public val rt = new ArrayList<String>
	/**
	 * Set of relationship types
	 */
	public val rst = new ArrayList<String>
	/**
	 * $\text{fills} \subseteq T \times CT \times RT$
	 */
	public val fills = new ArrayList<Pair<Pair<String, String>, String>>
	/**
	 * $\text{rel} : RST \times CT \rightarrow (RT \times RT)$
	 */
	public val rel = new HashMap<Pair<String, String>, Pair<String, String>>
	
	
	/**
	 * $\text{rolec} : CT \rightarrow 2^{\text{Card} \times RG}$
     */
	public val rolec = new HashMap<String, List<Pair<Cardinality, Object>>>
	/**
	 * $\text{card} : RST \times CT \rightarrow (Card \times Card)$
	 */
	public val card = new HashMap<Pair<String, String>, Pair<Cardinality, Cardinality>>
	/**
	 * $\text{intra} \subseteq RST \times CT \times \mathbb{E}$
	 */
	public val intra = new ArrayList<Pair<Pair<String, String>, String>>
	/**
	 * $\text{inter} \subseteq RST \times CT \times IRC \times RST $
	 */
	public val inter = new HashMap<Pair<Pair<String, String>, String>, String>
	/**
	 * $\text{fields} : T \cup (CT \times RT ) \rightarrow \text{MM}( F \times T )$
	 */
	public val fields = new HashMap<String, List<Pair<String, String> > >

	def initializeTest() {
		//CROM
		nt.addAll("Person", "Company", "Account")
		ct.addAll("Bank", "Transaction")
		rst.addAll("own_ca", "own_sa", "advises", "trans")
		fills.addAll(("Person" -> "Bank" -> "Consultant"), ("Person" -> "Bank" -> "Customer"),
			("Company" -> "Bank" -> "Customer"), ("Account" -> "Transaction" -> "Source"),
			("Account" -> "Transaction" -> "Target"), ("Account" -> "Bank" -> "CA"),
			("Account" -> "Bank" -> "SA"), ("Transaction" -> "Bank" -> "MoneyTransfer"))
		rel.put("own_ca" -> "Bank", ("Customer" -> "CA"))
		rel.put("own_sa" -> "Bank", ("Customer" -> "SA"))
		rel.put("advises" -> "Bank", ("Consultant" -> "Customer"))
		rel.put("trans" -> "Transaction", ("Source" -> "Target"))

		//constraint model
		rolec.put("Bank",
			#[new Cardinality(1, -1) -> "Consultant", new Cardinality(0, -1) -> new RoleGroup(1, 1, "CA", "SA")])
		rolec.put("Transaction", #[new Cardinality(2, 2) -> new RoleGroup(1, 1, "Source", "Target")])
		card.put("own_ca" -> "Bank", new Cardinality(1, 1) -> new Cardinality(0, -1))
		card.put("own_sa" -> "Bank", new Cardinality(1, -1) -> new Cardinality(0, -1))
		card.put("advises" -> "Bank", new Cardinality(0, 1) -> new Cardinality(1, -1))
		card.put("trans" -> "Transaction", new Cardinality(1, 1) -> new Cardinality(1, 1))
		intra.addAll("advises" -> "Bank" -> "irreflexive")
		inter.put("own_ca" -> "Bank" -> "own_sa" , "exclusion")
	}

}
