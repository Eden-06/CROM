package org.rosi.crom.toformal.popup.actions;

import org.rosi.crom.toformal.generator.OntologyGenerator;



public class GenerateOntologyAction extends AbstractGenerateAction {
	
	/**
	 * Constructor for Action.
	 */
	public GenerateOntologyAction() {
		super();
		generator=new OntologyGenerator();
	}
}
