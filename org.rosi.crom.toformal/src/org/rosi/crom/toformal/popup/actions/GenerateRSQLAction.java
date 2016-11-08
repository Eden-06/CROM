package org.rosi.crom.toformal.popup.actions;

import org.rosi.crom.toformal.generator.RSQLGenerator;

public class GenerateRSQLAction extends AbstractGenerateAction {
	
	/**
	 * Constructor for Action.
	 */
	public GenerateRSQLAction() {
		super();
		generator=new RSQLGenerator();
	}
}
