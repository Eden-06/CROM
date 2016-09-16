package org.rosi.crom.toformal.popup.actions;

import org.rosi.crom.toformal.generator.FormalCROMGenerator;

public class GenerateFormalCromAction extends AbstractGenerateAction {
	
	/**
	 * Constructor for Action.
	 */
	public GenerateFormalCromAction() {
		super();
		generator=new FormalCROMGenerator();
	}
}
