package org.rosi.crom.toformal.popup.actions;

import org.rosi.crom.toformal.generator.ScrollCodeGenerator;

public class GenerateScrollCodeAction extends AbstractGenerateAction {

	public GenerateScrollCodeAction(){
		super();
		generator=new ScrollCodeGenerator();
	}
}
