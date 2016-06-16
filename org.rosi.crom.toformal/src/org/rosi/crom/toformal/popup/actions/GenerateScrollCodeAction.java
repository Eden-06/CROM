package org.rosi.crom.toformal.popup.actions;

import generator.ScrollCodeGenerator;

public class GenerateScrollCodeAction extends AbstractGenerateAction {

	public GenerateScrollCodeAction(){
		super();
		generator=new ScrollCodeGenerator();
	}
}
