package org.rosi.crom.toformal.generator

import crom_l1_composed.Model
import generator.CROMGenerator

class ScrollCodeGenerator extends AbstractCROMGenerator {

	public new() {
		super("scala");
	}

	public override generate(String modelname, Model model) {
		//Begin of the dirty dirty hack
		System.out.println(modelname)
		var transformation = new CROMGenerator(true).generate(model)
		if (! modelname.isEmpty)
			transformation=transformation.replace("CROMApplication", modelname)
		return transformation
	}

}
