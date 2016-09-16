package org.rosi.crom.toformal.generator

import org.eclipse.core.runtime.IPath
import crom_l1_composed.Model
import generator.CROMGenerator

class ScrollCodeGenerator extends AbstractCROMGenerator {

	public new() {
		super("scala");
	}

	override generate(IPath path, Model model) {
		//Begin of the dirty dirty hack
		val modelname = path.toFile.name.replace(".crom","")
		System.out.println(modelname)
		var transformation = new CROMGenerator(true).generate(model)
		if (! modelname.isEmpty)
			transformation=transformation.replace("CROMApplication", modelname)
		return transformation
	}

}
