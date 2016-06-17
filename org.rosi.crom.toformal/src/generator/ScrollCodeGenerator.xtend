package generator

import org.eclipse.core.runtime.IPath
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.swt.widgets.Shell
import org.eclipse.core.resources.ResourcesPlugin
import java.io.ByteArrayInputStream
import org.eclipse.core.resources.IResource
import java.nio.charset.StandardCharsets
import org.eclipse.jface.dialogs.MessageDialog
import crom_l1_composed.Model

class ScrollCodeGenerator extends AbstractCROMGenerator {

	public new() {
		super("scala");
	}

	override generate(IPath path, Model model) {
		var transformation = new CROMGenerator(true).generate(model)

		//Begin of the dirty dirty hack
		val modelname = path.toFile.name.replace(".crom","")
		System.out.println(modelname)
		if (! modelname.isEmpty)
			transformation=transformation.replace("CROMApplication", modelname)
		return transformation
	}

}
