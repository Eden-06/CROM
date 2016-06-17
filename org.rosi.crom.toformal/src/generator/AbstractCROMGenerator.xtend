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

abstract class AbstractCROMGenerator implements IGenerator {

	private var ext = "txt"

	public new(String fileext) {
		if (fileext == null || fileext.isEmpty)
			throw new IllegalArgumentException("Argument ext should not be null or empty")
		this.ext = fileext
	}

	public def getExt(){ return ext; }

	public override generate(Shell shell, IPath path, Resource resource) {
		
		if (!(resource.contents.isEmpty || resource.contents.get(0) instanceof Model))
			throw new IllegalArgumentException("The given CROM model '" + path.toPortableString + "' was empty?")
		val model = resource.contents.get(0) as Model
		val transformation = generate(path,model);
		val n = path.removeFileExtension().addFileExtension(ext);
		val target = ResourcesPlugin.getWorkspace().getRoot().getFile(n);

		if (target.exists() && (MessageDialog.openQuestion(shell, "Confirmation Request",
			"The file '" + target.fullPath + "' already exists. Do you want to overwrite the existing file?"))){
				target.delete(true,null)
			}

		//Bottleneck due to missing StringOutputStream
		val content = new ByteArrayInputStream(transformation.getBytes(StandardCharsets.UTF_8));

		//create file in workspace
		target.create(content, IResource.NONE, null);
		
	}

	public def String generate(IPath path, Model model)
}
