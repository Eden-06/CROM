package generator

import org.eclipse.core.runtime.IPath
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.swt.widgets.Shell
import builder.CROMVisitor
import org.eclipse.core.resources.ResourcesPlugin
import java.io.ByteArrayInputStream
import org.eclipse.core.resources.IResource
import java.nio.charset.StandardCharsets
import org.eclipse.jface.dialogs.MessageDialog
import builder.CROModel

abstract class AbstractCROMGenerator implements IGenerator {

	private var ext = "txt"

	public new(String fileext) {
		if (fileext == null || fileext.isEmpty)
			throw new IllegalArgumentException("Argument ext should not be null or empty")
		this.ext = fileext
	}

	public override generate(Shell shell, IPath path, Resource resource) {
		val model = new CROModel
		val visitor = new CROMVisitor
		for (c : resource.contents)
			visitor.visit(model, c)
		val transformation = generate(model);
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

	public def String generate(CROModel b)
}
