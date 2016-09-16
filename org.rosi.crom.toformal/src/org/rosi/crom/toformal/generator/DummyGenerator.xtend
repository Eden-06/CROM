package org.rosi.crom.toformal.generator

import org.rosi.crom.toformal.generator.IGenerator
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.core.runtime.IPath
import crom_l1_composed.Model
import org.eclipse.swt.widgets.Shell
import org.eclipse.jface.dialogs.MessageDialog

class DummyGenerator implements IGenerator {
	
	override generate(Shell shell, IPath path, Resource resource) {
		var s=""
		for (o: resource.contents){
			val m=o as Model
			for (e:m.elements){
				s+=e.getName()+"\n"
			}
		}		
		val transformation = s;				
		val n=path.removeFileExtension().addFileExtension("txt");
		//val target=ResourcesPlugin.getWorkspace().getRoot().getFile(n);
		//Bottleneck due to missing StringOutputStream
		//val content= new ByteArrayInputStream(transformation.getBytes(StandardCharsets.UTF_8));
		//create file in workspace
		//target.create(content, IResource.NONE, null);
		MessageDialog.openInformation(shell,"Simulated Generation for "+n,transformation);			
		//throw new UnsupportedOperationException("TODO: auto-generated method stub")
	}	
	
}