package org.rosi.crom.toformal.generator;

import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.swt.widgets.Shell;

public interface IGenerator {
	public void generate(Shell shell,IPath path,Resource resource);
}
