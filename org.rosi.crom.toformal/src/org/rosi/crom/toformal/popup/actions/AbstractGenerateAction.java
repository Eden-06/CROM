package org.rosi.crom.toformal.popup.actions;

import org.rosi.crom.toformal.generator.DummyGenerator;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.rosi.crom.toformal.generator.IGenerator;

import crom_l1_composed.Crom_l1_composedPackage;

public abstract class AbstractGenerateAction implements IObjectActionDelegate {

	private Shell shell;
	private ISelection selection;
	protected IGenerator generator= new DummyGenerator();

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	@Override
	public final void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
		selection = targetPart.getSite().getSelectionProvider().getSelection();
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 */
	@Override
	public final void run(IAction action) {
		if (selection == null || generator == null || shell == null
				|| selection.isEmpty())
			return;
		IStructuredSelection i = (IStructuredSelection) selection;
		List<IPath> list = new ArrayList<IPath>();
		List<String> errors = new ArrayList<String>();
		for (Object f : i.toList()) {
			if (f != null && f instanceof IFile) {
				IPath p = ((IFile) f).getFullPath();
				if (p != null)
					list.add(p);
			}
		}
		// Create a resource set to hold the resources.
		//
		ResourceSet resourceSet = new ResourceSetImpl();

		// Register the appropriate resource factory to handle all file
		// extensions.
		//
		resourceSet
				.getResourceFactoryRegistry()
				.getExtensionToFactoryMap()
				.put(Resource.Factory.Registry.DEFAULT_EXTENSION,
						new XMIResourceFactoryImpl());

		// Register the package to ensure it is available during loading.
		//
		resourceSet.getPackageRegistry().put(Crom_l1_composedPackage.eNS_URI,
				Crom_l1_composedPackage.eINSTANCE);
		Resource resource = null;
		for (IPath p : list) {			
			try {
				resource = resourceSet.getResource(
						URI.createURI(p.toString()), true);
				// resource.load(new FileInputStream(new
				// File(p.toPortableString())),Collections.EMPTY_MAP);
				generator.generate(shell, p, resource);
			} catch (Exception exception) {
				exception.printStackTrace();
				errors.add("[Error:] " + p.toString() + " "
						+ exception.getClass().getSimpleName() + " "
						+ exception.getMessage());
			}
		}
		if (!errors.isEmpty()) {
			String s = "";
			for (String e : errors)
				s += e + "\n";
			MessageDialog.openError(shell,
					"Not all formal CROM files could be generated", s);
		}
		// MessageDialog.openInformation(shell,"Generator for Formal Model","Formal CROM was executed on:\n"+filenames);
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	@Override
	public final void selectionChanged(IAction action, ISelection selection) {
	}

}
