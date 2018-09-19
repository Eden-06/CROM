/**
 * 
 */
package org.rosi.crom.toformal.cli;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.xmi.impl.XMIResourceFactoryImpl;
import org.rosi.crom.toformal.generator.AbstractCROMGenerator;
import org.rosi.crom.toformal.generator.FormalCROMGenerator;
import org.rosi.crom.toformal.generator.OntologyGenerator;
import org.rosi.crom.toformal.generator.RSQLGenerator;
import org.rosi.crom.toformal.generator.ScrollCodeGenerator;

import crom_l1_composed.Crom_l1_composedPackage;
import crom_l1_composed.Model;

/**
 * @author thomas
 *
 */
public class ToFormalCLI {
	
	public static final String Version = "1.0.2";

	public static final String Documentation = "org.rosi.crom.toformal.cli OPTIONS FILES\n"
			+ " transform a set of *.crom FILES to another representation (default: owl)\n"
			+ "OPTIONS\n"
			+ " -tEXT Extension of the files to create (py,owl,scala,rsql)\n"
			+ " -v    Verbose output\n"
			+ "FILES\n"
			+ " List of CROM (*.crom) files to process\n\n"
			+ "VERSION\n"+Version;

	public static final AbstractCROMGenerator[] generators = {new OntologyGenerator(),new FormalCROMGenerator(),new RSQLGenerator(),new ScrollCodeGenerator()};

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length <= 1) {
			System.out.println(Documentation);
			System.exit(0);
		}
		List<String> files = new ArrayList<String>();
		String extension="owl";
		boolean verbose = false;

		for (String s : args) {
			try {
				if (s.equals("-v")) {
					verbose = true;
				} else if (s.startsWith("-t")) {
					extension=s.replace("-t", "");
				} else {
					if (s.endsWith(".crom"))
						files.add(s);
					else
						System.err
								.println("Error: Can only process CROM files "
										+ s);
				}
			} catch (Exception exception) {
				System.err.println(exception.getMessage());
				if (verbose)
					exception.printStackTrace();
				System.exit(0);
			}
		}
		AbstractCROMGenerator generator=null;
		for (AbstractCROMGenerator g:generators){
			if (extension.equals(g.getExt()))
				generator=g;
		}
		if (generator==null){
			System.err.println("There is no generator for the given extension "+ extension);
			for (AbstractCROMGenerator g:generators){
				System.err.println(" "+g.getExt()+" : "+g.getClass().getSimpleName());
			}
			System.exit(2);
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
		for (String f : files) {
			Resource resource = null;
			FileWriter target = null;
			BufferedWriter buffered = null;
			try {
				if (verbose)
					System.out.println(String.format("Converting %s...", f));
				resource = resourceSet.getResource(URI.createURI(f), true);
				if (!(resource.getContents().isEmpty() || (resource
						.getContents().get(0) instanceof Model))) {
					throw new IllegalArgumentException(
							"The given CROM model \'" + f + "\' was empty?");
				}

				if (verbose)
					System.out.println(String
							.format("Transforming model...", f));
				Model model = (Model) resource.getContents().get(0);
				String modelname = f.replace(".crom", "");
				String transformation = generator.generate(modelname, model);

				// save file
				String t = f.replace(".crom", "." + generator.getExt());
				if (verbose)
					System.out.println(String.format("Storing model to %s...",
							t));
				// byte[] _bytes =
				// transformation.getBytes(StandardCharsets.UTF_8);
				// target = new FileOutputStream(t);
				target = new FileWriter(t);
				buffered = new BufferedWriter(target, 1024 * 1024);
				buffered.write(transformation);

			} catch (Exception exception) {
				if (verbose)
					exception.printStackTrace();
				System.err.println("[Error:] " + f + " "
						+ exception.getClass().getSimpleName() + " "
						+ exception.getMessage());
			} finally {
				try {
					if (buffered != null)
						buffered.close();
					if (target != null)
						target.close();
				} catch (IOException exception) {
					if (verbose)
						exception.printStackTrace();
					System.err.println("[Error:] " + f + " "
							+ exception.getClass().getSimpleName() + " "
							+ exception.getMessage());

				}
				if (resource != null)
					resource.unload();
			}
		}

	}

}
