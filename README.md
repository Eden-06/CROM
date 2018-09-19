CROM
====

The Compartment Role Object Metamodel (CROM) is a feature-complete, comprehensive model for role-based modeling and programming languages.
In addition to the Metamodel, this repository also includes a code generator from CROM to the [formal CROM](https://github.com/Eden-06/formalCROM).

## Installation

1. Make sure you have Java 1.8 installed and set as default for the whole *Eclipse Workbench* **(Very Important)**
2. Clone the repository to your Eclipse installation
3. Open the \*.genmodel and Reload it (using the `model/crom\_l1\_composed.ecore` file)
4. Afterwards, open the `src-gen/crom\_l1\_composed.genmodel` and use the context menu of the root node to *generate all* classes

## Requirements for the Code Generator

* Eclipse Modeling Tools (version Photon) [[here]](https://www.eclipse.org/downloads/packages/release/photon/r/eclipse-modeling-tools).
* [Xtext](https://github.com/eclipse/xtext)
* [Xtend](https://github.com/eclipse/xtext-xtend)

## Graphical Representation of CROM

[![Graphical model of the CROM metamodel](https://raw.githubusercontent.com/Eden-06/CROM/master/org.rosi.crom.metamodel/model/crom_l1_composed.png)](https://raw.githubusercontent.com/Eden-06/CROM/master/org.rosi.crom.metamodel/model/crom_l1_composed.svg)
