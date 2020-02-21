Repository Structure and Versioning {#structure}
===================================

NEMS modeling applications are multi-component coupled systems that
must pull together, in a systematic way, components from different
locations. The NEMS AppBuilder enables users to construct a specific,
versioned modeling application from a versioned set of model
components and configuration files. The AppBuilder approach also helps
to ensure that changes made to the different applications are
coordinated as they get checked back into the NEMS repository.

The NEMS AppBuilder introduces two levels of configuration control:
the modeling application level and the NEMS level. This split provides
the model developer with full version control over all aspects of the
modeling application code, while hiding many technical details about
the infrastructure at the NEMS level.

Exposed to/changeable by the user:

* Version of NEMS.
* List of external components (e.g. MOM5, CICE, ...) required by the application.
* Version of each required external component.
* Location of source and installation directory for NEMS and each external component.
* Compiler and 3rd party library versions.

Mostly hidden from the user:
* Knowledge base of how to build each of the supported external components.
* Infrastructure to configure and build NEMS according to the
  information specified by the application layer.
* Coupling system based on ESMF/NUOPC.

Overall Repository Structure
----------------------------

The NEMS repository is divided into three types of areas:

 * NEMS Framework - This repository contains the mediator, build
   system, compset runner, and this documentation

 * model components and caps - These reside in other repositories

 * NEMS Applications - A group of repositories, one for each NEMS
   application.  They contain Subversion Externals to the NEMS
   Framework, and each component.

