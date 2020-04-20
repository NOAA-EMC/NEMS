Introduction to NEMS {#introduction}
====================

The NOAA Environmental Modeling System (NEMS) is the infrastructure
underlying NOAA's [Unified Forecast System (UFS)] (https://ufscommunity.org/) a fully coupled modeling framework that supports predictions of
Earth's environment at a range of time scales. Examples of other
coupled modeling systems are the 
[Community Earth System Model (CESM)](http://www.cesm.ucar.edu)
and the
[Met Office Unified Model] (http://www.metoffice.gov.uk/research/modelling-systems/unified-model).

NEMS includes infrastructure for coupling model components
representing major Earth system domains and processes.  
**A model component** is a software representation of a physical
domain or process, for example sea ice. It is often developed by a
team of specialists in that domain. Model coupling is a software
representation of feedbacks between physical processes. It involves
modifying the exported fields of a component through grid, unit,
temporal, and other transformations so that they can be used as the
inputs for another component. These components are managed through repositories, primarily on GitHub.

In general, model components are
coupled through the NEMS mediator (in other coupled modeling systems
this is often called the "coupler").  NEMS also includes some
specialized mediators; for example, for space weather. In some cases
in NEMS, the model components are coupled "in-line", meaning that they
are called directly from another model component instead of having
fields sent through the mediator.

NEMS can be assembled into a number of different **modeling
applications** (often shortened to just applications). Modeling
applications are associated with a purpose, like medium-range
forecasting; a set of model components; and a set of parameters that
represent a range of supported options, including grids and
resolutions. Different NEMS modeling applications can have different
types and numbers of model components. Also, the same physical domain
may be represented by different model components in different modeling
applications. For example, in some NEMS modeling applications the
ocean component may be the HYbrid Coordinate Ocean Model (HYCOM) and
in others it may be the Modular Ocean Model (MOM).

Infrastructure
--------------

[NEMS is built using the Earth System Modeling Framework (ESMF)](https://www.earthsystemcog.org/projects/esmf/)
infrastructure software. ESMF provides utilities like generation of
interpolation weights and utilities for calendar and timee management,
and also wrappers that create a standard component calling
interface. This enables model components developed at different sites
to be coupled more easily.

[The National Unified Operational Prediction Capability (NUOPC) Layer] (https://earthsystemcog.org/projects/nuopc/)
adds additional rules about how ESMF models interact and increases
their interoperability. The NUOPC Layer covers aspects from the level
of build dependencies, to standardization of initialization phases,
all the way to standard names of the exchanged fields. NEMS is an
example of a modeling system built using the NUOPC Layer architecture.

Architecture 
------------

The NEMS architecture is based on an ESMF component hierarchy with the
application driver `MAIN_NEMS` at the top, calling into the
`NEMS_COMP` component, which in turn drives the `EARTH_COMP`
component. The `EARTH_COMP` drives the `ATM` and other components.
The architecture allows for
multiple `EARTH_COMP` instances, supporting ensemble applications such
as the Global Ensemble Forecast System (GEFS).

Coupled NEMS includes atmosphere, ocean, ice, wave, land,
aerosol/chemistry, and hydrologic models, with coupling interface and
utilities based on the 
[Earth System Modeling Framework (ESMF)](https://www.earthsystemcog.org/projects/esmf/).
The NEMS applications also utilize intreopereability conventions
introduced by the 
[National Unified Operational Prediction Capability (NUOPC)](https://www.earthsystemcog.org/projects/nuopc/).
