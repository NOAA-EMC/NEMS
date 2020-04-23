Mediator Reference {#mediator}
=======================

This page describes the main NEMS mediator. There is also a 
\ref sw_mediator "NEMS space weather mediator".

In the NUOPC Layer architecture, the mediator (often called the
coupler) handles the scientific and customized aspects of coupling for
a modeling system.  For example, it may handle transfer of coupling
fields, regridding, merging, treatments of coastlines or custom field
transformations.

Overview
--------

In NEMS, the mediator is a separate gridded component with multiple
phases.  Phases can be thought of as different subroutines within the
mediator that can be called in some sequence to carry out the work of
the mediator.  The mediator runs on its own set of PETs (persistent
execution threads, similar to processors).  Often the PETs chosen for
the mediator overlap with other components to optimize resource
utilization.  The PETs and run sequence are specified by the compset.
Mediator phases might have names like prep_atm, prep_ocn, restart,
accumulate, and so forth.  The way the mediator phases are implemented
is somewhat arbitrary and will probably evolve over time as sequencing
requirements change.

Mediator PE layout
------------------

The mediator PETs are set in NEMS in the nems.configure file.  Other
mediator attributes are also defined there in a section that looks
like this,

    MED_model:                         nems
    MED_petlist_bounds:             76 135
    MED_attributes::
      Verbosity = 0
      DumpFields = true
      DumpRHs = false
      coldstart = false
      restart_interval = 0
    ::

Components
----------

The main NEMS mediator is capable of technically coupling atmosphere,
ocean, sea ice, land, hydrology, and wave components. All of these
component types have demonstrated techically correct passage of fields
through the mediator. Only the behavior of atmosphere, ocean, and sea
ice components has been examined to assess correct physical coupling.

All components do not have to be present to operate the NEMS mediator.

Exchange Fields
---------------

The mediator and all components advertise fields during
initialization.  NUOPC reconciles those field names and then coupling
fields are realized and connected.  In order for a field to be
connected between two components, it has to be advertised in the two
components as well as in the mediator. Field names that match between
components are automaticallly coupled in the system.  Any field that
is exported from one component and imported to another component with
the same standard name is coupled between those components.

Coupling Periods
----------------

There is a slow and a fast coupling period in the NEMS mediator.  The
slow coupling period is associated with the ocean model and allows the
ocean model to be coupled at a lower frequency than other components.
The fast coupling period is for the atmosphere and ice model.  They
are coupled at the same frequency in the system.

Accumulation and Averaging
--------------------------

The mediator accumulates all fields between coupling periods for all
components.  For example, the atmosphere and ice coupling fields are
accumulated and averaged between calls to the ocean model.  At the
same time, the ocean fields coupled to the atmosphere and ice models
are held static between the longer ocean coupling periods.

Grids
-----

Model grids are passed to the mediator at initialization.  The
mediator receives those grids and instantiates a decomposition of
those grids on its PETs (persistent execution threads, similar to
processors).  The mediator is then able to receive all model fields on
the native model grids.

The ocean and ice components will generally be run on the same grid
for science reasons, but the mediator is implemented such that this is
not a requirement.

Interpolation (Regridding)
--------------------------

Regridding is performed by the mediator. The regridding weights are
computed at initialization and depend on the grids and regridding
method.  The regridding method is defined in the mediator on a
field-by-field basis when the field is defined. In general, fluxes are
mapped conservatively, and states are mapped bilinearly. The higher
order finite element patch method was not used for any fields because
of an observed reproducibility issue.

In the current revision, fields transferred from the ocean to the sea
ice component are copied rather than interpolated. This can be done
because the ocean and sea ice components are on the same grid. When
different grids are detected for these components, the interpolation
method defaults to bilinear.

Run Sequence
------------

The run sequence is evolving as new requirements are defined.  There
are several mediator phases currently implemented in the mediator and
a typical UGCS-Seasonal run sequence is set up as follows as shown in
a typical nems.configure file,

    runSeq::
     @1800.0
       MED MedPhase_prep_ocn
       MED -> OCN :remapMethod=redist
       OCN
       @900.0
         MED MedPhase_prep_ice
         MED MedPhase_prep_atm
         MED -> ATM :remapMethod=redist
         MED -> ICE :remapMethod=redist
         ATM
         ICE
         ATM -> MED :remapMethod=redist
         ICE -> MED :remapMethod=redist
         MED MedPhase_atm_ocn_flux
         MED MedPhase_accum_fast
       @
       OCN -> MED :remapMethod=redist
       MED MedPhase_write_restart
     @
    ::

In the file above, the sequence of component run calls (ie. OCN, ICE,
ATM), field coupling via connectors (ie. ATM to MED), and mediator
phases (ie. \c "MedPhase_prep_ice", \c "MedPhase_atm_ocn_flux",
MedPhase_write_restart) are indicated.  The coupling periods are also
defined (ie. \@1800.0, \@900.0) for the ocean and atmosphere/ice models.

The current implementation of the mediator phases does the following,

\c MedPhase_prep_ocn - prepares the ocean coupling fields for the ocean
model by averaging the atm/ice coupling fields, interpolating fields
to the ocean grid, computing any merges or custom field calculations,
and then filling the ESMF State sent to the ocean model.

\c MedPhase_prep_atm - prepares the atmosphere coupling fields for the
atmosphere model by interpolating fields to the atmosphere grid,
computing any merges or custom field calculations, and then filling
the ESMF State sent to the atmosphere model.

\c MedPhase_prep_ice - prepares the sea ice coupling fields for the sea
ice model by interpolating fields to the sea ice grid, computing any
merges or custom field calculations, and then filling the ESMF State
sent to the sea ice model.

\c MedPhase_atm_ocn_flux - computes the atmosphere/ocean fluxes from
atmosphere and ocean fields.  The computation is done on the ocean
grid and data from the atmosphere model are interpolated to the ocean
grid.  These fluxes can be used in the atmosphere and ocean model.

\c MedPhase_accum_fast - accumulates the atmosphere and ice coupling
fields for coupling to the ocean model.

\c MedPhase_write_restart - writes mediator restart files.

Reconciliation of Masks
-----------------------

The land mask implementation is described in more detail here.

### Exchange Field Sign and Direction Conventions

The NEMS mediator uses the convention that heat/water/momentum flux and wind stress is
positive downward. There is also a hierachy of "down" with respect to
models, which from top to bottom is:

 * atm
 * lnd
 * rof
 * ice
 * ocn

If a flux in the coupler is positive, that means it's transferring
heat/water/momentum downward from a higher component to a lower
component.

Model components have to have consistent conventions for fluxes that NEMS mediator knows about
so that it can adjust signs when passing data from one component to another.

Flux Field Treatment
--------------------

The NEMS system couples heat, water, and momentum fluxes.  In general,
fluxes are computed at the interface between two components and those
fluxes are often computed in one of the two components or in the
mediator.  Fluxes are normally interpolated conservatively, but the
best regridding method for a particular application is a science
issue.

The mediator is able to compute fluxes as well as regrid and couple
fluxes computed in components.  There is no specific constraint on how
or where the fluxes should be computed.  Often the choice depends on
the relative coupling frequency, the amount of information required to
be coupled to compute the fluxes, the sophistication of the flux
calculation, the relative grid resolution, and whether an exchange
grid is used.

For some components, fluxes need to be merged.  For instance, on the
atmosphere grid, the ice fraction might evolve in time and the land
fraction might be static.  For conservation, the merging of fluxes is
as important as the regridding and reuse of fluxes.  Again, this is a
science issue.

To compute fluxes,

 * A specific flux method and implementation have to exist or be developed.

 * The required coupling fields to compute the flux have to be
   interpolated and passed into the flux calculation.

 * The computed fluxes have to be interpolated and passed to the relevant components.

 * There needs to be some coordination between components and the
   mediator about what fields are coupled and where computations are
   carried out.

