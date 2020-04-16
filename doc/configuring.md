Configuring {#configuring}
===========

The NEMS application is highly configurable. During
build-time, i.e. when the NEMS executable is being compiled and
linked, choices are made as to which model and mediator components are
built into the system. The built-in components become accessible
during run-time, allowing the execution of different run
configurations without the need to rebuild NEMS.

Often the build and run configurability of NEMS is hidden from the
user by application or test scripts. However, there are times where it
is useful to understand the technical details on the level of the NEMS
executable.

Run Configuration
-----------------

During run-time of the NEMS executable, it accesses a file called
nems.configure, which it expects to find in the run directory. This
file specifies the dynamic component selection, and the exact run
sequence to be used. Only models built into the executable at
build-time are accessible during run-time. An error will be triggered
if an unavailable model is selected in nems.configure. The component
selection is based on two variables:

    xxx_model:          abc
    xxx_petlist_bounds: lower upper

Here `xxx` can be `atm`, `ocn`, `ice`, `ipm`, `med`. The `abc`
stands for the actual instance name, e.g. `fv3` or `mom6`. The
lower and upper bounds of the petList specification are integer PET
numbers.

The specification of the run sequence provides the flexibility needed
to cover different coupling scenarios. The format is line based
between special tags:

    runSeq::
        line1
        line2
        ...
        lineN
    ::

There are a number of format options for each line:

* A time loop is introduced by a `@` symbol, followed immediatly by
  the number of seconds of the associated time step.

* A time loop is closed by a single `@` symbol on a line.

* The `RUN` method of model component `xxx` is called by specifying
  just `xxx` on a line. The supported values of `xxx` are the same
  as for the model specification above. A specific RUN phase can be
  provided by adding the phase label to the same line, following the
  model string.

* A connector going from component `xxx` to component `yyy` is
  specified by a line like this: `xxx -> yyy`. An additional
  argument on the same line can be used to specify connection options
  for all of the field handled by the connector. The format and
  supported values of the connection options is documented in the
  NUOPC reference manual.

A very simple run sequence is running only the atmospheric uncoupled model.
The run sequence looks like:

    # Run Sequence #
    runSeq::
        ATM
    ::

A more complex example is when components are exchanging information at two time scales:

    # Run Sequence #
    runSeq::
        @7200.0
        OCN -> MED
        MED MedPhase_slow
        MED -> OCN
        OCN
        @3600.0
            MED MedPhase_fast_before
            MED -> ATM
            ATM
            ATM -> MED
            MED MedPhase_fast_after
          @
        @
    ::

Anything on a line after the `#` symbol is treated as a comment and
ignored. Indentation in the formatting does not change the meaning of
a line, but is purely used to increase readability.
