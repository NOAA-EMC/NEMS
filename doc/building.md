Compiling: NEMSAppBuilder and GNUmakefile {#building}
=========================================

NEMS is the technical backbone for multiple modeling
applications. These modeling applications are multi-component coupled
systems that must pull together, in a systematic way, components from
different centers and agencies. The build system enables users to
construct a specific, versioned modeling application from a versioned
set of model components and configuration files.

The NEMS build system uses a GNU Makefile built on top of the GNU Make
Standard Library.  The prior approach, the NEMSAppBuilder, a massive
shell script, has been abandoned.  A script by the same name is in
place, but it is simply a wrapper around "make."  This was created to
support existing software; it is suggested to switch to calling the
GNU Makefile directly because it provides more fine-grained control
over what to build and when.

The build system is split into two levels: Application and NEMS.  In
situations where both levels have a capability, the application
version (if present) overrides the NEMS settings.  This allows
multiple applications to use the same NEMS, while having slightly
different component build rules or modulefile selections.

* Application level:
  * Version of NEMS.
  * List of external components and frameworks (MOM6, FV3, CICE, FMS, etc.)
  * Version of each external component or framework.
  * Location of source and installation areas for each external component or framework.
  * Build rules for each component and framework.
  * Build environment, such as compilers, MPI selection, and third-party libraries.

* NEMS level:
  * Dependencies between components and frameworks (ie. FMS must be built before MOM6)
  * Location of source and installation areas for each external component or framework.
  * Build rules for each component and framework.
  * Coupling system based on ESMF/NUOPC.




Building with the GNU Makefile
------------------------------

The top-level makefile is located within the
\ref structure
as ./NEMS/GNUmakefile.  It is a GNU makefile, and will not work with
other Make implementations.  

The first step to running the build system is to be within the NEMS directory

    git clone (insert url here) my-app
    cd my-app/NEMS

\info You must `cd` into the `NEMS/` directory before you execute the new build system.

There are two ways to call the makefile: with an appBuilder file, or
without.  Here is an example command line that will build the coupled
FV3-MOM6-CICE5 on NOAA RDHPCS Theia with the Intel compiler:

    make COMPONENTS=FV3,MOM6,CICE \
         CHOSEN_MODULE=theia.intel/fv3_coupled \
         CONFOPT=configure.fv3_coupled.theia.intel \
         build

An equivalent way to do this is to place the commands within an
appBuilder file.  The appBuilder files belong at the application level
(directory above NEMS).  One of the advantages of an appBuilder file
is that some of the build system information is available to
facilitate automatic decisions, such as machine-specific
configuration.  An appBuilder file for the coupled FV3-MOM6-CICE5
would look something like this:

    # Contents of coupledFV3_MOM6_CICE.appBuilder
    COMPONENTS=( FV3 MOM6 CICE )
    CHOSEN_MODULE=$FULL_MACHINE_ID/fv3_coupled
    CONFOPT=configure.fv3_coupled.$FULL_MACHINE_ID

Note the addition of the `$FULL_MACHINE_ID`.  That is one of several
special variables discussed later.  Also notice that the file is a
bash shell script.  This was done for backward compatibility, to
prevent users from having to rewrite their existing appBuilder files.
However, only a limited set of variables is forwarded from the
appBuilder file to Make.  These are listed later in this page.

The new command to build would be:

    make app=coupledFV3_MOM6_CICE build

Later sections will discuss variables to configure the build
environment, variables that configure components, and different build
targets for compiling or cleaning NEMS (or parts thereof).



### Instructing the NEMS build system: Phony Targets

The NEMS build system can be instructed to do various things, such as
cleaning NEMS (or part of it), building NEMS (or part of it), or
providing information about the build.  This is implemented via what
are called "phony targets."  Those are build targets that do not
correspond to an actual file.  An example from the above section is
the "build" target:

    make [options] build

Internally, that enables targets for the NEMS executable and all
component ESMF makefile fragments.  

There are several other build rules for the NEMS level, and several
for each component:

* `build` --- compile the NEMS executable and all ESMF makefile fragments
* `clean` --- delete compiler intermediate files (.o, .mod, etc.) from NEMS and all components or frameworks.
* `distclean` --- in addition to `clean`, deletes final targets like libraries, the NEMS executable, and ESMF makefile fragments.
* `debug_info` --- the default target if none is specified; provides information about the build environment and selected components.

For each component or external framework, there are three more rules
to act just on that.  For a hypothetical component "FOO:"

* `build_FOO` --- create the ESMF makefile fragment for FOO, and any other targets needed to link FOO to NEMS.

* `clean_FOO` --- delete all of FOO's compiler intermediate files

* `distclean_FOO` --- same as `clean_FOO` but also deletes the ESMF
  makefile fragment, the component's library and any other
  non-intermediate targets.



### The Build Environment

There are three steps to setting up the build environment, and are
described in more detail in comments within the `NEMS/GNUmakefile`:

1. Platform capability settings, controlled by the `BUILD_ENV`
   variable.  This also sets up defaults for later steps.

2. Selection of `configure.nems`, `modules.nems`, and other
   configuration files.  These are generally chosen by the application
   level, but suitable defaults are provided.

3. Application overrides of steps 1 and 2.

The goal of these steps is to set values for the following variables,
which configure later steps of the build:

* `BUILD_ENV` --- a description of the build environment, used to
  select one by name.  Generally, this should be the FULL_MACHINE_ID
  or BUILD_TARGET.

* `MACHINE_ID` --- a short name for the machine, excluding any details
  about the build environment or partition.  Example: "wcoss" for NOAA
  WCOSS phases 1, 2, and 3.

* `FULL_MACHINE_ID` --- a longer name for the machine, which may
  include more information about the build environment or partition.
  Example: "wcoss_phase2" for NOAA WCOSS Phase 2.

* `USE_MODULES` --- `YES` or `NO`: does the unix "module" command work
  on this machine?  If `NO`, the modules.nems will be "sourced" as a
  shell include file instead.

* `MODULE_LOGIC` --- a shell command or series of shell commands to
  load the `modules.nems` file.

* `BUILD_TARGET` --- the machine and partition description, and
  further details about the build environment.  It must have the
  format `$(MACHINE).$(NEMS_COMPILER)` where MACHINE is the
  FULL_MACHINE_ID or the MACHINE_ID.

* `NEMS_COMPILER` --- the name of a compiler suite to use, such as
  gnu, pgi, or intel.

* `DEFAULT_MODULE` --- the default modulefile or shell include file
  to use for modules.nems on this platform.

* `MODULE_DIR` --- the directory that contains application modulefiles
  for this platform.  Default is the `modulefiles/$(FULL_MACHINE_ID)`
  relative to the application level.

* `CHOSEN_MODULE` --- the name of the modulefile within
  `$(MODULE_DIR)` to use as `modules.nems`.

* `MODULE_LIST` --- optional: a shell command or sequence of shell
  commands to list the loaded modules.

* `CONFIGURE_NEMS_FILE` --- the location of the `configure.nems` file,
  either an absolute path or a path relative to the application-level
  `conf/` directory.

* `ESMF_VERSION_DEFINE` ---  the location of the `NEMS/src/ESMFVersionDefine.h` file,
  either an absolute path or a path relative to the application-level
  `conf/` directory.

* `EXTERNALS_NEMS_FILE` --- the location of the `externals.nems` file,
  either an absolute path or a path relative to the application-level
  `conf/` directory.

The list of known build environments (values of `BUILD_ENV`) are
obtained via the logic within `NEMS/src/incmake/env/` and
application-level `conf/env`.  Each subdirectory of `env` contains a
`detect.mk` which detects the build environments available on the
current machine, and specifies a makefile fragment that will set up
the environment.  For example, on the NOAA Jet cluster, the four
detected build environments will be `jet`, `linux.gnu`, `linux.intel`,
and `linux.pgi`.


### Parallel Builds

In Make, a parallel build is requested by the `-j` option:

    make -j 8 [options] build

Within the makefile, when another make is executed in a certain way
(called a submake), the `-j` option is passed down to it.  Hence,
executing `make` at the top level with the `-j` option will pass that
same option down to all components.  

Note that some components will fail to build, or build incorrectly, if
they receive `-j` options greater than zero.  For those components, it
is critical that their `component_FOO.mk` explicitly set `-j 1` in
submakes to disable parallel builds of that component's internal build
system.



Configuring the NEMS Build
--------------------------

### Component Build Configuration

The components are specified via the `$(COMPONENTS)` variable.  That
is a space-separated or comma-separated list of components that should
be included in the NEMS build.  Components may be added or reordered
based on the logic in the `NEMS/src/incmake/dependencies.mk` file.  Example:

    COMPONENTS=FV3 MOM6 CICE CCPP

would build the coupled FV3-MOM6-CICE5 system with CCPP included.  In
reality, CCPP must be built first, and FMS is required as well.  The
`NEMS/src/incmake/dependencies.mk` knows this, and will update the
components list:

    COMPONENTS=FMS CCPP FV3 MOM6 CICE

For each component FOO, these variables may be set to control how and
where it is built:

* `foo_mk` --- the path to the ESMF makefile fragment for the component

* `FOO_BINDIR` --- the path to the directory that contains the
  component's library and any other files needed to link the component
  to NEMS.

* `FOO_SRCDIR` --- the directory that contains the component source code

* `FOO_MAKEOPT` --- component "make" options; the meaning is component-specific.

* `FOO_CONFOPT` --- component configuration options; the meaning is component-specific.

* `FOO_BUILDOPT` --- component build options; the meaning is component-specific.

Each component's build rules, in `NEMS/src/incmake/component_FOO.mk`,
decides how to build the component, based on those six variables.  The
NEMS build logic, in `NEMS/src/GNUmakefile`, will decide how to link the
component, based on `FOO_BINDIR` and the ESMF makefile fragment.

As a convenience, in the `$(COMPONENT)` variable, a component name may
be followed by a series of options of the form
`FOO%OPT1=ARG1%OPT2=ARG2%...`.  If so, the `OPT=ARG` pairs are added
to the special `FOO_MAKEOPT` variable for that component.  For
example, if we want to build FV3 and FMS in debug mode, using the
component list from before:

    COMPONENTS=FV3%DEBUG=Y MOM6 CICE5 CCPP




### Build Environment Specification Files: `modulefile` and `configure` File

The NEMS build system requires two files to set up the build
environment.  

 * `modulefiles/(machine)/(modulefile)` - This will become
   `NEMS/src/conf/modules.nems`.  On a machine with the UNIX `module`
   command, this will be loaded as a modulefile.  On other machines,
   this file will be "sourced" as a shell include file.

 * `conf/configure.nems.*` - coped to `NEMS/src/conf/configure.nems`.
   This sets variables required by NEMS build scripts.  This is a
   Makefile fragment, which will be included by the makefiles within
   `NEMS/src`.  Some components use this file as well, such as the FV3.



### Modifying Component Build Rules or Adding Components

An example `component_FOO.mk` with more detailed instructions can be
found in `NEMS/src/incmake/example_component_FOO.mk`.  Please read
that example before attempting to add or modify a component; we
provide information here as well for the benefit of this document's
readers.  This information is not sufficient though; you must read the
example file as well.

Adding a component FOO is as simple as adding this file:

    NEMS/src/incmake/component_FOO.mk

OR, at the application level:

    conf/component_FOO.mk

If both are present, the application-level file is used.  If a
component has dependencies, then this file must be updated:

    NEMS/src/incmake/dependencies.mk

For technical reasons, at this time, that file cannot be overridden at
the application level.

At a minimum, the `component_FOO.mk` must set these four variables:

    foo_mk=$(FOO_BINDIR)/foo.mk
    all_component_mk_files+=$(foo_mk)
    
    FOO_SRCDIR?=$(ROOTDIR)/FOO
    FOO_BINDIR?=$(ROOTDIR)/FOO_INSTALL

Note that the `FOO_SRCDIR` and `FOO_BINDIR` are default values which
are overridden by any earlier setting.  The `foo_mk` can only be
overridden by the environment or command-line.  

The `all_component_mk_files` variable tracks the ESMF makefile
fragments the `NEMS/src/GNUmakefile` uses to build the NEMS-specific
source code and link the NEMS executable.

The `component_FOO.mk` must also have these build rules:

    build_FOO: $(foo_mk)

    $(foo_mk): configure
    	... rules for building the FOO component ...

    clean_FOO: 
    	... rules for cleaning the FOO intermediate files ...

    distclean_FOO: clean_FOO
    	rm -rf $(FOO_BINDIR) $(foo_mk)
    	... delete any other FOO targets ...

You must be aware of some of the details of how submakes work, and the
interactions between makefile fragments, before attempting to add or
modify a component.  The `NEMS/src/incmake/example_component_FOO.mk`
explains these issues in more detail.







### The `modules.nems`

Some systems have the unix `module` command.  On those systems, the
NEMS `modulefiles` must follow the standard `modulefile` syntax.  On
systems without the `module` command, the `modules.nems` file is
simply a POSIX sh include file.

\info On systems with the `module` command, the `modules.nems` must be
a valid modulefile.  On systems without the `module` command
(`USE_MODULES=NO`), the `modules.nems` is simply a POSIX sh include
script.

Modulefiles must begin with this line:

    #%Module######################################################################

Other lines specify the modules to load.  Here is the full modulefile
for one of the GFS apps on WCOSS:

    #%Module######################################################################
    # This script is responsible for loading modules that are compatible with
    # the NUOPC Layer version used in NEMS.
    
    module load  EnvVars/1.0.0
    module load ibmpe lsf NetCDF/4.2/serial ESMF/700
    module load ics/15.0.3

Note that this is not a shell script; it is tcl script.  You cannot
have any `source` or `export` commands.  It is best to stick with
these two commands if possible:

 * `module load module-name`
 * `module use /p/a/t/h`

The `modulefiles` can contain other commands, the most common of which
are:

 * `prepend-path /p/a/t/h`
 * `append-path /p/a/t/h`
 * `remove-path /p/a/t/h`
 * `setenv variable value`
 * `unsetenv variable`
 * `set variable value`
 * `unset variable`

There are multiple implementations of the `module` command, some of
which have more powerful features.  On some platforms, you may have to
use the more advanced features in order to properly set up the
environment.  That is why NEMS uses a different modulefile for each
platform.

No matter what you do, you must follow this critical rule:

\warning Never put a `module purge` command in a `modulefile`.

Placing a `module purge` in a `modulefile` will cause infinite loops,
corrupted environments, or segfaults of the `module` command on some
platforms.  The NEMS scripts already purge the modules by running a
"shell include" file before loading your `modulefile`.  This script
can be found in:

* `NEMS/src/conf/module-setup.sh.inc` (for bash, sh, and ksh)
* `NEMS/src/conf/module-setup.csh.inc` (for csh and tcsh)



### For More Information About `make` and `modulefiles`

Good general resources for learning about `modulefiles` are:

* NICS module website: http://www.nics.tennessee.edu/computing-resources/modules

*  The `module` command:

       module help # list allowed "module" commands
       module avail # list available modules
       module spider # also list modules in hidden families

   Note that the `module spider` command is only available on
   platforms that are able to hide modules that are unavailable
   without prerequisites.  For example, hiding a `NetCDF` library that
   was compiled with `gfortran` until the `gfortran` module is loaded.

For `makefiles`,

* GNU Make tutorial: http://opensourceforu.com/2012/06/gnu-make-in-detail-for-beginners/

* GNU Make manual: https://www.gnu.org/s/make/manual/make.html

* If you have a lot of time on your hands, a book: https://notendur.hi.is/jonasson/software/make-book/

The NEMS build system uses the GNU Make Standard Library, documented here:

* https://gmsl.sourceforge.io/





The NEMSAppBuilder
------------------

This was the main build system prior to the introduction of the pure
Make system.  There is still a script by that name, to ensure
backward-compatibility for apps that use the NEMSAppBuilder.  However,
it no longer has graphical build capabilities.  For those that require
a graphical build system, we suggest using an Integrated Development
Environment, such as Eclipse.

The syntax for running the NEMSAppBuilder is:

    ./NEMS/NEMSAppBuilder (options) app=(build-target)

Here, the `(build-target)` is the application build target, also known
as the "build," or "project."  It corresponds to a
`(build-target).appBuild` file at the top-level (app) checkout
directory.

The  `(options)` should be one of the following:

* `rebuild` --- clean the source directory before recompiling.

* `norebuild` --- do not clean; reuse existing libraries and object
    files whenever possible.

Using the new Make-based build system directly, a `rebuild` is
equivalent to:

    cd `NEMS`
    make app=(build-target) distclean
    make app=(build-target) build

while a `norebuild` is equivalent to:

    cd `NEMS`
    make app=(build-target) build

Contrary to prior versions of the NEMSAppBuilder, the script will not
enable a parallel build automatically.  To do a parallel build, one
must use the `-j` option to `make`.



Troubleshooting Failed Builds
-----------------------------

### Incomplete Checkout

When there are network problems or high server load, your clone of the
git repository may fail.  Unfortunately, with Git, there is no way to
resume a failed clone.  You must start over with a new clone.

### Unclean Environment

Setting up your environment incorrectly can lead to problems while
building.  If you see build issues from a clean, new checkout, this
may be the problem.  You should remove all `module` commands from your
`~/.*rc` files and get a clean, new login shell.  Then retry the
build.

### Unclean Checkout

Another common cause of failed builds is having unintended changes in
your source code or build system, or a corrupted repository checkout
or clone.  To test for this, get a clean, new clone of the repository
and retry.

### Unsupported Platform

Some apps only support a few platforms.  For example, the NEMSGSM app
is only supported on WCOSS Phase 1 (Gyre/Tide) and NOAA Theia.
Attempts to build on other platforms may or may not work.  Sometimes
the app will try, and fail, to build; other times it will not even
make the attempt.

### Simultaneous Builds

Attempting to build multiple times in the same NEMS checkout directory
will cause unexpected failures.  For example, if you are running the
regression test system twice at once, multiple builds will happen at
the same time.  On Theia, this frequently shows up as a massive, many
terabyte, file which cannot be created due to fileset quota limits.
Other failure modes are possible.











