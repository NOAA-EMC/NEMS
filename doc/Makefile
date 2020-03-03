LOCAL_DOCS=$(wildcard *.md) $(wildcard *.dox)
CONTROL_FILES=Makefile prep_inputs.py Doxyfile.IN DoxygenLayout.xml.IN


# Find Doxygen

ifndef DOXYGEN
DOXYGEN=doxygen
endif

ifeq ("$(DOXYGEN)","doxygen")
  $(info Attempting to auto-detect environment.)
  THEIA_DOXYGEN=/scratch3/NCEPDEV/hwrf/save/Samuel.Trahan/doxygen-1.8.10/bin/doxygen
  JET_DOXYGEN=/contrib/doxygen/1.8.10/bin/doxygen
  LUNA_DOXYGEN=/gpfs/hps3/emc/hwrf/noscrub/soft/doxygen-1.8.10/bin/doxygen
  TIDE_DOXYGEN=/hwrf/noscrub/soft/doxygen-1.8.10/bin/doxygen
  HERA_DOXYGEN=/usr/bin/doxygen

  ifneq ($(wildcard $(THEIA_DOXYGEN)*),)
    $(info On Theia.)
    DOXYGEN=$(THEIA_DOXYGEN)
    TARGET ?= samuel.trahan@dmzgw.ncep.noaa.gov:/home/www/emc/htdocs/projects/nems-sample/.
  else ifneq ($(wildcard $(JET_DOXYGEN)*),)
    $(info On Jet.)
    DOXYGEN=$(JET_DOXYGEN)
    TARGET ?= samuel.trahan@dmzgw.ncep.noaa.gov:/home/www/emc/htdocs/projects/nems-sample/.
  else ifneq ($(wildcard $(LUNA_DOXYGEN)*),)
    $(info on Luna or Surge.)
    DOXYGEN=$(LUNA_DOXYGEN)
    TARGET ?= rvasic@emcrzdm.ncep.noaa.gov:/home/www/emc/htdocs/mmb/rvasic/nems-sample/.
  else ifneq ($(wildcard $(TIDE_DOXYGEN)*),)
    $(info on Tide or Gyre.)
    DOXYGEN=$(TIDE_DOXYGEN)
    TARGET ?= rvasic@emcrzdm.ncep.noaa.gov:/home/www/emc/htdocs/mmb/rvasic/nems-sample/.
  else ifneq ($(wildcard $(HERA_DOXYGEN)*),)
    $(info on Local machine.)
    DOXYGEN=$(HERA_DOXYGEN)
  else
    $(error Could not detect environment.  You must set TARGET and DOXYGEN manually)
  endif
else
  $(info DOXYGEN set in environment.  Will not set TARGET variable.  You must set it manually.  DOXYGEN=$(DOXYGEN))
endif

TARGET ?= /please/update/TARGET/in/Makefile

default:
	@echo Specify build mode:
	@echo doc        = build documentation just for this directory
	@echo clean      = delete outputs
	@echo deliver    = copy to website
	exit 19

clean:
	rm -rf html webpage.tar.gz repo_info.sh.inc Doxyfile DoxygenLayout.xml

doc: clean
	./prep_inputs.py ..
	set -x ; $(DOXYGEN)
	tar -cpzf webpage.tar.gz html Doxyfile

deliver: html
	cd html && rsync -arv . "$(TARGET)/."

