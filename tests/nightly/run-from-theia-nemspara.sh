#! /bin/bash --login
cd $( dirname "$0" )
cd ..
source ../src/conf/module-setup.sh.inc
if [[ "$1" == "-Mslurm" ]] ; then
    set +xue
    echo module load slurm
    module load slurm
    shift
fi
cat apps.def nightly/nightly.def | ./multi-app-test.sh "$@"

