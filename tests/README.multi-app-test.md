Multi-App Test Instructions
===========================

Running the Commit Process
--------------------------

Edit the `commit.def` file, commit, and push it to your NEMS branch.

On each machine `$machine`:

    git clone gerrit:NEMS multi-app-test
    cd multi-app-test
    git checkout (your-nems-branch-goes-here)
    cd tests

On one machine:

    cat apps.def control.def | ./multi-app-test.sh jet web_init
    cat apps.def control.def | ./multi-app-test.sh jet make_branches

On each machine `$machine`, run as yourself:

    cat apps.def control.def | ./multi-app-test.sh $machine checkout

On each machine `$machine`, run under nemspara:

    cat apps.def control.def | ./multi-app-test.sh $machine test

On each machine `$machine`, run as yourself:

    cat apps.def control.def | ./multi-app-test.sh $machine push
    cat apps.def control.def | ./multi-app-test.sh $machine deliver

Then check the results, and make sure you are satisfied.  Next, run as yourself:

    cat apps.def control.def | ./multi-app-test.sh $machine master

Running the Nightly Tests
--------------------------

On each machine `$machine`:

    git clone gerrit:NEMS nightly-tests
    cd nightly-tests/tests

On one machine:

    cat apps.def nightly.def | ./multi-app-test.sh jet web_init

On each machine `$machine`, run as yourself:

    cat apps.def nightly.def | ./multi-app-test.sh $machine checkout

On each machine `$machine`, run under nemspara:

    cat apps.def nightly.def | ./multi-app-test.sh $machine test

On each machine `$machine`, run as yourself:

    cat apps.def nightly.def | ./multi-app-test.sh $machine deliver
