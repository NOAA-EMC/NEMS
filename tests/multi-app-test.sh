#! /bin/sh

set -m # enable job control

if [[ "$#" != 4 && "$2" == resume_test ]] ; then
    echo "SYNTAX: cat apps.def commit.def | multi-app-test.sh platform resume_test AppName /path/to/app" 1>&2
    echo "When resuming a test, you must specify which test to resume." 1>&2
elif [[ "$#" != 3 && "$2" == master ]] ; then
    echo "SYNTAX: cat apps.def commit.def | multi-app-test.sh platform master /path/to/file/with/commit/message" 1>&2
    echo "When resuming a test, you must specify which test to resume." 1>&2
elif [[ "$#" != 2 && "$2" != master && "$2" != resume_test ]] ; then
    echo "SYNTAX: cat apps.def commit.def | multi-app-test.sh platform stage" 1>&2
    echo "    OR: cat apps.def commit.def | multi-app-test.sh platform resume_test /path/to/NEMS/tests" 1>&2
    echo "    OR: cat apps.def commit.def | multi-app-test.sh platform master /path/to/file/with/commit/message" 1>&2
    exit 2
fi

if ( test -t 0 ) ; then
    echo "ERROR: stdin must not be a terminal." 1>&2
    echo "SYNTAX: cat apps.def commit.def | multi-app-test.sh platform stage" 1>&2
    exit 2
fi

# Kill child processes on abnormal exit:
prolicide() {
    set +uex
    jobs=$( jobs -p )
    if [[ ! -z "$jobs" ]] ; then
        kill $jobs
    fi
    return 0
}
trap 'result="$?" ; if [[ "$result" != 0 ]] ; then prolicide ; fi ; exit $result' EXIT
trap 'prolicide ; exit 1' SIGINT SIGTERM SIGHUP SIGQUIT

platform="$1"
stage="$2"

# Jet CRON workaround:
if [[ -x /apps/local/bin/account_params && -d /lfs3 ]] ; then
    export PATH=$PATH:/apps/local/bin
fi

########################################################################

nonsense() {
    local which=$(( RANDOM % 12 ))
    case "$which" in
        0) echo "Script is aborting due to metaphysical dilemma." ;;
        1) echo "Rumination failure.  Check for false dichotomies." ;;
        2) echo "Aborting due to loss of ipseity." ;;
        3) echo "This process has been eaten by a grue." ;;
        4) echo "Check for gremlins." ;;
        5) echo "Brain-dump into sewer. Please retrieve." ;;
        6) echo "Kernel panic: no popcorn remains." ;;
        7) echo "Existential error: null pointer dereference." ;;
        8) echo "No clam found in shell.  Try again?" ;;
        9) echo "Sanity lost.  Please find." ;;
        10) echo "Operator failed.  Please replace." ;;
        *) echo "CPU elves are on a lunch break.  Check back later?" ;;
    esac
}

########################################################################

forbid_terminals() {
    local stage="$1"
    if ( test -t 1 -o -t 2 ) ; then
        nonsense 1>&2
        echo 1>&2
        echo "FATAL ERROR: this process is connected to a terminal." 1>&2
        echo "In stage \"$stage\" this script must not be connected to a terminal." 1>&2
        echo "Run in nohup, CRON, or a batch job." 1>&2
        echo 1>&2
        # Both git and svn can do odd things if they are connected to
        # a terminal when their process is backgrounded.
        exit 2
    fi
}

########################################################################

set_global() {
    # Allows setting a calculated variable name to a value.
    #   set_global variable_name value
    local var=$( echo "$1" | sed 's,[. -],_,g' )
    local value="$2"
    eval "$var=\"\$value\""
    #new_value=$( get_global "$var" )
    #echo "$var=$value=$new_value" 1>&2
}

have_global() {
    # Given a string, see if a variable by that name exists.
    #   have_global variable_name
    local var=$( echo "$1" | sed 's,[. -],_,g' )
    eval "[[ ! -z \"\$$var\" ]]"
}

get_global() {
    # Given a string, get the value of the variable with that name
    #   get_global variable_name
    local var=$( echo "$1" | sed 's,[. -],_,g' )
    local result=$( eval "echo \"\$$var\"" )
    #echo "GET $var=$result" 1>&2
    echo "$result"
}

get_global_or_default() {
    # Given a string, get the value of the variable with that name
    #   get_global variable_name
    local var=$( echo "$1" | sed 's,[. -],_,g' )
    local default="$2"
    #echo "GET $var OR $default" 1>&2
    eval "echo \"\${$var:-$default}\""
}

########################################################################

parse_control_file() {
    local context key command value
    local check_platform check_apps
    local result=0
    local lineno=0
    local eof=0
    platforms=""
    all_apps=""
    set +eux
    #set -x

    # Default values:
    user=MISSING
    role=MISSING
    nems_branch=default
    app_branch=MISSING
    master_branch=master
    more_rt_sh_args=

    while [[ $eof == 0 ]] ; do
        lineno=$(( lineno+1 ))
        read context key command value
        eof=$?
        check_platform_in_key=NO
        check_apps_in_value=NO
        check_app_in_key=NO
        if [[ -z "$context" ]] ; then
            continue # blank line
        elif [[ "${context:0:1}" == '#' ]] ; then
            #echo "line $lineno: comment: $context $key $command $value" 1>&2
            continue
        elif [[ -z "$value" ]] ; then
            result=1
            echo "line $lineno: invalid line; no value: $context $key $command $value" 1>&2
        elif [[ "$context" == PLATFORM && "$command" == NAME ]] ; then
            #echo "Platform $key has name $value"
            platforms="$platforms $key"
            set_global "platform_${key}_name" "$value"
        elif [[ "$context" == APP && "$command" == COMPSETS ]] ; then
            #echo "App $key shall run compsets specified by: $value"
            set_global "compsets_for_$key" "$value"
            all_apps="$all_apps $key"
            check_app_in_key=YES
        elif [[ "$context" == APP && "$command" == URL ]] ; then
            #echo "App $key URL is: $value"
            set_global "app_url_$key" "$value"
            check_app_in_key=YES
        elif [[ "$context" == APP && "$command" == CHECKOUT ]] ; then
            echo "App $key starting branch is: $value"
            set_global "starting_branch_for_$key" "$value"
            check_app_in_key=YES
        elif [[ "$context" == ON && "$command" == EXTRA_ARGS ]] ; then
            #echo "Platform $key uses extra args to NEMSCompsetRun: $value" 1>&2
            set_global "rt_sh_args_for_$key" "$value"
            check_platform_in_key=YES
        elif [[ "$context" == ON && "$command" == SCRUB ]] ; then
            #echo "Platform $key uses scrub space $value"
            set_global "scrub_space_for_$key" "$value"
            check_platform_in_key=YES
        elif [[ "$context" == ON && "$command" == APPS ]] ; then
            #echo "Platform $key will run apps $value"
            set_global "app_list_for_$key" "$value"
            check_platform_in_key=YES
            check_apps_in_value=YES
        elif [[ "$context $key $command" == "USER ACCOUNT IS" ]] ; then
            user="$value"
        elif [[ "$context $key $command" == "ROLE ACCOUNT IS" ]] ; then
            role="$value"
        elif [[ "$context $key $command" == "NEMS BRANCH IS" ]] ; then
            nems_branch="$value"
        elif [[ "$context $key $command" == "APP BRANCH IS" ]] ; then
            app_branch="$value"
        elif [[ "$context $key $command" == "MASTER BRANCH IS" ]] ; then
            master_branch="$value"
        elif [[ "$context" == ON && "$command" == WEBPAGE ]] ; then
            #echo "Platform $key sends to webpage $value"
            set_global "webpage_for_$key" "$value"
            check_platform=YES
            check_apps=YES
        else
            echo "line $lineno: invalid line: $context $key $command $value" 1>&2
            result=1
        fi

        if [[ "$check_platform_in_key" == YES ]] ; then
            if ( ! have_global "platform_${key}_name" ) ; then
                echo "line $lineno: unknown platform $key (no PLATFORM...NAME block for this)" 1>&2
                result=1
            fi
        fi
        if [[ "$check_app_in_key" == YES ]] ; then
            if ( ! have_global "compsets_for_$key" ) ; then
                echo "line $lineno: unknown app $key (no APP...COMPSETS line for this app)" 1>&2
                result=1
            fi
        fi
        if [[ "$check_apps_in_value" == YES ]] ; then
            for app in $value ; do
                if ( ! have_global "compsets_for_$app" ) ; then
                    echo "line $lineno: unknown app $app (no APP...COMPSETS line for this app)" 1>&2
                    result=1
                fi
            done
        fi
    done
    if [[ "$user" == MISSING ]] ; then
        echo "user: missing \"USER ACCOUNT IS\" line in stdin" 1>&2
        result=1
    fi
    if [[ "$role" == MISSING ]] ; then
        echo "role: missing \"ROLE ACCOUNT IS\" line in stdin" 1>&2
        result=1
    fi
    if [[ "$nems_branch" != default && "$app_branch" == MISSING ]] ; then
        echo "role: when a nems branch is specified, the app branch must also be specified.  Missing \"APP BRANCH IS\" line in stdin." 1>&2
        result=1
    fi
    if [[ "$master_branch" != master ]] ; then
        cat<<EOF 1>&2
WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING!

  The master branch is set to "$master_branch" instead of "master"
  This means the commits will NOT go to the git master of NEMS nor
  the git master of apps.  You probably do not want to do this!

WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING!
EOF
    fi
    if [[ -z "$platforms" ]] ; then
        nonsense 1>&2
        echo "No platforms defined!" 1>&2
        echo "Send the control file into stdin!" 1>&2
        exit 1
    fi
    if [[ "$result" != 0 ]] ; then
        nonsense 1>&2
        echo "ABORT: Errors in control file." 1>&2
        exit 1
    fi
    if ( ! have_global "platform_${platform}_name" ) ; then
        nonsense 1>&2
        echo "ERROR: Unknown platform \"$platform\" selected on command line." 1>&2
        echo "Known platforms: $platforms" 1>&2
        exit 1
    fi
}

########################################################################

# Accessors of data from the control file

get_app_url() {
    local app="$1"
    get_global_or_default "app_url_$app" "gerrit:$app"
}

get_app_test_set() {
    local app="$1"
    get_global "compsets_for_$app"
}

get_human_readable_name_for_platform() {
    local plat="${1:-$platform}"
    get_global "platform_${plat}_name"
}

get_scrub_for_user() {
    local username="$1"
    local expr=$( get_global "scrub_space_for_$platform" )
    eval "echo $expr"
}

get_app_list_for_platform() {
    local plat="${1:-$platform}"
    get_global "app_list_for_$plat"
}

get_webpage_for_platform() {
    local plat="${1:-$platform}"
    get_global_or_default "webpage_for_$plat" "none"
}

get_rt_sh_args() {
    local plat="${1:-$platform}"
    get_global_or_default "rt_sh_args_for_$plat" " "
}

get_app_starting_branch() {
    local app="$1"
    get_global_or_default "starting_branch_for_$app" "$master_branch"
}

get_platform_specific_variables() {
    webpage=$( get_webpage_for_platform )
    workuser=$( get_scrub_for_user "$user" )
    worknems=$( get_scrub_for_user "$role" )
    apps=$( get_app_list_for_platform )
    more_rt_sh_args=$( eval echo $( get_rt_sh_args ) )
    webuser=$( echo "$webpage" | awk 'BEGIN{FS=":"}{print $1}' )
    webbase=$( echo "$webpage" | awk 'BEGIN{FS=":"}{print $2}' )
    email_from=$user@noaa.gov
    email_to=$user@noaa.gov
}

########################################################################

dump_control_file() {
    local url tests starting_branch
    get_platform_specific_variables
    local human_platform=$( get_human_readable_name_for_platform )
    echo "DUMP OF CONTROL FILES FROM STDIN"
    echo
    echo "Known platforms: $platforms"
    echo "Known apps: $all_apps"
    echo
    echo "PLATFORM $human_platform"
    echo "  - key = $platform"
    echo "  - user $user work area = $workuser"
    echo "  - role $role work area = $worknems"
    echo "  - back-end website login = $webuser"
    echo "  - back-end website dir = $webbase"
    if [[ ! -z "$more_rt_sh_args" ]] ; then
        echo "  - extra args to NEMSCompsetRun = $more_rt_sh_args"
    fi
    echo
    echo "BRANCHES:"
    if [[ "$nems_branch" == default ]] ; then
        echo "  - apps: master"
        echo "  - nems: Use each app's own NEMS."
    else
        echo "  - apps: $app_branch"
        echo "  - nems: $nems_branch"
        if [[ "$master_branch" != master ]] ; then
            echo "  - push to master: $master_branch"
        fi
    fi
    echo
    echo "APPS TO RUN: $apps"
    for app in $apps ; do
        url=$( get_app_url $app )
        tests=$( get_app_test_set $app )
        echo "  - $app ($url) will run $tests"
    done
    echo
    echo "BRANCHING INFO: $all_apps"
    for app in $all_apps ; do
        url=$( get_app_url $app )
        starting_branch=$( get_app_starting_branch $app )
        if [[ "$starting_branch" != "$master_branch" ]] ; then
            echo "  - $app ($url) checkout $starting_branch push to $app_branch"
        else
            echo "  - $app ($url) push to $app_branch"
        fi
    done
}

########################################################################

mkdir_p_workaround() {
    # If multiple threads "mkdir -p" a directory at the same time,
    # then all but one thread will fail.
    local dir="$1"
    local x
    for x in $( seq 1 10 ) ; do
        ( set +e ; mkdir -p "$dir" ; exit 0 )
        if [[ -d "$dir" ]] ; then
            echo "mkdir -p \"$1\": success after $x tries" 1>&2
            return 0
        fi
    done
    echo "mkdir -p \"$1\": gave up after $x tries" 1>&2
    return 1
}

########################################################################

require_a_nems_branch() {
    if [[ "$nems_branch" == default ]] ; then
        nonsense 1>&2
        echo "NEMS commit process is disabled.  Running in Nightly Test mode." 1>&2
        echo "Will not make any branches." 1>&2
        exit 1
    fi
}

########################################################################

run_in_background_for_each_app() {
    local workdir="$1"
    local log_name="$2"
    local command_template="$3"
    local app_list_id="$4"
    local app_list
    local command

    if [[ "$app_list_id" == all_apps ]] ; then
        app_list="$all_apps"
    else
        app_list=$( get_app_list_for_platform )
    fi

    set -ue

    # Make a file that will hold exit statuses.
    local status_file="${TMPDIR:-/tmp}/${log_name}_status.$$.$RANDOM.$RANDOM"
    echo 0 > "$status_file"
    chmod 600 "$status_file"
    echo 0 > "$status_file"

    mkdir_p_workaround "$workdir/log"
    cd "$workdir/log"
    for app in $app_list ; do
        log="${app}_${platform}_${log_name}.log"
        if [[ "$nems_branch" != default ]] ; then
            log="$nems_branch-$log"
        fi
        command=$( eval "echo $command_template" )

        echo "$app: log $log"
        echo "$app: running $command"

        (   set +xue ;
            ( $command > "$log" 2>&1 ) ;
            result=$? ;
            echo "$result" >> "$status_file" ;
            echo "$app: exit $result" 1>&2 ) &
    done
    wait

    local status
    local max_status=0
    for status in $( cat "$status_file" ) ; do
        if [[ "$status" -gt 0 && ( "$status" -gt "$max_status" || "$max_status" == 0 ) ]] ; then
            max_status=status
        fi
    done

    rm -f "$status_file"

    if [[ "$max_status" != 0 ]] ; then
        echo "FAILURE: $log_name: non-zero exit status $max_status!" 1>&2
        return 1
    fi
    echo "Zero exit status for all jobs."
    return 0
}

########################################################################

generate_hash_for_test_name() {
    local purpose="$1"
    local name="$2"
    echo "$purpose"-$( echo "$name" | md5sum | cut -c1-8 )
}

########################################################################

repeatedly_try_to_run() {
    local tries scale naptime success
    local max_tries=7
    success=NO
    for tries in $( seq 1 $max_tries ) ; do
        if ( "$@" ) ; then
            return 0
        fi

        # We sleep a random time after pulling to reduce the chances
        # of many simultaneous pushes.
        if [[ "$tries" -lt 11 ]] ; then
            scale=$(( 2**tries / tries + tries))
        else
            scale=180
        fi
        if [[ "$scale" -lt 3 ]] ; then
            $scale=3
        fi
        naptime=$(( 1 + $RANDOM%$scale ))
        echo "Sleep $naptime..."
        sleep $naptime
    done

    echo "$*: failed after $tries tries" 1>&2
    return 1
}

########################################################################

repeatedly_try_to_push() {
    # Pull and push until we succeed in pushing:
    local tries scale naptime success
    local max_tries=7
    success=NO
    for tries in $( seq 1 $max_tries ) ; do
        git pull "$@" || true

        # We sleep a random time after pulling to reduce the chances
        # of many simultaneous pushes.
        if [[ "$tries" -lt 11 ]] ; then
            scale=$(( 2**tries / tries + tries))
        else
            scale=180
        fi
        if [[ "$scale" -lt 3 ]] ; then
            $scale=3
        fi
        naptime=$(( 1 + $RANDOM%$scale ))
        echo "Sleep $naptime..."
        sleep $naptime

        if ( git push "$@" ) ; then
            success=YES
            echo "Push success at try #$tries"
            break
        fi
    done
    if [[ "$success" != YES ]] ; then
        echo "Unable to push to repository after $tries tries." 1>&2
        return 1
    fi
    return 0
}

########################################################################

make_branch() {
    require_a_nems_branch
    echo "MAKE BRANCH $*"
    set -xe
    local app="$1"
    local unique_id="$2"
    local delete="$3"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$app" )
    if [[ "$?" != 0 || -z "$app_url" ]] ; then
        echo "CANNOT CHECKOUT UNKNOWN APP $app" 1>&2
        return 1
    fi
    local app_starting_branch=$( get_app_starting_branch "$app" )

    local workdir="$workuser/branch_${nems_branch}_$unique_id"
    mkdir_p_workaround "$workdir"
    cd $workdir

    local test_hash=$( generate_hash_for_test_name branch "$test_name.$$.$RANDOM" )

    rm -rf "$test_hash"
    repeatedly_try_to_run git clone "$app_url" "$test_hash"
    cd "$test_hash"

    # Make sure the branch does not exist before we create it.
    if [[ "$delete" == DELETE ]] ; then
        # We are being instructed to delete the branch if it exists.
        git push origin --delete "$app_branch" || true
    elif ( git branch -a | grep "origin/$app_branch" ) ; then
        echo "$app_branch: already exists" 1>&2
        exit 9
    fi

    git checkout "$app_starting_branch"
    git checkout -b "$app_branch"
    repeatedly_try_to_run git submodule update --init --recursive
    cd NEMS
    git fetch
    git checkout "$nems_branch"
    cd ..
    git add NEMS
    git commit -m "Check out NEMS $nems_branch"
    repeatedly_try_to_push origin "$app_branch"
    git branch --set-upstream-to=origin/"$app_branch" "$app_branch" || \
        git branch --set-upstream "$app_branch" origin/"$app_branch"
    cd ..
    rm -rf "$test_hash"
}

########################################################################

delete_branch() {
    require_a_nems_branch
    echo "DELETE BRANCH $*"
    set -xe
    local app="$1"
    local unique_id="$2"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$app" )
    if [[ "$?" != 0 || -z "$app_url" ]] ; then
        echo "CANNOT CHECKOUT UNKNOWN APP $app" 1>&2
        return 1
    fi
    local app_starting_branch=$( get_app_starting_branch "$app" )

    local workdir="$workuser/delete_${nems_branch}_$unique_id"
    mkdir_p_workaround "$workdir"
    cd $workdir

    local test_hash=$( generate_hash_for_test_name branch "$test_name.$$.$RANDOM" )

    rm -rf "$test_hash"
    repeatedly_try_to_run git clone "$app_url" "$test_hash"
    cd "$test_hash"

    git push origin --delete "$app_branch" || true
    cd ..
    rm -rf "$test_hash"
}

########################################################################

checkout_app() {
    # Checks out an app under the user and makes a flag file so the
    # nemsuser knows where the checkout resides.
    set -xe
    local app="$1"
    local unique_id="$2"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$1" )
    if [[ "$?" != 0 || -z "$app_url" ]] ; then
        echo "CANNOT CHECKOUT UNKNOWN APP $1" 1>&2
        return 1
    fi

    local test_hash=$( generate_hash_for_test_name checkout "$test_name.$$.$RANDOM" )

    mkdir_p_workaround "$workuser"
    cd $workuser
    rm -rf "$test_hash"

    if ( echo "$app_url" | grep -E 'gerrit|git' > /dev/null 2>&1 ) ; then
        which git
        rm -rf "$test_hash"
        repeatedly_try_to_run git clone "$app_url" "$test_hash"
    else
        which svn
        svn co "$app_url" "$test_hash"
    fi

    cd "$test_hash"

    if [[ "$nems_branch" != default ]] ; then
        git checkout "$app_branch"
        git branch --set-upstream-to=origin/"$app_branch" "$app_branch" || \
            git branch --set-upstream "$app_branch" origin/"$app_branch"
    fi
    
    repeatedly_try_to_run git submodule update --init --recursive

    if [[ -x checkout.sh ]] ; then
        ./checkout.sh "$USER"
    fi

    chmod -R go=u-w .

    echo $( date +%s ) $unique_id $( pwd ) > $workuser/$test_name.for.nemspara
}

########################################################################

test_app() {
    # Obtains the repo checked out by the user, and runs it.  Creates
    # a flag file so the next stages know how to find the results.
    set -x
    local app="$1"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$1" )
    local set=$( get_app_test_set "$1" )
    if [[ "$?" != 0 ]] ; then
        echo "CANNOT TEST UNKNOWN APP $1" 1>&2
        return 1
    fi
    set="${set:--f}"

    local maxwait=1800
    local flagfile="$workuser/$test_name".for.nemspara
    local startdate=$( date )
    if [[ -f "$HOME/nems-test-email-lists/$test_name" ]] ; then
        email_to=$( echo $( cat "$HOME/nems-test-email-lists/$test_name" ) )
    fi

    mkdir_p_workaround "$worknems"

    local scriptstart=$( date +%s )

    local waitstart=$scriptstart
    local waittime=0

    while [[ ! -s "$flagfile" && $waittime -lt "$maxwait" ]] ; do
        echo "$flagfile: empty or does not exist; sleep 30"
        sleep 30
        local nowtime=$( date +%s )
        waittime=$(( nowtime - waitstart ))
    done
    if [[ ! -s "$flagfile" ]] ; then
        nonsense 1>&2
        echo "$flagfile: gave up waiting after $maxwait seconds."
        exit 1
    fi

    sleep 15

    local timestamp=$( head -1 "$flagfile" | awk '{print $1}' )
    local unique_id=$( head -1 "$flagfile" | awk '{print $2}' )
    local checkout=$( head -1 "$flagfile" | awk '{print $3}' )

    set +e
    echo "timestamp=$timestamp"
    echo "unique_id=$unique_id"
    echo "checkout=$checkout"
    set -e

    if (( scriptstart - timestamp > 3600*24 )) ; then
        nonsense 1>&2
        echo "$flagfile: timestamp is more than one day old.  Check CRON."
        exit 1
    fi

    if [[ ! -d "$checkout" ]] ; then
        nonsense 1>&2
        echo "$checkout: not a directory or does not exist.  Check logs."
        exit 1
    fi

    local test_hash=$( generate_hash_for_test_name test "$test_name.$$.$RANDOM" )

    local workarea="$worknems/$test_hash"
    cd /
    if ( ! rm -rf "$workarea" ) ; then
        sleep 10
        rm -rf "$workarea" || true
    fi
    mkdir_p_workaround "$workarea"
    cd "$workarea"
    set +e
    rsync -arv "$checkout/." . || true
    rsync -arv "$checkout/." .

    set +eu
    if [[ -d .svn ]] ; then
        svn info . > repo.info
        echo >> repo.info
        svn propget svn:externals . | while read dir rest ; do
            echo "$dir" >> repo.info
            svn info "$dir" >> repo.info
        done
    else
        (
            echo REPO TOP:
            git branch -vv | head -1 | cut -c3-  ;
            git remote show -n origin | grep 'Fetch URL:' | sed "s,^ *,,g" ;
            git status -suno ;
            echo ;
            git submodule foreach 'sh -c '\''git branch -vv | head -1 | cut -c3- ; git remote show -n origin | grep "Fetch URL:" | sed "s,^ *,,g" ; git status -suno ; echo'\' ;
        ) > repo.info
    fi
    set -eu
    local repo_info_file="$( pwd -P )/repo.info"
    test -s "$repo_info_file"

    set +ue

    env > env.out
    module list > module-list.out 2>&1

    rm -f rt-f.log # "$workarea"/log/report*log/*
    echo "WILL RUN NEMSCompsetRun FOR SET \"$set\""
    echo "  IN DIR $( pwd -P )"
    echo "  LOGGING TO $( pwd )/rt-f.log"
    echo "  REPO INFO AT $repo_info_file"
    echo "This may take a while..."

    ./NEMS/NEMSCompsetRun --multi-app-test-mode \
        $set $more_rt_sh_args > rt-f.log 2>&1

    tail -20 rt-f.log

    local status="$?"
    cd NEMS/tests

    echo "$( date +%s ) $( pwd )" > $worknems/$test_name.for.sam
}

########################################################################

resume_test() {
    set -x
    local workarea="$1"
    local app="$2"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$1" )
    local set=$( get_app_test_set "$1" )
    if [[ "$?" != 0 ]] ; then
        echo "CANNOT TEST UNKNOWN APP $1" 1>&2
        return 1
    fi
    set="${set:--f}"

    set -e
    test -d "$workarea"
    cd "$workarea"
    cd ../../

    rm -f rt-f.log
    set +x
    echo "WILL RUN NEMSCompsetRun FOR SET \"$set\""
    echo "  IN DIR $( pwd -P )"
    echo "  LOGGING TO $( pwd )/rt-f.log"
    echo "  REPO INFO AT $repo_info_file"
    echo "This may take a while..."
    set -x

    ./NEMS/NEMSCompsetRun --multi-app-test-mode \
        $set $more_rt_sh_args > rt-f.log 2>&1

    tail -20 rt-f.log

    local status="$?"
    cd NEMS/tests

    echo "$( date +%s ) $( pwd )" > $worknems/$test_name.for.sam
}

########################################################################

generate_regtest_txt_and_send_email() {
    set -x
    local test_name="$1"

    local worktest=$( cat "$worknems/$test_name.for.sam" | awk '{print $2}' )
    local workarea=$( cd "$worktest" ; cd ../.. ; pwd -P )
    local rt_f_log="$workarea/rt-f.log"
    local startdate=$( stat -c %z "$rt_f_log" )

    local RUNDIR=''
    local PLATFORM_NAME=''
    vars=$( cat "$rt_f_log" | grep -E 'RUNDIR.*PLATFORM' )
    eval $vars

    if [[ -z "$PLATFORM_NAME" || -z "$RUNDIR" ]] ; then
    # New scripts
        local REPORT_DIR=$( cat "$rt_f_log" | perl -ne 'chomp; /copy build logs to (\S+)/ and do { print $1; exit }' )
        local RTREPORT="$REPORT_DIR/rtreport.txt"
        echo "New scripts.  Report dir = $REPORT_DIR"
        echo "New scripts.  Report txt = $RTREPORT"
    else
    # Old scripts
        local REPORT_DIR="$workarea/log/report-$PLATFORM_NAME-log"
        local RTREPORT="$REPORT_DIR/rtreport.txt"
        echo "Old scripts.  Report dir = $REPORT_DIR"
        echo "Old scripts.  Report txt = $RTREPORT"
        echo "Old scripts.  Platform   = $PLATFORM_NAME"
        echo "Old scripts.  Run dir    = $RUNDIR"
    fi

    local result
    local build
    if [[ ! -s "$RTREPORT" ]] ; then
        echo RTREPORT DOES NOT EXIST: $RTREPORT
        result=ABORT
    elif ( grep 'REGRESSION TEST FAILED' < $RTREPORT ) ; then
        result=FAIL
    elif ( grep 'REGRESSION TEST WAS SUCCESSFUL' < $RTREPORT ) ; then
        result=PASS
    else
        result=ABORT
    fi

    REGRESSION_TEST_RESULT=$result

    if [[ "$result" == PASS ]] ; then
        echo "Success; will not email."
        return 0
    fi

    (
        echo "Automated NEMS regression test run at $startdate."
        echo "Test result: $result."
        echo "Completed at: $( date )."
        echo "Main log: $REPORT_DIR/rtreport.txt"
        for buildfile in "$REPORT_DIR/build"*log ; do
            build=$( basename $buildfile | sed 's,build_,,g' | sed 's,\.x\.log,,g' )
            echo "Compile log for $build: $buildfile"
        done
        echo
        echo "========================================================================"
        echo "Subversion information:"
        cat "$repo_info_file"
        echo
        echo "========================================================================"
   echo "Regression test log:"
   cat $RTREPORT
    ) | /bin/true mail \
        -s "Regression test result: $result" \
        -r "$email_from" \
        "$email_to"
    set +x
}

########################################################################

push_logs_to_branch() {
    set -x
    local app="$1"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$1" )
    if [[ "$?" != 0 ]] ; then
        echo "CANNOT PUSH LOGS TO UNKNOWN APP $1" 1>&2
        return 1
    fi

    set -eu

    # Find the directory in which nemspara ran the test:
    local nemsfile="$worknems/$test_name.for.sam"
    local testdir=$( head -1 "$nemsfile" | awk '{print $2}' )
    local test_time=$( head -1 "$nemsfile" | awk '{print $1}' )
    testdir=$testdir/../..
    test -d "$testdir/NEMS/tests"

    # Find the directory from which nemspara copied the repo:
    local flagfile="$workuser/$test_name".for.nemspara
    local checkout=$( head -1 "$flagfile" | awk '{print $3}' )
    test -d "$checkout/NEMS/tests"

    # Determine if the push has already happened
    local push_flag_file="$workuser/$test_name".pushed
    if [[ -s "$push_flag_file" ]] ; then
        # A push happened.  Did it happen after the test?
        set +ue
        local push_time=$( head -1 "$push_flag_file" | awk '{print $1}' )
        if [[ "$push_time" -gt "$test_time" ]] ; then
            echo "Push already done for this test of this repo."
            exit 0
        fi
    fi

    # Update the checkout directory with nemspara's logs:
    cd "$checkout"
    test -d NEMS/tests
    rsync --exclude '*.o' --exclude '*.a' --exclude '*.mod' --exclude '*.exe' \
        -arv "$testdir/." .

    git add log
    git commit -m "Commit logs for $platform testing NEMS $nems_branch"
    repeatedly_try_to_push

    echo "$( date +%s ) $app $( pwd )" > "$push_flag_file"
}

########################################################################

deliver_test_to_web_server() {
    # Finds the nemsuser's test and copies the result to the website.
    set -x
    local app="$1"
    local test_name="${app}_on_${platform}"
    if [[ "$nems_branch" != default ]] ; then
        test_name="$nems_branch-${test_name}"
    fi
    local app_url=$( get_app_url "$1" )
    if [[ "$?" != 0 ]] ; then
        echo "CANNOT DELIVER TEST FOR UNKNOWN APP $1" 1>&2
        return 1
    fi

    set -eu

    REGRESSION_TEST_RESULT=ABORT
    generate_regtest_txt_and_send_email "$test_name"
    
    local human_platform=$( get_human_readable_name_for_platform "$platform")
    local human_name="$human_platform $app"
    local javascript_name="$app/$platform"

    local rtset='full'

    umask 077 # Make sure only I can access my web directory.

    local nemsfile="$worknems/$test_name.for.sam"

    local timestamp=$( head -1 "$nemsfile" | awk '{print $1}' )
    local result=$REGRESSION_TEST_RESULT
    local testdir=$( head -1 "$nemsfile" | awk '{print $2}' )
    test -d "$testdir"
    local rtflog=$testdir/../../rt-f.log

    ########################################################################
    
    # Get further info from log files:
    local RUNDIR=''
    local PLATFORM_NAME=''
    set +eu
    local vars=$( cat "$testdir/../../rt-f.log" | grep -E 'RUNDIR.*PLATFORM' )
    eval "$vars"

    local LOGDIR
    local RTREPORT

    # New scripts
    LOGDIR=$( cat $rtflog | perl -ne 'chomp; /copy build logs to (\S+)/ and do { print $1; exit }' )
    RTREPORT="$LOGDIR/rtreport.txt"
    echo "NEW SCRIPTS: $LOGDIR $RTREPORT"
    local repo_info_file="$LOGDIR/../../repo.info"
    set -eu
    test -d "$LOGDIR"
    test -s "$RTREPORT"

    local REPORT_TIME=-1
    local START_TIME=-1
    #WORKFLOW REPORT AT %s (+%d)
    eval $( cat "$RTREPORT" | perl -ne '
if(m:WORKFLOW REPORT AT.*\(\+(\d+)\):) {
   print "REPORT_TIME=$1 ";
} elsif(m:WORKFLOW STARTED AT.*\(\+(\d+)\):) {
   print "START_TIME=$1 ";
};
END {
  print "\n";
}' )

    test -s "$repo_info_file"

    ########################################################################

    # Make the local version of the web directory:

    local webdir_pre="$workuser/webdir.$test_name.$( date +%s )."
    local webdir="$workuser/webdir.$test_name.$( date +%s ).$$"
    rm -rf "$webdir_pre"*
    mkdir_p_workaround "$webdir"
    cd "$webdir"

    ########################################################################

    # Generate regtest.txt:
    (
        set -xue
        echo "===!REGTEST BEGIN +$START_TIME"
        echo "===!REGTEST PLATFORM $human_name"
        echo "===!REGTEST RESULT $result"
        echo "===!REGTEST REPO BEGIN"
        grep -vE '^===!REGTEST' "$repo_info_file"
        echo "===!REGTEST REPO END"
        echo "===!REGTEST LOG BEGIN"
        grep -vE '^===!REGTEST' "$RTREPORT"
        echo "===!REGTEST LOG END"
        for buildfile in "$LOGDIR/build"*log ; do
            build=$( basename $buildfile | sed 's,build_,,g' | sed 's,\.log,,g' )
            echo "===!REGTEST COMPILE $build"
            grep -vE '^===!REGTEST' "$buildfile"
            echo "===!REGTEST COMPILE END"
        done
        echo "===!REGTEST END +$REPORT_TIME"
    ) | perl -ne 's:[^ -~\t\r\n]:_:gms ; print' > "regtest.txt"

########################################################################

    # Generate index.html

    cat<<EOF > index.html
<html><head>
  <title>$human_name Regression Tests</title>
  <link rel="stylesheet" href="../../regtestview.css" type="text/css"/>
  <script src="../../regtestview.js"></script>
</head>
<body>
<div id="content">

</div>
<noscript>
<h1>Turn on Javascript Support (3).</h1>

<p>It appears you have Javascript blocked for this website or turned
off entirely.  In order to use the automatic regression test viewer,
you must enable javascript.  If you want to view the original
regression test text files instead, they are embedded in the <a
href="regtest.txt">regtest.txt</a> in this directory.  You will find
the Javascript-based viewer more user-friendly than viewing that file
directly.  Please turn on Javascript for this webpage.</p>
</noscript>
</body>
</html>
EOF

    ########################################################################

    # Copy to website

    set +e
    ssh $webuser mkdir -p "$webbase/$javascript_name"
    set -e
    
    sleep 2
    
    rsync -arv -e ssh . "$webuser:$webbase/$javascript_name/."
    
    sleep 2
    
    if ( ! ssh $webuser chmod -R go=u-w "$webbase/$javascript_name/." ) ; then
        sleep 2
        ssh $webuser chmod -R go=u-w "$webbase/$javascript_name/."
    fi
}

########################################################################

copy_static_files_to_website() {
    local platform app
    set -xue
    cd $( dirname "$0" )
    cd web-top

    local tempfile="temp-regtestlist.js.temp"

    local test_name='NEMS Nightly Regression Tests'
    if [[ "$nems_branch" != default ]] ; then
        test_name="Test of NEMS $nems_branch"
    fi

    set +x
    cat<<EOF >> "$tempfile"
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// THIS SCRIPT IS AUTOMATICALLY GENERATED.  ALL EDITS WILL BE LOST!
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

// You need to edit the app list and regtestlist.js.in instead,
// and rerun the multi-app-test.sh script.

TEST_NAME="$test_name";

// TEST_DIRS is automatically generated from the app list:
TEST_DIRS=[
EOF
    local first=YES
    for platform in $platforms ; do
        for app in $( get_app_list_for_platform "$platform" ) ; do
            if [[ "$first" == YES ]] ; then
                first=NO
            else
                echo , >> "$tempfile"
            fi
            echo -n "  '$app/$platform'" >> "$tempfile"
        done
    done
    cat<<EOF >> "$tempfile"
];

// The rest of this file is copied from regtestlist.js.in:

EOF
    cat regtestlist.js.in >> "$tempfile"
    set -x
    mv -f "$tempfile" regtestlist.js

    ssh $webuser mkdir -p $webbase
    rsync --exclude 'temp*temp' -arv . $webuser:$webbase/.

}

########################################################################

commit_if_needed() {
    local commit_file="$1"
    local result
    set +e
    git commit -F "$commit_file" 2>&1 | tee errfile.$$
    result="$?"
    set -e
    if [[ "$result" != 0 ]] ; then
        if ( grep -iE 'nothing.*commit' errfile.$$ ) ; then
            return 0
        fi
        return 1
    fi
}

########################################################################

push_to_master() {
    require_a_nems_branch
    set -xue
    local commit_file="$1"
    local workspace="$workuser/push_${app_branch}_to_${master_branch}.$$.$RANDOM"
    local app

    mkdir -p "$workspace"
    cd "$workspace"

    # Make sure the commit file exists, is non-empty, and is readable:
    test -s "$commit_file"
    head "$commit_file"

    # Make sure all apps have urls:
    for app in $all_apps ; do
        get_app_url $app
    done

    # Check out $master_branch and $app_branch of each app:
    for app in $all_apps ; do
        git clone $( get_app_url $app ) "$app"
        cd "$app"
        # Make sure the $master_branch exists:
        git checkout "$master_branch"
        # Now get the $app_branch, which is what we want to use next:
        git checkout "$app_branch"
        cd ..
    done

    # Check out $master_branch of NEMS:
    git clone gerrit:NEMS NEMS
    cd NEMS
    git checkout "$master_branch"

    # Merge, commit, and push the $nems_branch:
    git merge --squash "origin/$nems_branch"
    commit_if_needed "$commit_file"
    repeatedly_try_to_push
    cd ..
    
    # Update each app to point to the nems $master_branch and push:
    for app in $all_apps ; do
        cd "$app"

        # Update to NEMS $master_branch and push that change:
        cd NEMS
        rsync -arv ../../NEMS/. .
        cd ..
        git add NEMS
        echo "Point to head of NEMS $master_branch." > point-to-head.txt
        commit_if_needed point-to-head.txt
        repeatedly_try_to_push

        # Merge $app_branch to $master_branch and push:
        git checkout "$master_branch"
        git merge --squash "$app_branch"
        commit_if_needed "$commit_file"
        repeatedly_try_to_push

        cd ..
    done

    echo "Rejoice!  Commit has been successful!"
}

########################################################################

parse_control_file
get_platform_specific_variables

case "$stage" in
    dump)
        dump_control_file
        exit $?
        ;;
    make_branches)
        forbid_terminals $stage
        global_unique_id=$$.$RANDOM
        run_in_background_for_each_app "$workuser" make_branches \
            'make_branch "$app" "$global_unique_id"' all_apps
        ;;
    checkout_all_apps)
        forbid_terminals $stage
        global_unique_id=$$.$RANDOM
        run_in_background_for_each_app "$workuser" checkout      \
            'checkout_app "$app" "$global_unique_id"' all_apps
        ;;
    delete_branches)
        forbid_terminals $stage
        global_unique_id=$$.$RANDOM
        run_in_background_for_each_app "$workuser" delete_branches \
            'delete_branch "$app" "$global_unique_id"' all_apps
        ;;
    delete_and_make_branches)
        forbid_terminals $stage
        global_unique_id=$$.$RANDOM
        run_in_background_for_each_app "$workuser" make_branches \
            'make_branch "$app" "$global_unique_id" DELETE' all_apps
        ;;
    resume_test)
        app_argument="$3"
        path_argument="$4"
        resume_test "$path_argument" "$app_argument"
        ;;
    web_init)
        copy_static_files_to_website
        exit $?
        ;;
    checkout)
        forbid_terminals $stage
        global_unique_id=$$.$RANDOM
        run_in_background_for_each_app "$workuser" checkout      \
            'checkout_app "$app" "$global_unique_id"' platform_apps
        ;;
    test)
        run_in_background_for_each_app "$worknems" test          \
            'test_app "$app"' platform_apps
        ;;
    push)
        forbid_terminals $stage
        require_a_nems_branch
        run_in_background_for_each_app "$workuser" push          \
            'push_logs_to_branch "$app"' platform_apps
        ;;
    deliver)
        forbid_terminals $stage
        run_in_background_for_each_app "$workuser" deliver       \
            'deliver_test_to_web_server "$app"' platform_apps
        ;;
    master)
        #forbid_terminals $stage # do not need this because we are not backgrounding
        require_a_nems_branch
        set -x
        push_to_master "$3"
        ;;
    *)
        nonsense 1>&2
        cat<<EOF 1>&2
UNKNOWN STAGE $stage
  make_branches - create a branch for this test in each app's repository
                  abort if the branch already exists
  delete_and_make_branches - same as make_branches, but delete the branch first
  web_init - copy static files to website
  checkout - check out all apps that will run on this platform
  test - run in the role account to execute the tests
  push - push the logs to the branch made in the make_branches step
  deliver - deliver results to website
  master - push changes to the master of NEMS and all apps.
  dump - print parsed contents of the definition files (stdin)

ABORTING: unknown stage $stage
EOF
        exit 1
esac
