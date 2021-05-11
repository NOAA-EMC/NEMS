#! /usr/bin/env python

import re, subprocess, sys, getopt, datetime, glob, os

bad_flag=False # set to true if errors occur

NEMS_PROJECT_NAME='NEMS'
NEMS_MAIN_PAGE_MD='nemsmain.md'
APP_DOC_MAIN='doc/README.md'  # app-level main doc file relative to app-level checkout

def main():
    app_mode, nems_path, app_paths = scan_args(sys.argv[1:])

    # app_mode = True: we are running within an app checkout via "make app_doc"
    # app_mode = False: nems checkout via "make nems_doc"

    nems_rev, nems_loc = scan_nems(nems_path)
    app_info=[ list(scan_app(d)) for d in app_paths ]
    main_page=NEMS_MAIN_PAGE_MD

    # Determine project name (eg. "NEMS")
    # and project number (eg. "branches/update-docs@93510")
    if app_mode:
        if len(app_info)!=1:
            error('Running in app_doc mode but no app is present.\n')
            sys.exit(1)

        app0_readme=os.path.join(app_info[0],APP_README)
        if os.path.exists(app0_readme):
            main_page=app0_readme

        app_rev, app_name, app_loc = app_info[0]

        project_number=app_loc+'@'+app_rev
        project_name='NEMS App '+app_name
    else:
        #project_number=nems_loc+'@'+nems_rev
        project_number='Version 2.0.0'
        project_name=NEMS_PROJECT_NAME

    with open('Doxyfile','wt') as dw:
        with open('Doxyfile.IN','rt') as dr:
            for line in dr:
                dw.write(line.replace('--PROJECT_NUMBER--',project_number)
                         .replace('--PROJECT_NAME--',project_name)
                         .replace('--MAIN_PAGE--',main_page)
                         .replace('--CWD--',os.path.realpath(os.getcwd())))

# ----------------------------------------------------------------------

def error(what):
    global bad_flag
    bad_flag=True
    sys.stderr.write('prep_inputs.py error: '+what.strip()+'\n')

# ----------------------------------------------------------------------

def scan_args(arglist):
    try:
        opt,args=getopt.getopt(arglist,'a')
    except getopt.GetoptError as e:
        sys.stderr.write(str(e)+'\n')
        sys.exit(2)
    return ( ('-a' in opt), args[0], args[1:] )

# ----------------------------------------------------------------------

def check_output(cmd):
    # Workaround for lack of subprocess.check_output in 2.6
    p=subprocess.Popen(['sh','-c',cmd],executable='/bin/sh',stdout=subprocess.PIPE)
    (out,err)=p.communicate()
    if p.poll():
        raise Exception('%s: non-zero exit status'%(cmd,))

    print '%s => %s --- %s'%(repr(cmd),repr(out),repr(err))
    return out

# ----------------------------------------------------------------------

def scan_nems(nems_dir):
    if os.path.exists(os.path.join(nems_dir,'.git')):
        return git_scan_nems(nems_dir)
    elif os.path.exists(os.path.join(nems_dir,'.svn')):
        return svn_scan_nems(nems_dir)
    else:
        error('%s: could not determine if this is a git or subversion repo'%(nems_dir,))
        return None, None

# ----------------------------------------------------------------------

def scan_app(app_dir):
    if os.path.exists(os.path.join(app_dir,'.git')):
        return git_scan_app(app_dir)
    elif os.path.exists(os.path.join(app_dir,'.svn')):
        return svn_scan_app(app_dir)
    else:
        error('%s: could not determine if this is a git or subversion repo'%(app_dir,))
        return None, None, None

# ----------------------------------------------------------------------

def svn_scan_nems(nems_dir):
    nems_rev=None
    nems_loc=None

    svn_info=check_output('svn info '+nems_dir)
    for line in svn_info.splitlines():
        r=re.match('''(?x) (?:
              ^ Revision: \s+ (?P<revision> \d+ )
            | ^ URL: .*? (?P<loc> branches/\S+ | tags/\S+ | [^/]+ )$ 
        ) ''', line.strip())
        if not r: 
            continue
        if r.group('revision'):   nems_rev=r.group('revision')
        if r.group('loc'):        nems_loc=r.group('loc')

    if not nems_rev:
        error('%s: could not get svn revision\n'%(nems_dir,))
        nems_rev='unknown'

    if not nems_loc:
        error('%s: could not get nems location '
              'relative to top of project\n'%(nems_dir,))
        nems_loc='nems'

    return nems_rev, nems_loc

# ----------------------------------------------------------------------

def git_scan_nems(nems_dir):
    nems_rev=None
    nems_loc=None

    info=check_output('set -e ; cd '+nems_dir+' ; git branch')
    for line in info.splitlines():
        r=re.match('^\s*\*\s*(\S+)',line.strip())
        nems_loc = ( r and r.group(1) ) or None
        if nems_loc: break
    
    info=check_output('set -e ; cd '+nems_dir+' ; git rev-parse HEAD')
    for line in info.splitlines():
        nems_rev = line.strip()[:10] or None
        if nems_rev: break

    if not nems_rev:
        error('%s: could not get git hash of HEAD\n'%(nems_dir,))
        nems_rev='unknown'

    if not nems_loc:
        error('%s: could not get git branch\n'%(nems_dir,))
        nems_loc='nems'

    return nems_rev, nems_loc

# ----------------------------------------------------------------------

def svn_scan_app(app_dir):
    app_rev=None
    app_name=None
    app_loc=None
    
    info_up2=check_output('svn info '+app_dir)
    for lines in info_up1.splitlines():
        r=re.match('''(?x) (?:
              ^ Revision (?P<revision> \d+ )
            | ^ URL: https://.*?/apps/ (?P<name>[^/]+)
            | ^ URL: .*? (?P<loc> branches/\S+ | tags/\S+ | [^/]+) $
            )''', line.strip())
        if not r: continue
        if r.group('revision'):   app_rev=r.group('revision')
        if r.group('loc'):        app_loc=r.group('loc')
        if r.group('name'):       app_name=r.group('name')

    if not app_rev:
        error('%s: could not get svn revision\n'%(app_dir,))
        app_rev='unknown'

    if not app_name:
        error('%s: could not get app name\n'%(app_dir,))
        app_name='app'

    if not app_loc:
        error('%s: could not get svn location '
              'relative to app directory\n'%(app_dir,))
        app_rev='app'

    return app_rev, app_name, app_loc

# ----------------------------------------------------------------------

def git_scan_app(app_dir):
    app_rev=None
    app_name=None
    app_loc=None

    info=check_output('set -e ; cd '+app_dir+' ; git remote show -n origin')
    for line in info.splitlines():
        r=re.match('^.*URL:\s*(\S+)',line.strip())
        app_name = ( r and r.group(1) ) or None
        if app_loc: break

    info=check_output('set -e ; cd '+app_dir+' ; git branch')
    for line in info.splitlines():
        r=re.match('^\s*\*\s*(\S+)',line.strip())
        app_loc = ( r and r.group(1) ) or None
        if app_loc: break
    
    info=check_output('set -e ; cd '+app_dir+' ; git rev-parse HEAD')
    for line in info.splitlines():
        app_rev = line.strip()[:10] or None
        if app_rev: break

    if not app_rev:
        error('%s: could not get git hash of HEAD\n'%(app_dir,))
        app_rev='unknown'

    if not app_loc:
        error('%s: could not get git branch\n'%(app_dir,))
        app_loc='app'

    if not app_name:
        error('%s: could not get git URL\n'%(app_dir,))
        app_loc='app'

    return app_rev, app_name, app_loc

# ----------------------------------------------------------------------

def get_app_list(app_info):
    return '' # FIXME: implement this

# ----------------------------------------------------------------------

if __name__=='__main__':
    main()
