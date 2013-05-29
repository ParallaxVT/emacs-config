#! /bin/bash

# Usage:
# Merges init_base.el + files in conf/ + files in defun/ -> init.el
# Byte-compile init.el to init.elc

# ================
# Global variables
# ================
emacsdir="/media/c/Users/rafaelgp/AppData/Roaming/.emacs"
initfile="$emacsdir/init.el"
confdir="$emacsdir/conf"
defundir="$emacsdir/defun"

if [ $HOSTNAME = "debian" ]; then
    emacspath=/usr/bin/emacs-snapshot
fi

if [ $HOSTNAME = "RafaelGP" ]; then
    emacspath=/media/c/emacs-24.2/bin/emacs.exe
fi

# ================
# Global functions
# ================

function print_done() {
    printf "[ \e[92mdone\e[0m ] ...\n"
}

function print_ok() {
    printf "[  \e[92mok\e[0m  ] $1\n"
}

function print_info() {
    printf "[ \e[96minfo\e[0m ] $1\n"
}

function print_warn() {
    printf "[ \e[93mwarn\e[0m ] $1\n"
}

function print_fail() {
    printf "[ \e[91mfail\e[0m ] $1\n"
    exit 1
}

# Check if forders exists
function checkdir() {
    if [ -d $1 ]; then
        print_ok "Found directory: $(basename $1)"
    else
        echo_fail "Directory not found: $1"
    fi
}

# Check if forders exists
function checkfile() {
    if [ -f $1 ]; then
        print_ok "Found file: $(basename $1)"
    else
        echo_fail "File not found: $1"
    fi
}

# ================
# Script
# ================

checkdir $emacsdir
checkdir $confdir
checkdir $defundir
checkfile $emacspath
checkfile ./init_base.el

# Merge
cat $confdir/conf*.el > $confdir/all.tmp
cat $defundir/defun*.el > $defundir/all.tmp

cp --remove-destination $emacsdir/init_base.el $initfile

cat $confdir/all.tmp >> $initfile
cat $defundir/all.tmp >> $initfile
cat $emacsdir/vendor/vendor.el >> $initfile

checkfile $confdir/all.tmp  && rm $confdir/all.tmp
checkfile $defundir/all.tmp  && rm $defundir/all.tmp

# Make sure the file doesn't have any '^M'
checkfile $initfile  && dos2unix -q $initfile

# Byte-compile, no emacs loading messages (-Q), no byte-compile wanning messgages

$emacspath -Q -batch --eval '(byte-compile-disable-warning nil) (byte-compile-file "init.el")' && print_ok "Create file: init.elc"

print_done

exit 0
