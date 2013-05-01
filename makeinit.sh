#! /bin/bash

minfile="init_min.el"
initfile="init.el"

emacsfolder=/media/c/Users/rafaelgp/AppData/Roaming/.emacs

if [ $HOSTNAME = "debian" ]; then
    emacspath=/usr/bin/emacs-snapshot
fi

if [ $HOSTNAME = "RafaelGP" ]; then
    emacspath=/media/c/emacs-24.2/bin/emacs.exe
fi

cat $emacsfolder/conf/conf*.el > $emacsfolder/conf/all.tmp
cat $emacsfolder/defun/defun*.el > $emacsfolder/defun/all.tmp

cp $emacsfolder/init_base.el $emacsfolder/init.el

cat $emacsfolder/conf/all.tmp >> $emacsfolder/init.el
cat $emacsfolder/defun/all.tmp >> $emacsfolder/init.el
cat $emacsfolder/vendor/vendor.el >> $emacsfolder/init.el

rm $emacsfolder/conf/all.tmp
rm $emacsfolder/defun/all.tmp
# rm $emacsfolder/vendor/all.tmp

dos2unix $emacsfolder/init.el

# > $minfile
# for d in ./defun/*.el ; do
    # cat $d >> $minfile
    # echo $d
# done

# for c in ./conf/*.el ; do
    # cat $c >> $minfile
    # echo $c
# done

$emacspath -batch --eval '(byte-compile-file "init.el")'
# emacs --batch --eval '(byte-compile-file "your-elisp-file.el")'

echo "EOF"
exit 1