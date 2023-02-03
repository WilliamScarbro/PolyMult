#!/bin/bash

if [[ -z $1 ]]; then
    echo "Usage: timer.sh file-name"
    exit
fi

pushd "/home/scarbro/CSU/PolyMult/kernel-timer" > /dev/null

rm $1
make > /dev/null
rm /tmp/kernel-timer-results
for i in {1..10}; do
    $1  > /tmp/kernel-timer-results
    if [[ "$?" != 0 ]]; then
	echo "failed"
    else
	cat /tmp/kernel-timer-results
    fi
done

popd > /dev/null
