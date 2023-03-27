#!/bin/bash

if [[ -z $1 ]]; then
    echo "Usage: timer.sh file-name"
    exit
fi

touch /tmp/timercount
tc=`cat /tmp/timercount`
if [ -z $tc ]; then
  tc=0;
fi
(( tc+=1 ))
#echo "timer count $tc" 1>&2
echo $tc > /tmp/timercount

titers=`cat /tmp/timer_iterations`
echo "timer iterations $titers"

pushd "/home/scarbro/CSU/PolyMult/kernel-timer" > /dev/null

rm $1
make > /dev/null
i=0
while [ $i -le ${titers} ]; do
    rm /tmp/kernel-timer-results
    $1  > /tmp/kernel-timer-results
    if [[ "$?" != 0 ]]; then
    	echo "failed"
    else
    	cat /tmp/kernel-timer-results | grep time
    fi
    (( i+=1 ))
done
popd > /dev/null
