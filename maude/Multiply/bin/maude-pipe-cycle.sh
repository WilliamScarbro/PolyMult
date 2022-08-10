#!/bin/bash

if [[ -z $MAUDE_HOME ]]; then
  echo "MAUDE_HOME must be set"
  exit 1
fi

mh=$MAUDE_HOME
mdfp=$mh/bin/MAUDE_SOURCE_POINTER
msfp=$mh/bin/MAUDE_DEST_POINTER
mdf=$mh/bin/out-file1
msf=$mh/bin/out-file2

while getopts m:i: flag
do
  case ${flag} in
    i) i="${OPTARG}";;
    m) m="${OPTARG}";;
  esac
done

function usage {
  echo "Usage:"
  echo "  -i FILE : initialize cycle with FILE"
  echo "  -m FILE : maude source"
  exit 1
}

if [[ ! -f $m ]]; then
  usage
fi

  
if [[ ! -z $i ]]; then
  if [[ ! -f $i ]]; then
    usage
  fi
  maude-pipe.sh -m $mh/$m -of -a "rew [1]" < $i | tee $mdf
  echo $msf > $msfp
  echo $mdf > $mdfp
else
  md=`cat $msfp`
  ms=`cat $mdfp`
  maude-pipe.sh -m $mh/$m -of -a "rew [1]" < $ms | tee $md
  echo $md > $mdfp
  echo $ms > $msfp
fi
