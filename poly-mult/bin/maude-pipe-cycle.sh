#!/bin/bash

if [[ -z $MAUDE_HOME ]]; then
  echo "MAUDE_HOME must be set"
  exit 1
fi

mh=$MAUDE_HOME

mch=/tmp/maude-cycle
mkdir -p $mch
mdfp=$mch/MAUDE_SOURCE_POINTER
msfp=$mch/MAUDE_DEST_POINTER
mdf=$mch/out-file1
msf=$mch/out-file2

while getopts psm:i: flag
do
  case ${flag} in
    i) i="${OPTARG}";;
    m) m="${OPTARG}";;
    s) s="s";;
    p) p="p";;
  esac
done

function usage {
  echo "Usage:"
  echo "  -i FILE : initialize cycle with FILE"
  echo "  -m FILE : maude source"
  echo "  -s      : Supress errors"
  echo "  -p      : Print INFO"
}

function maude_pipe {
  ef=$mch/error
  #echo "maude-pipe.sh -m $1 -a 'rew [1]' < $2 > $3 2> $ef"
  maude-pipe.sh -m $1 -a "rew [1]" -of$p < $2 > $3 2> $ef
  if [[ ! -z `cat $ef` ]] && [[ -z $s ]] ; then
    echo "Error"
    cat $ef
  fi
  echo "---"
  cat $3
  echo "---"
  if [[ ! -z $p ]]; then
    cat /tmp/maude-pipe/INFO
  fi
 
}  
if [[ -z $m ]]; then
  usage
  echo "Error: 'm' flag must be specified"
  exit 1
fi

if [[ ! -f $m ]]; then
  usage
  echo "Error: $m is not a file"
  exit 1
fi

  
if [[ ! -z $i ]]; then
  if [[ ! -f $mh$i ]]; then
    usage
    echo "Error: $mh$i is not a file"
    exit 1
  fi
  rm -rf $mch
  mkdir -p $mch
  maude_pipe $m $mh$i $mdf
  echo $msf > $msfp
  echo $mdf > $mdfp
else
  md=`cat $msfp`
  ms=`cat $mdfp`
  maude_pipe $m $ms $md
  if [[ -z `diff $ms $md` ]]; then
    echo "Warning: No Change"
  fi
  echo $md > $mdfp
  echo $ms > $msfp
fi
