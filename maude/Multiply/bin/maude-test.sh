#!/bin/bash

function usage {
  echo "Usage: maude-tests.sh"
  echo "  -m <FILE> : maude file "
  echo "  -t <FILE> : test file "
  echo "  -i        : initialize test file"
  echo "  (implied) <FILE> : correct result file in \$MAUDE_HOME/correct/\$t"
}

if [[ -z $MAUDE_HOME ]]; then
  echo "MAUDE_HOME must be set"
  exit 1
fi

mh=$MAUDE_HOME


while getopts im:t: flag
do
  case ${flag} in
    m) m="${OPTARG}";;
    t) t="${OPTARG}";;
    i) i=i;;
    ?) usage; echo "Error: unknown option '$flag'"; exit 1;;
  esac
done

if [[ -z $m ]] || [[ -z $t ]] ; then
  usage
  echo "Error: missing required option"
  exit 1;
fi


if [[ ! -f $mh/$m ]] || [[ ! -f $mh/$t ]]; then
  usage
  echo "Error: one of inputs is not a file"
  echo "> $mh/$m"
  echo "> $mh/$t"
  exit 1;
fi

tn=${t##*/}
c=$mh/correct/$tn

if [[ ! -f $c ]] && [[ -z $i ]]; then
  usage
  echo "Error: correct file '$c' does not exist"
  exit 1;
fi

th=/tmp/maude-test

mkdir -p $th

maude-pipe.sh -m $m < $mh/$t > $th/$tn-result 

if [[ "$i" == "i" ]]; then
  if [[ -f $c ]]; then
    echo ---
    diff $c $th/$tn-result
    echo ---
    while true; do
      read -p "Correct file already exists, confirm changes (above) (Y/N):" yn
      case $yn in 
	      [yY] )  break;;
	      [nN] ) echo exiting...;
		        exit;;
	      * ) echo invalid response;;
      esac
    done
  fi
  cat $th/$tn-result > $c
  echo "Populated correct file $c"
  exit 0;
fi

diff $c $th/$tn-result | tee $th/$tn-diff

if [[ ! -z "`cat $th/$tn-diff`" ]]; then
  echo "Test fail on input $mh/$t" 1>&2
  exit 1;
fi
