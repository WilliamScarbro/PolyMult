#!/bin/bash

function usage {
  echo "Usage: maude-tests.sh"
  echo "  -m <FILE> : maude test file "
  echo "  -t        : test name"
  echo "  -i        : initialize test file"
  echo "  -c        : compare test with correct"
  echo "  (implied) <FILE> : test file in \$MAUDE_HOME/lib/test/\$t"
  echo "  (implied) <FILE> : correct result file in \$MAUDE_HOME/lib/correct/\$t"
  echo "                     not required when i flag is used"
}

if [[ -z $MAUDE_HOME ]]; then
  echo "MAUDE_HOME must be set"
  exit 1
fi

mh=$MAUDE_HOME


while getopts im:t:c flag
do
  case ${flag} in
    m) m="${OPTARG}";;
    t) t="${OPTARG}";;
    i) i=i;;
    c) c=c;;
    ?) usage; echo "Error: unknown option '$flag'"; exit 1;;
  esac
done

if [[ -z $m ]] || [[ -z $t ]] ; then
  usage
  echo "Error: missing required option"
  exit 1;
fi

test_mod="$mh/$m"
test_name=${t##*/}
test_file="$mh/lib/test/$test_name"
if [[ ! -f $test_mod ]]; then
  usage
  echo "Error: src file does not exist"
  echo "> $test_mod"
  exit 1;
fi

if [[ ! -f $test_file ]]; then
  usage
  echo "Error: test file does not exist"
  echo "> $test_file"
  exit 1
fi

correct="$mh/lib/correct/$test_name"

if [[ ! -f $correct ]] && [[ -z $i ]]; then
  usage
  echo "Error: correct file '$correct' does not exist"
  exit 1;
fi

th=/tmp/maude-test

rm -rf $th
mkdir -p $th

maude-pipe.sh -m $m < $test_file > $th/$test_name-result 

if [[ "$i" == "i" ]]; then
  if [[ -f $correct ]]; then
    echo ---
    diff $correct $th/$test_name-result
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
  cat $th/$test_name-result > $correct
  echo "Populated correct file $correct"
  exit 0;
fi

if [[ $c == c ]]; then
  echo "--- input"
  cat $test_file
  echo "--- result"
  cat $th/$test_name-result
  exit 
fi

diff $correct $th/$test_name-result > $th/$test_name-diff

if [[ ! -z "`cat $th/$test_name-diff`" ]]; then
  echo "---" 1>&2
  echo "Test fail on input $test_name" 1>&2
  cat $correct 1>&2
  cat $th/$test_name-result 1>&2
  echo "---" 1>&2
  exit 1;
fi
