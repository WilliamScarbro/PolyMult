#!/bin/bash



function usage {
  echo "Usage: maude-tests-suite.sh"
  echo "  -m    : module to test "
  echo "  -i    : initialize correct files (passes to maude-test)"
  echo "  -a    : test all modules"
  echo "  (implied) <FILE> : test handle file in \$MAUDE_HOME/src/test/\$m-TEST.maude"
  echo "  (implied) <FILE> : suite file in \$MAUDE_HOME/lib/suite/\$m"
}

if [[ -z $MAUDE_HOME ]]; then
  echo "MAUDE_HOME must be set"
  exit 1
fi

mh=$MAUDE_HOME


while getopts aim: flag
do
  case ${flag} in
    m) m="${OPTARG}";;
    i) i=i;;
    a) a=a;;
    ?) usage; echo "Error: unknown option '$flag'"; exit 1;;
  esac
done

if [[ "$a" == a ]]; then
  all_modules="$mh/lib/suite/all"
  if [[ ! -f $all_modules ]]; then
    echo "Error: missing all module file '$all_modules'"
    exit 1
  fi
  while read -r line; do
    maude-test-suite.sh -m $line
  done < $all_modules
  exit 
fi

if [[ -z $m ]] ; then
  usage
  echo "Error: missing required option"
  exit 1;
fi

test_source="$mh/src/test/$m-TEST.maude"
test_suite="$mh/lib/suite/$m"

if [[ ! -f $test_source ]]; then
  usage
  echo "Error: source file does not exist"
  echo "> $test_source"
  exit 1;
fi

if [[ ! -f $test_suite ]]; then
  usage
  echo "Error: test suite file does not exist"
  echo "> $test_suite"
  exit 1
fi

count=0
while read -r line; do
  #echo $line
  test_file="$mh/lib/test/$line"
  correct_file="$mh/lib/correct/$line"
  if [[ ! -f $test_file ]]; then
    echo "Error: $test_file is not a file"
    exit 1
  fi
  if [[ ! -f $correct_file ]] && [[ -z $i ]]; then
    echo "Error: $correct_file is not a file"
    exit 1
  fi
  if [[ -z $i ]]; then
    maude-test.sh -m "src/test/$m-TEST.maude" -t "$line"
  else
    echo "Y" | maude-test.sh -m "src/test/$m-TEST.maude" -t "$line" -i
  fi
  if [[ $? != 0 ]]; then
    count=$(( count+1 ))
  else
    echo "Test success on input $line"
  fi
done < $test_suite

if [[ $count != 0 ]]; then
  echo "Error count: $count"
  exit 1
fi
