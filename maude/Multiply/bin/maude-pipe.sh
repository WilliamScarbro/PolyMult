#!/bin/bash

while getopts pofm:a: flag
do
  case $flag in
    m) m=${OPTARG};;
    a) a=${OPTARG};;
    o) o=o;;
    f) f=f;;
    p) p=p;;
    ?) echo "invalid option: ${OPTARG}";;
  esac
done

function usage {
  echo "Usage: maude-pipe "
  echo "  -m FILE : maude source (required)"
  echo "  -a ARG  : maude argument"
  echo "  -f      : display functions pretty"
  echo "  -o      : display objects pretty"
  echo "  -p      : turn on printing"
}

function update_out {
  counter=$((counter + 1))
  new=$tmp/out$counter
  cp $out $new
  export out=$new
}

if [[ -z $MAUDE_HOME ]]; then
  echo "MAUDE_HOME not set"
  exit 1
fi

mh=$MAUDE_HOME

if [[ -z $m ]]; then
  usage
  echo "Error: 'm' flag is required"
  exit 1
fi

if [[ ! -f $mh/$m ]]; then
  usage
  echo "Error: $mh/$m is not a file"
  exit 1
fi

tmp=/tmp/maude-pipe
rm -rf $tmp
mkdir -p $tmp
echo "maude pipe run | date: `date` | maude source: $m | input: $i" > $tmp/context

maude $mh/$m -no-wrap < /dev/stdin > $tmp/out1 2> $tmp/error
out=$tmp/out1
counter=1

# parse maude output
update_out
grep result $out | sed "s/result [^ ]*: //g" > $tmp/inter
grep INFO $out > $tmp/INFO
cat $tmp/inter > $out


if [[ ! -z $a ]]; then
  update_out
  sed "s/^/$a /g" $out -i
  sed "s/$/ ./g" $out -i
fi

if [[ ! -z $p ]]; then
  update_out
  sed "s/^/set print attribute on .\n/g" $out -i
fi

# parse objects
if [[ ! -z $o ]]; then
  update_out
  sed 's/| /|\n  /g' $out -i
  sed 's/,\([A-Z]\)/,\n  \1/g' $out -i
  sed 's/\([^-]\)> /\1>\n/g' $out -i
  sed 's/</\n</g' $out -i
fi

# parse functions
if [[ ! -z $f ]]; then
  update_out
  #sed 's/\([A-Za-z-]\)*[/\1[\n    /g' $out -i
  #sed "s/\[/\[\n      /g" $out -i
  #sed "s/\]/\n    \]/g" $out -i
  #sed 's/,\([a-z]\)/,\n    \1/g' $out -i
  sed "s/\([a-z][a-z-]*\[\)/\n    \1/g" $out -i
fi

cat $tmp/error > /dev/stderr
cat $out
