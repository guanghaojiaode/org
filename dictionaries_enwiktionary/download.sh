#!/bin/bash

# test for existence of lynx and awk
for PROG in lynx awk
do
command -v $PROG >/dev/null 2>&1 || { echo >&2 "Program $PROG is required but it's not installed.  Aborting."; exit 1; }
done

#printout usage info:
if [ $# -lt 2 ] 
then
echo "usage: gettextdictionary.sh SOURCE-ISO  TARGET-ISO >dictionaryfile.ding";
echo "where SOURCE-ISO  and  TARGET-ISO are the iso-languagecodes of the source- target-language resp., e.g \"es\" \"en\" for the Spanish-English dictionary"
exit;
fi

iso=$1
iso2=$2
WIKIPATH=User:Matthias_Buchmeier/$iso-$iso2
 
for letter in a b c d e f g h i j k l m n o p q r s t u v w x y z 0
do
lynx -width=1000 -nolist -underscore -dump -assume_charset=utf-8 -display_charset=utf-8 "http://en.wiktionary.org/w/index.php?title=$WIKIPATH-$letter&printable=yes" |\
awk '/::/ {gsub(/[\ ]+/, " "); gsub(/^[\ ]/, ""); print;}' 
done

