#!/bin/sh

MHSHOW=/usr/local/nmh-1.7.1/bin/mhshow

if [ X"$1" = X"" ]; then
  echo "invalid argument"
  exit 0
fi

(# insert dummy header
  cont=`expr "$1" : ':=HDRT=:\(.*\):=HDRE=:.*'`
  if [ X"$cont" != X"" ]; then
    echo Content-Type: $cont
  else
    # assume old junet mail
    echo "Content-Type: text/plain; charset=iso-2022-jp"
  fi
  char=`expr "$1" : ':=HDRT=:.*:=HDRE=:\(.*\)'`
  if [ X"$char" != X"" ]; then
    echo Content-Transfer-Encoding: $char
  fi
  echo ""
  cat - ) | \
$MHSHOW -textonly -inlineonly -noheader -form mhl.body -file -
