#!/bin/sh

set -eu #x

# Generate sources:

rm -rf scala/target/generated/
mkdir scala/target/generated/

git grep --name-only //X | while read s; do
  cp $s scala/target/generated/
done

find scala/target/generated/ -type f | while read s; do
  seq 1 8 100 | while read i; do
    replacement=$(seq 0 $(expr $i - 1) | xargs -i printf "{} :: ")
    echo "$s.$i.scala"
    cat $s | sed "s#0 :: //X#$replacement#g" > $s.$i.scala
  done
  rm $s
done

# Mesure bytecode size:

rm -f bytecode-size.log
rm -f *.class
find "$(pwd)/scala/target/generated/" -type f | sort | while read s; do
  if echo "$s" | grep -q "dependent"; then
    time -f "%e" -o /tmp/time ./ddotty/bin/dotc -J-Xss4m "$s"
  else
    time -f "%e" -o /tmp/time $HOME/workspace/dotty-master/bin/scalac -J-Xss4m "$s"
  fi
  echo "$s: $(cat *.class | wc -c) ($(cat /tmp/time)s)" | tee -a bytecode-size.log
  rm -f *.class
done
