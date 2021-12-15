#!/bin/sh

set -eu

BASEDIR=$(realpath "$(dirname "$0")")

# Generate sources:
# rm -rf "$BASEDIR/scala/target/generated/"
# mkdir -p "$BASEDIR/scala/target/generated/"
# git grep --name-only //X" | grep scala | while read s; do
#   cp "$s" "$BASEDIR/scala/target/generated/"
# done
# find "$BASEDIR/scala/target/generated/" -name "*.scala" | while read s; do
#   seq -w 1 8 256 | while read i; do
#     replacement=$(seq 0 $(expr $i - 1) | xargs -i printf "{} :: ")
#     echo "$s.$i.scala"
#     cat $s | sed "s#0 :: //X#$replacement#g" > "$s.$i.scala"
#   done
#   rm $s
# done

# Mesure bytecode size:
# rm -f bytecode-size.log
# rm -f *.class *.tasty
# find "$BASEDIR/scala/target/generated/" -name "*.scala" | sort | while read s; do
#   if echo "$s" | grep -q "dependent"; then
#     /usr/bin/time -f "%e" -o "/tmp/time" $HOME/workspace/thesis/bin/dotc -J-Xss4m "$s"
#   else
#     /usr/bin/time -f "%e" -o /tmp/time $HOME/workspace/dotty-master/bin/scalac -J-Xss4m "$s"
#   fi
#   echo "$s: $(cat *.class | wc -c) ($(cat /tmp/time)s)" | tee -a bytecode-size.log
#   rm -f *.class *.tasty
# done

# Mesure compilation times:
find "$BASEDIR/scala/target/generated/" -name "*.scala" | sort | while read s; do
  if echo "$s" | grep -q "dependent"; then
    (cd "$BASEDIR/ddotty/" && sbt "dotty-bench/jmh:run 60 60 1 $s" | tee "$s.log")
  else
    (cd "$HOME/workspace/dotty-master/" && sbt "scala3-bench/jmh:run 60 60 1 $s" | tee "$s.log")
  fi
done
