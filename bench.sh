#!/bin/sh

set -eu
test "$(pwd)" = "$(realpath "$(dirname "$0")")" || (echo "wrong dir"; exit 1)

# Generate sources:
# rm -rf "scala/target/generated/"
# mkdir -p "scala/target/generated/"
# git grep --name-only //X" | grep scala | while read s; do
#   cp "$s" "scala/target/generated/"
# done
# find "scala/target/generated/" -name "*.scala" | while read s; do
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
# find "scala/target/generated/" -name "*.scala" | sort | while read s; do
#   if echo "$s" | grep -q "dependent"; then
#     /usr/bin/time -f "%e" -o "/tmp/time" $HOME/workspace/thesis/bin/dotc -J-Xss4m "$s"
#   else
#     /usr/bin/time -f "%e" -o /tmp/time $HOME/workspace/dotty-master/bin/scalac -J-Xss4m "$s"
#   fi
#   echo "$s: $(cat *.class | wc -c) ($(cat /tmp/time)s)" | tee -a bytecode-size.log
#   rm -f *.class *.tasty
# done

# Mesure compilation times:
# find "scala/target/generated/" -name "*.scala" | sort | while read s; do
#   if echo "$s" | grep -q "dependent"; then
#     (cd "ddotty/" && sbt "dotty-bench/jmh:run 60 60 1 $s" | tee "$s.log")
#   else
#     (cd "$HOME/workspace/dotty-master/" && sbt "scala3-bench/jmh:run 60 60 1 $s" | tee "$s.log")
#   fi
# done

# Generate CSV:
rm -rf figures/*.csv
find "scala/target/generated/" -name "*.log" | sort | while read l; do
  r=$(grep "±(99.9%)" "$l")
  avg=$(echo "$r" | grep -oP '\d+.\d+(?=.*99.9)')
  err=$(echo "$r" | grep -oP '(?<= )\d+.\d+(?= ms/op)')
  bench=$(echo "$l" | grep -oP '(?<=generated/)[^.]+(?=.scala)')
  size=$(echo "$l" | grep -oP '\d+')
  echo "$size,$avg,$err" >> "figures/$bench.csv"
done
