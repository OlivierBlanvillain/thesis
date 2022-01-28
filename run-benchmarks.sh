#!/bin/sh

set -eu
test "$(pwd)" = "$(realpath "$(dirname "$0")")" || (echo "wrong dir"; exit 1)

test -d ddotty || git clone git@github.com:dotty-staging/dotty.git --branch add-transparent-7 ddotty
test -d dotty || git clone git@github.com:lampepfl/dotty.git --branch 3.1.2-RC1 dotty

# Generate sources:
rm -rf "scala/target/generated/"
mkdir -p "scala/target/generated/"
git grep --name-only "//X" | grep scala | while read s; do
  cp "$s" "scala/target/generated/"
done
find "$(pwd)/scala/target/generated/" -name "*.scala" | while read s; do
  seq -w 1 8 256 | while read i; do
    replacement=$(seq 0 $(expr $i - 1) | xargs -i printf "{} :: ")
    echo "$s.$i.scala"
    cat $s | sed "s#0 :: //X#$replacement#g" > "$s.$i.scala"
  done
  rm $s
done

# Mesure bytecode sizes:
rm -f bytecode-size.log
rm -f *.class *.tasty
find "$(pwd)/scala/target/generated/" -name "*.scala" | sort | while read s; do
  if echo "$s" | grep -q "dependent"; then
    ddotty/bin/dotc -J-Xss4m "$s"
  else
    dotty/bin/scalac -J-Xss4m "$s"
  fi
  echo "$s: $(cat *.class | wc -c)" | tee -a bytecode-size.log
  rm -f *.class *.tasty
done

# Mesure compilation times:
find "$(pwd)/scala/target/generated/" -name "*.scala" | sort | while read s; do
  if echo "$s" | grep -q "dependent"; then
    (cd "ddotty/" && sbt "dotty-bench/jmh:run 60 60 1 $s" | tee "$s.log")
  else
    (cd "dotty/" && sbt "scala3-bench/jmh:run 60 60 1 $s" | tee "$s.log")
  fi
done

# Generate CSV files:
rm -rf figures/*.csv
find "$(pwd)/scala/target/generated/" -name "*.log" | sort | while read l; do
  r=$(grep "±(99.9%)" "$l")
  avg=$(echo "$r" | grep -oP '\d+.\d+(?=.*99.9)')
  err=$(echo "$r" | grep -oP '(?<= )\d+.\d+(?= ms/op)')
  bench=$(echo "$l" | grep -oP '(?<=generated/)[^.]+(?=.scala)')
  size=$(echo "$l" | grep -oP '\d+')
  echo "$size,$avg,$err" >> "figures/$bench.csv"
done
