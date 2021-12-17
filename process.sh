#!/bin/sh

set -eux

BASEDIR=$(dirname "$0")

rm "$BASEDIR/"*.csv || true

cat "$BASEDIR"/*.log         |\
grep -E -A4 "Running|Result" |\
grep -E "±|scala"            |\
paste -s -d' \n'             |\
while read res; do
  # [info] Running (fork) dotty.tools.benchmarks.Bench 40 10 1 /path/to/src-gen/dependent-remove-140.scala   295.236 ±(99.9%) 17.036 ms/op [Average]
  file=$(echo "$res" | grep -oP '(?<=src-gen/).+(?=.scala)')
  avg=$(echo "$res" | grep -oP '(?<=   )\d+.\d+(?=.*99.9)')
  err=$(echo "$res" | grep -oP '(?<= )\d+.\d+(?= ms/op)')
  bench=$(echo "$file" | grep -oP '.+(?=-\d+)')
  size=$(echo "$file" | grep -oP '\d+')
  echo "$size,$avg,$err" >> "$BASEDIR/$bench.csv"
done

ls "$BASEDIR/"*.csv | while read f; do
  sort -n "$f" -o "$f"
done
