load "figures/4x4.gnu"
set output "figures/concat.gex"
set ylabel "Compilation time (sec)"
set xlabel "Input size"
# set xlabel "~"

plot                        \
  "concat-implicits.csv"    \
  u 1:($2/1000)             \
  w lines lw 3 dt 2         \
  title "implicits concat"  \
,                           \
  "concat-dependent.csv"    \
  u 1:($2/1000)             \
  w lines lw 3              \
  title "singletons concat" \
,                           \
  "concat-match-types.csv"  \
  u 1:($2/1000)             \
  w lines lw 3              \
  title "match types concat"

set output "figures/concat-2.gex"
set ylabel "Compilation time (ms)"

plot                        \
  "concat-dependent.csv"    \
  u 1:($2)                  \
  w lines lw 3              \
  title "singletons concat" \
,                           \
  "concat-match-types.csv"  \
  u 1:($2)                  \
  w lines lw 3              \
  title "match types concat"
