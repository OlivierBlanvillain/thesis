load "figures/4x4.gnu"
set output "figures/numpy.gex"
set xlabel "Input size"
set ylabel "~"

plot                        \
  "numpy-implicits.csv"     \
  u 1:($2/1000)             \
  w lines lw 3 dt 2         \
  title "implicits reduce"  \
,                           \
  "numpy-dependent.csv"     \
  u 1:($2/1000)             \
  w lines lw 3              \
  title "singletons reduce" \
,                           \
  "numpy-match-types.csv"   \
  u 1:($2/1000)             \
  w lines lw 3              \
  title "match types reduce"

set output "figures/numpy-2.gex"

plot                        \
  "numpy-dependent.csv"     \
  u 1:($2)                  \
  w lines lw 3              \
  title "singletons reduce" \
,                           \
  "numpy-match-types.csv"   \
  u 1:($2)                  \
  w lines lw 3              \
  title "match types reduce"
