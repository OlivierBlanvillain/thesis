load "figures/4x4.gnu"
set output "figures/numpy.gex"
set xlabel "Input size"
set ylabel "~"

plot                         \
  "numpy-implicits.csv"      \
  u 1:($2/1000) w lines lt 3 \
  title "implicits reduce"   \
,                            \
  "numpy-dependent.csv"      \
  u 1:($2/1000) w lines lt 2 \
  title "singletons reduce"  \
,                            \
  "numpy-match-types.csv"    \
  u 1:($2/1000) w lines lt 1 \
  title "match types reduce" \

set output "figures/numpy-2.gex"

plot                         \
  "numpy-match-types.csv"    \
  u 1:($2) w lines           \
  title "match types reduce" \
,                            \
  "numpy-dependent.csv"      \
  u 1:($2) w lines           \
  title "singletons reduce"  \
