load "figures/4x4.gnu"
set output "figures/numpy.gex"
set xlabel "Input size"
set ylabel "~"

plot                         \
  "numpy-implicits.csv"      \
  u 1:($2/1000) w lines lt 3 \
  title "Implicits Reduce"   \
,                            \
  "numpy-dependent.csv"      \
  u 1:($2/1000) w lines lt 2 \
  title "Singletons Reduce"  \
,                            \
  "numpy-match-types.csv"    \
  u 1:($2/1000) w lines lt 1 \
  title "Match types Reduce" \

set output "figures/numpy-2.gex"

plot                         \
  "numpy-match-types.csv"    \
  u 1:($2) w lines           \
  title "Match types Reduce" \
,                            \
  "numpy-dependent.csv"      \
  u 1:($2) w lines           \
  title "Singletons Reduce"  \
