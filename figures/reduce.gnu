load "figures/4x4.gnu"
set output "figures/reduce.gex"
set xlabel "Input size"
set ylabel "~"

plot                         \
  "reduce-implicits.csv"     \
  u 1:($2/1000) w lines lt 3 \
  title "Implicits reduce"   \
,                            \
  "reduce-dependent.csv"     \
  u 1:($2/1000) w lines lt 2 \
  title "Singletons reduce"  \
,                            \
  "reduce-match-types.csv"   \
  u 1:($2/1000) w lines lt 1 \
  title "Match types reduce" \

set output "figures/reduce-2.gex"

plot                         \
  "reduce-match-types.csv"   \
  u 1:($2) w lines           \
  title "Match types reduce" \
,                            \
  "reduce-dependent.csv"     \
  u 1:($2) w lines           \
  title "Singletons reduce"  \
