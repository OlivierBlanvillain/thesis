load "figures/4x4.gnu"
set output "figures/concat.gex"
set ylabel "Compilation time (sec)"
set xlabel "Input size"
# set xlabel "~"

plot                         \
  "concat-implicits.csv"     \
  u 1:($2/1000) w lines lt 3 \
  title "Implicits concat"   \
,                            \
  "concat-dependent.csv"     \
  u 1:($2/1000) w lines lt 2 \
  title "Singletons concat"  \
,                            \
  "concat-match-types.csv"   \
  u 1:($2/1000) w lines lt 1 \
  title "Match types concat" \

set output "figures/concat-2.gex"
set ylabel "Compilation time (ms)"

plot                         \
  "concat-match-types.csv"   \
  u 1:($2) w lines           \
  title "Match types concat" \
,                            \
  "concat-dependent.csv"     \
  u 1:($2) w lines           \
  title "Singletons concat"  \
