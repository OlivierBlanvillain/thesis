load "figures/4x4.gnu"
set output "figures/join.gex"
set xlabel "Input size"
set ylabel "Compilation time (sec)"

plot                         \
  "join-implicits.csv"       \
  u 1:($2/1000) w lines lt 3 \
  title "Implicits Join"     \
,                            \
  "join-dependent.csv"       \
  u 1:($2/1000) w lines lt 2 \
  title "Singletons Join"    \
,                            \
  "join-match-types.csv"     \
  u 1:($2/1000) w lines lt 1 \
  title "Match types Join"   \

set output "figures/join-2.gex"
set ylabel "Compilation time (ms)"

plot                         \
  "join-match-types.csv"     \
  u 1:($2) w lines           \
  title "Match types Join"   \
,                            \
  "join-dependent.csv"       \
  u 1:($2) w lines           \
  title "Singletons Join"    \
