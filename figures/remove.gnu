load "figures/4x4.gnu"
set output "figures/remove.gex"
# set xlabel "~"
set xlabel "Input size"
set ylabel "~"

plot                         \
  "remove-implicits.csv"     \
  u 1:($2/1000) w lines lt 3 \
  title "Implicits Remove"   \
,                            \
  "remove-dependent.csv"     \
  u 1:($2/1000) w lines lt 2 \
  title "Singletons Remove"  \
,                            \
  "remove-match-types.csv"   \
  u 1:($2/1000) w lines lt 1 \
  title "Match types Remove" \

set output "figures/remove-2.gex"

plot                         \
  "remove-match-types.csv"   \
  u 1:($2) w lines           \
  title "Match types Remove" \
,                            \
  "remove-dependent.csv"     \
  u 1:($2) w lines           \
  title "Singletons Remove"  \
