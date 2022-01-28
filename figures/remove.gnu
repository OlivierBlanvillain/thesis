load "figures/4x4.gnu"
set output "figures/remove.gex"
# set xlabel "~"
set xlabel "Input size"
set ylabel "~"

plot                         \
  "remove-implicits.csv"     \
  u 1:($2/1000) w lines lt 3 \
  title "Implicits remove"   \
,                            \
  "remove-dependent.csv"     \
  u 1:($2/1000) w lines lt 2 \
  title "Singletons remove"  \
,                            \
  "remove-match-types.csv"   \
  u 1:($2/1000) w lines lt 1 \
  title "Match types remove" \

set output "figures/remove-2.gex"

plot                         \
  "remove-match-types.csv"   \
  u 1:($2) w lines           \
  title "Match types remove" \
,                            \
  "remove-dependent.csv"     \
  u 1:($2) w lines           \
  title "Singletons remove"  \
