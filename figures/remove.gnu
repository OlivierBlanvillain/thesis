load "figures/4x4.gnu"
set output "figures/remove.gex"
# set xlabel "~"
set xlabel "Input size"
set ylabel "~"

plot                         \
  "remove-implicits.csv"     \
  u 1:($2/1000) w lines lt 3 \
  title "implicits remove"   \
,                            \
  "remove-dependent.csv"     \
  u 1:($2/1000) w lines lt 2 \
  title "singletons remove"  \
,                            \
  "remove-match-types.csv"   \
  u 1:($2/1000) w lines lt 1 \
  title "match types remove" \

set output "figures/remove-2.gex"

plot                         \
  "remove-match-types.csv"   \
  u 1:($2) w lines           \
  title "match types remove" \
,                            \
  "remove-dependent.csv"     \
  u 1:($2) w lines           \
  title "singletons remove"  \
