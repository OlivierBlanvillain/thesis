load "figures/4x4.gnu"
set output "figures/remove.gex"
# set xlabel "~"
set xlabel "Input size"
set ylabel "~"

plot                        \
  "remove-implicits.csv"    \
  u 1:($2/1000)             \
  w lines lw 3 dt 2         \
  title "implicits remove"  \
,                           \
  "remove-dependent.csv"    \
  u 1:($2/1000)             \
  w lines lw 3              \
  title "singletons remove" \
,                           \
  "remove-match-types.csv"  \
  u 1:($2/1000)             \
  w lines lw 3              \
  title "match types remove"

set output "figures/remove-2.gex"

plot                        \
  "remove-dependent.csv"    \
  u 1:($2)                  \
  w lines lw 3              \
  title "singletons remove" \
,                           \
  "remove-match-types.csv"  \
  u 1:($2)                  \
  w lines lw 3              \
  title "match types remove"
