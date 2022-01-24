load "figures/4x4.gnu"
set output "figures/2early2late.gex"
set xlabel "Input size"
set ylabel "Compilation time (sec)"

plot                                  \
  "numpy-match-types-2early2late.csv" \
  u 1:($2/1000) w lines               \
  title "2e2l reduce"                 \
,                                     \
  "numpy-match-types.csv"             \
  u 1:($2/1000) w lines               \
  title "match types reduce"          \
