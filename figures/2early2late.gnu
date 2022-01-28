load "figures/4x4.gnu"
set output "figures/2early2late.gex"
set xlabel "Input size"
set ylabel "Compilation time (sec)"
set key width -5

plot                                  \
  "numpy-match-types-2early2late.csv" \
  u 1:($2/1000) w lines               \
  title "Match types Reduce S1"       \
,                                     \
  "numpy-match-types.csv"             \
  u 1:($2/1000) w lines               \
  title "Match types Reduce S2"       \
