load "figures/4x4.gnu"
set output "figures/2early2late.gex"
set xlabel "Input size"
set ylabel "Compilation time (sec)"
set key width -5

plot                                   \
  "reduce-match-types-2early2late.csv" \
  u 1:($2/1000) w lines                \
  title "Match types reduce S2"        \
,                                      \
  "reduce-match-types.csv"             \
  u 1:($2/1000) w lines                \
  title "Match types reduce S1"        \
