load "figures/2x1.gnu"
set output "figures/regex-runtime.gex"
set ylabel "Execution time (${\\mu}s$)"
set yrange [0:300]

plot "regex-runtime.csv" using 0:2:3:xtic(1) with boxes lc rgb var ,\
     "regex-runtime.csv" using 0:($2+16.7):2 with labels
