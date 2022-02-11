load "figures/2x1.gnu"
set output "figures/regex-compiletime.gex"
set ylabel "Compilation time ($ms$)"
set yrange [0:1800]
set ytics 300

plot "regex-compiletime.csv" using 0:2:3:xtic(1) with boxes lc rgb var ,\
     "regex-compiletime.csv" using 0:($2+100):2 with labels
