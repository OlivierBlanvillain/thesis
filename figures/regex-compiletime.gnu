load "figures/2x1.gnu"
set output "figures/regex-compiletime.gex"
set ylabel "Compilation time ($ms$)"
set yrange [0:400]

plot "regex-compiletime.csv" using 0:2:3:xtic(1) with boxes lc rgb var ,\
     "regex-compiletime.csv" using 0:($2+16.7):2 with labels
