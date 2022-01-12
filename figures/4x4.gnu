set terminal epslatex dashed
set size 0.65
set size ratio 0.8
set loadpath "figures"

set style line 12 lc rgb"#808080" lt 0 lw 1
set border 3 back # ls 11
set tics nomirror out scale 0.75
set grid back ls 12

set xtics (  \
  "0"    0,  \
  ""    25,  \
  "50"  50,  \
  ""    75,  \
  "100" 100, \
  ""    125, \
  "150" 150, \
  ""    175, \
  "200" 200, \
  ""    225, \
  "250" 250, )
set xrange [0:250]

set style line 1  lc rgb "#A6CEE3"
set style line 2  lc rgb "#1F78B4"
set style line 3  lc rgb "#B2DF8A"
set style line 4  lc rgb "#33A02C"
set style line 5  lc rgb "#FB9A99"
set style line 6  lc rgb "#E31A1C"
set style line 7  lc rgb "#FDBF6F"
set style line 8  lc rgb "#FF7F00"
set style line 9  lc rgb "#999999"
set style line 10 lc rgb "#000000"

set key left top
set key width -3.5
set key samplen 1.1


set datafile separator ","
set lmargin 0
set rmargin 0
