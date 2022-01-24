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

set key left top
set key width -3.5
set key samplen 1.1

set linetype 1 lw 2 dt 1
set linetype 2 lw 3 dt 2
set linetype 3 lw 3 dt 3

set datafile separator ","
set lmargin 0
set rmargin 0
