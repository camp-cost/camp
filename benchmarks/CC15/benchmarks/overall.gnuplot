# MapReduce
#
set term postscript font "Helvetica,24" eps enhanced color
set output "temp.eps"
set xlabel ""
set ylabel "Speedup relative to sequential (times)"
set key top right
set logscale y
set xtic rotate by -45 scale 0
set style data histogram
set style fill solid border -1
set style histogram clustered
set style histogram cluster gap 5
set arrow from -1,1 to 7,1 nohead lc rgb '#808080'

plot \
    "overall.dat" using (1):xticlabels(1) title 'Sequential' lt 7, '' using  ($2/$3) title 'Speedup (64 proc.)' lt 5,\

!epstopdf temp.eps --outfile fig-codegen-overall.pdf && rm temp.eps
