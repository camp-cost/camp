# Nbody
#
set term postscript font "Helvetica,24" eps enhanced color
set style data histogram
set style fill solid border -1
set style histogram clustered
set output "temp.eps"
set xlabel "Number of processes"
set ylabel "Runtime (seconds)"
set key top right
set style histogram cluster gap 5

plot \
    "nbody.dat" using (($2+$3)/2):xticlabels(1) title 'Unoptimised', '' using (($4+$5)/2) title 'Async. optimised'

!epstopdf temp.eps --outfile fig-codegen-nbody.pdf && rm temp.eps
