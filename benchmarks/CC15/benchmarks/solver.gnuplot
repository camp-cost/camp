# Stencil
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
    "solver.dat" using (($2+$3+$4+$5+$6+$7)/6):xticlabels(1) title 'Unoptimised',\
    '' using (($8+$9+$10+$11+$12+$13)/6) title 'Async. optimised'

!epstopdf temp.eps --outfile fig-codegen-solver.pdf && rm temp.eps
