# Stencil
#
set term postscript font "Helvetica,20" eps enhanced color
set output "temp.eps"
set xlabel "Number of processes"
set ylabel "Runtime (seconds)"
set xrange [1:64]
set key top right

plot \
    "stencil-opti.dat" using 1:(($2+$3+$4+$5+$6+$7+$8+$9+$10+$11+$12+$13+$14+$15+$16+$17+$18+$19+$20+$21+$22+$23+$24+$25+$26+$27+$28+$29+$30+$31+$32)/32) title 'Optimised'             with linespoints lt 1 lw 4,\
    "stencil.dat"      using 1:(($2+$3+$4+$5+$6+$7+$8+$9+$10+$11+$12+$13+$14+$15+$16+$17+$18+$19+$20+$21+$22+$23+$24+$25+$26+$27+$28+$29+$30+$31+$32)/32) title 'Direct implementation' with linespoints lt 2 lw 4

!epstopdf temp.eps --outfile fig-codegen-stencil.pdf && rm temp.eps
