# MapReduce
#
set term postscript font "Helvetica,24" eps enhanced color
set output "temp.eps"
set xlabel "Number of processes"
set ylabel "Runtime (seconds)"
set logscale y
set key top right
set style data histogram
set style fill solid border -1
set style histogram clustered
set style histogram cluster gap 5

plot \
    "mr_wc.dat" using (($2+$3)/2):xticlabels(1) title 'Word Count' lt 3,\
    ""          using (($4+$5)/2)               title 'AdPredictor' lt 4

!epstopdf temp.eps --outfile fig-codegen-mapreduce.pdf && rm temp.eps
