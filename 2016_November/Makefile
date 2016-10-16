analysis:
	mkdir ideal_points
	Rscript analysis.R
	rm -f Rplots.pdf

analysis_2d:
	mkdir ideal_points_2
	Rscript analysis_2d.R
	rm -f Rplots.pdf

clean:
	rm -rf ideal_points
	rm -rf ideal_points_2

all: analysis analysis_2d
	echo "Output will be stored in the ideal_points directory"
