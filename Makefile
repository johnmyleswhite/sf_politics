analysis:
	Rscript analysis.R

clean:
	rm ideal_points/*

all: analysis
	echo "Output will be stored in the ideal_points directory"
