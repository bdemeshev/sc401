# makefile: tex -> pdf via xelatex
# v 2.0
# .tex extension is automatically added
file_name_01 = sc_icef_lect_01
file_name_02 = sc_icef_lect_02

all: $(file_name_01).pdf $(file_name_02).pdf

$(file_name_01).pdf: $(file_name_01).tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_01).tex

	# clean auxillary files
	latexmk -c $(file_name_01).tex

# $(file_name).tex : $(file_name).Rnw
#	Rscript -e "library(knitr); knit('$(file_name).Rnw')"

$(file_name_02).pdf: $(file_name_02).tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex $(file_name_02).tex

	# clean auxillary files
	latexmk -c $(file_name_02).tex

# $(file_name).tex : $(file_name).Rnw
#	Rscript -e "library(knitr); knit('$(file_name).Rnw')"


clean:
	-rm $(file_name_01).aux $(file_name_01).log $(file_name_01).fls $(file_name_01).fdb_latexmk $(file_name_01).pdf $(file_name_02).aux $(file_name_02).log $(file_name_02).fls $(file_name_02).fdb_latexmk $(file_name_02).pdf
