# makefile: Rnw -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added
file_name = sc_problems_main

$(file_name).pdf: $(file_name).tex sc_pr_all.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -xelatex -pdf $(file_name).tex

	# clean auxillary files
	latexmk -c $(file_name).tex

#	$(file_name).tex : $(file_name).Rnw
#		Rscript -e "library(knitr); knit('$(file_name).Rnw')"

clean:
	rm -rf `biber --cache`
	latexmk -c $(file_name).tex
	-rm $(file_name).pdf $(file_name).aux $(file_name).log $(file_name).flx $(file_name).idx $(file_name).out 
