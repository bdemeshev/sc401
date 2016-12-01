all: clean pdf_noclean html epub mobi copy

clean:
	export LC_ALL=en_US.UTF-8
	export LC_CTYPE=en_US.UTF-8
	rm -rf `biber --cache`
	-rm -rfv _book
	-rm lecture_notes_sc.run.xml
	-rm lecture_notes_sc.Rmd
	-rm -fvr *.log
	Rscript -e "bookdown::clean_book(TRUE)"

html:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook")'
	cp -fvr images _book/
	cp -fvr css/style.css _book/

epub:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::epub_book")'
	cp -fvr _book/lecture_notes_sc.epub _book/

mobi: _book/lecture_notes_sc.epub
	Rscript -e 'bookdown::kindlegen("_book/lecture_notes_sc.epub")'
	cp -fvr _book/lecture_notes_sc.mobi _book/


pdf:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book")'
	cp -fvr _book/lecture_notes_sc.pdf p_book/

pdf_noclean:
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book", clean=FALSE)'
	cp -fvr _book/lecture_notes_sc.pdf _book/

site:
	Rscript -e 'rmarkdown::render_site(encoding = "UTF-8")'
	# should test

install:
	Rscript -e 'devtools::install_github("bdemeshev/lecture_notes_sc_book")'
	# what the hell?

deploy:
	Rscript -e 'bookdown::publish_book(render="local", account="bdemeshev")'
	# what the hell?

copy:
	-rm -rfv ../../../docs/notes_ranepa_2016
	cp -rfv _book ../../../docs/notes_ranepa_2016




cleaner:
	make clean
	rm -frv *.aux *.out *.toc # Latex output
