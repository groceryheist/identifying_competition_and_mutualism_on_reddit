#!/usr/bin/make
all: $(patsubst %.tex,%.pdf,$(wildcard *.tex))

# use the following section for Rnw/knitr documents
# all: $(patsubst %.Rnw,%.pdf,$(wildcard *.Rnw))
# %.tex: %.Rnw
#	Rscript -e "library(knitr); knit('$<')"

%.pdf: %.tex resources/*.tex resources/*.png
	latexmk -f -pdf $<

clean: 
	latexmk -C *.tex
	rm -f *.tmp *.run.xml
	rm -f vc
	rm -f *.bbl
	# the following lines are useful for Rnw/knitr
	# rm -rf cache/ figure/
	# rm -f *.tex

viewpdf: all
	evince *.pdf

vc:	resources/vc-git

pdf: all

.PHONY: clean all
# .PRECIOUS: %.tex

vpath %.tex resources

