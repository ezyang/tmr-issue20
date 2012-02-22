issue = Issue20

texsources = tuples.tex # Editorial.tex

default: $(issue).pdf

$(issue).tex : $(issue).lhs $(texsources) $(lhssources)
	lhs2TeX $(issue).lhs > $(issue).tex

%.pdf: %.tex force
	pdflatex $<

%.tex: %.lhs
	lhs2TeX $< -o $@

clean:
	rm -f *.log *.aux *.toc *.out *.blg *.bbl *.ptb *~
	rm -f $(issue).tex

bib : 
#	bibtex mighttpd

final : $(issue).pdf bib
	pdflatex $(issue).tex
	pdflatex $(issue).tex

.PHONY : force
