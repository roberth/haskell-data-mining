default : presentation.pdf

preview : presentation.pdf
	open -a /Applications/Skim.app presentation.pdf

presentation.pdf : presentation.tex
	pdflatex presentation.tex </dev/null && pdflatex -synctex=1 presentation.tex </dev/null

presentation.tex : presentation.lhs
	lhs2TeX --agda -o presentation.tex presentation.lhs

clean :
	rm -f *.aux *.log *.nav *.out *.ptb *.toc *.snm presentation.pdf *.tex *.synctex.gz

