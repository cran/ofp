makeall:
	make makedev
	make clean

makedev:
	make ofp1.pdf
	make ofp2.pdf
	make ofp3.pdf

ofp1.pdf: ofp1.tex maia2.sty
	pdflatex ofp1.tex

ofp2.pdf: ofp2.tex maia2.sty
	pdflatex ofp2.tex

ofp3.pdf: ofp3.tex maia2.sty
	pdflatex ofp3.tex

ofp1.tex: ofp1.rnw
	R CMD Sweave ofp1.rnw

ofp2.tex: ofp2.rnw
	R CMD Sweave ofp2.rnw

ofp3.tex: ofp3.rnw
	R CMD Sweave ofp3.rnw

clean:
	rm *.tex
	rm *.aux
	rm *.log
	rm tmp*
	rm Rplots*

