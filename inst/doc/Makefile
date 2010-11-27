bldcmd=R CMD Sweave

default:
	make draft
	make clean

enhanced:
	make draft bldcmd=makesweave

draft:
	make ofp1.pdf
	make ofp2.pdf

ofp1.pdf: ofp1.tex maia2.sty
	pdflatex ofp1.tex

ofp2.pdf: ofp2.tex maia2.sty
	pdflatex ofp2.tex

ofp1.tex: ofp1.rnw
	$(bldcmd) ofp1.rnw

ofp2.tex: ofp2.rnw
	$(bldcmd) ofp2.rnw

init:
	rm -f /tmp/makesweavepipe
	mkfifo /tmp/makesweavepipe
	echo -e "0\n" > /tmp/makesweavestatus
	makesweave -i &

clean:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f tmp*
	rm -f Rplots*


