.ONESHELL:
.PHONY: FORCE
all: thesis.pdf

deps := proofs/structure.pdf
deps += scala/code-sections.tex
deps += figures/concat.eps
deps += figures/remove.eps
deps += figures/join.eps
deps += figures/numpy.eps
deps += figures/2early2late.eps

thesis.pdf: FORCE $(deps)
	latexmk -xelatex -time -f -interaction=nonstopmode -outdir=latex.out -auxdir=latex.out thesis.tex
	cp latex.out/thesis.pdf thesis.pdf
	echo && ./pplatex -b -i latex.out/thesis.log

clean: FORCE
	rm -rf proofs/structure.pdf
	rm -rf scala/code-sections.tex
	rm -rf latex.out/*
	rm -rf figures/*.eps

watch: FORCE
	git ls-files | entr make

publish: FORCE
	git clean -fdX
	make
	cp thesis.pdf docs
	git add -f docs/thesis.pdf
	git commit -m "Publish snapshot to GitHub pages"

scala/code-sections.tex: scala/generate-code-sections.py $(shell find scala -name "*.scala")
	python3 scala/generate-code-sections.py $(shell find scala -name "*.scala")

%.gex %.eps %.pdf: %.gnu
	gnuplot $*.gnu || exit
	epstopdf $*.eps --outfile=$*.pdf

%.pdf: %.dot
	dot -Tpdf $< -o $@.pdf # dot from the graphviz package
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH -sOutputFile=$@ $@.pdf
	mv $@.pdf $@
