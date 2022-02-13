.ONESHELL:
.PHONY: FORCE
all: thesis.pdf

deps := proofs/structure.pdf
deps += scala/code-sections.tex
deps += figures/concat.eps
deps += figures/remove.eps
deps += figures/join.eps
deps += figures/reduce.eps
deps += figures/2early2late.eps
deps += figures/regex-compiletime.eps
deps += figures/regex-runtime.eps

thesis.pdf: FORCE $(deps)
	latexmk -xelatex -time -f -interaction=nonstopmode -outdir=latex.out -auxdir=latex.out thesis.tex
	cp latex.out/thesis.pdf thesis.pdf
	echo && ./pplatex -b -i latex.out/thesis.log

clean: FORCE
	rm -rf proofs/structure.pdf
	rm -rf scala/code-sections.tex
	rm -rf latex.out/*
	rm -rf figures/*.eps

cleanfigures: FORCE
	rm -rf figures/*.eps

watch: FORCE
	git ls-files | entr make

publish: FORCE
	make
	cp thesis.pdf docs
	rm -rf docs/benchmarks.zip
	zip docs/benchmarks.zip -- run-benchmarks.sh $$(git ls-files scala | grep -v .py | grep -v regex)
	rm -rf docs/sources.zip
	zip docs/sources.zip -- $$(git ls-files)
	git add -f docs/*
	git commit -m "Publish snapshot to GitHub pages"

scala/code-sections.tex: scala/generate-code-sections.py $(shell find scala -name "*.scala")
	python3 scala/generate-code-sections.py $(shell find scala -name "*.scala")

%.gex %.eps %.pdf: %.gnu figures/2x1.gnu figures/4x4.gnu
	gnuplot $*.gnu || exit
	epstopdf $*.eps --outfile=$*.pdf

%.pdf: %.dot
	dot -Tpdf $< -o $@.pdf # dot from the graphviz package
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH -sOutputFile=$@ $@.pdf
	mv $@.pdf $@
