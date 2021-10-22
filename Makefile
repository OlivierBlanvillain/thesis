.ONESHELL:
.PHONY: FORCE
all: thesis.pdf

thesis.pdf: FORCE proofs/structure.pdf code-sections.tex
	./latexrun --latex-cmd lualatex thesis.tex

clean: FORCE
	./latexrun --clean-all

watch: FORCE
	git ls-files | entr make

publish: FORCE
	- git clean -fdX
	- make
	- cp thesis.pdf docs
	- git add -f docs/thesis.pdf
	- git commit -m "Publish snapshot to GitHub pages"

code-sections-dep := $(shell git ls | grep .scala$)
code-sections.tex: $(code-sections-dep)
	python3 scala/generate-code-sections.py $(code-sections-dep)

%.pdf: %.dot
	- dot -Tpdf $< -o $@.pdf # dot from the graphviz package
	- gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dNOPAUSE -dBATCH -sOutputFile=$@ $@.pdf
	- mv $@.pdf $@
