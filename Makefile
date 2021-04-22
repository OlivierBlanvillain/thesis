.ONESHELL:
.PHONY: FORCE
all: thesis.pdf

thesis.pdf: FORCE
	./latexrun --latex-cmd pdflatex thesis.tex

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
