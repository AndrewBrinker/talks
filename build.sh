pandoc --template=base.template --highlight-style=tango --variable mainfont=Verdana --variable sansfont=Helvetica --variable monofont="Menlo" --variable fontsize=10pt --latex-engine=xelatex --toc -s cool.md -o cool.tex
xelatex cool.tex
