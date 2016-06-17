pdflatex formula.tex
gs -sDEVICE=pngalpha -o formula.png -r170 formula.pdf
rm formula.log formula.aux formula.pdf
mv formula.png ../public/img/
