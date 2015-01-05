# tune `elsif (/^-xelatex$/)      {` of latexmk
$pdflatex = 'xelatex -shell-escape -interaction=nonstopmode -halt-on-error %O %S';
$pdf_mode = 1;
$dvi_mode = $postscript_mode = 0; 

# with latexmk -pvc
$pdf_previewer = 'llpp %S';
$pdf_update_method = 2; # send SIGHUP to llpp
