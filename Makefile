all: 2Signs_pain-intensity.md 2Signs_pain-vs-no-pain.md 2Signs_qol-pain-intensity.md 2Signs_qol-pain-vs-no-pain.md factor-analysis.md


.DELETE_ON_ERROR: 
2Signs_pain-intensity.md: 2Signs_pain-intensity.Rmd
	Rscript -e 'rmarkdown::render("2Signs_pain-intensity.Rmd")'
 
2Signs_pain-vs-no-pain.md: 2Signs_pain-vs-no-pain.Rmd
	Rscript -e 'rmarkdown::render("2Signs_pain-vs-no-pain.Rmd")'
 
2Signs_qol-pain-intensity.md: 2Signs_qol-pain-intensity.Rmd
	Rscript -e 'rmarkdown::render("2Signs_qol-pain-intensity.Rmd")'
 
2Signs_qol-pain-vs-no-pain.md: 2Signs_qol-pain-vs-no-pain.Rmd
	Rscript -e 'rmarkdown::render("2Signs_qol-pain-vs-no-pain.Rmd")'
 
factor-analysis.md: factor-analysis.Rmd
	Rscript -e 'rmarkdown::render("factor-analysis.Rmd")'
 
