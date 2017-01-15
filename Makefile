# Set additional directories 'make' must search for dependencies in
VPATH = data input_scripts

# Create dummy targets to ensure all intermediate targets are 'made'
.PHONY: all

all: pain-vs-no-pain.md pain-intensity.md qol-pain-vs-no-pain.md qol-pain-intensity.md

pain-intensity.md: inputs/pain-intensity.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', out_dir = '.', fig_dir = 'figures/pain-intensity', keep_md = FALSE)"

pain-vs-no-pain.md: inputs/pain-vs-no-pain.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', out_dir = '.', fig_dir = 'figures/pain-vs-no-pain', keep_md = FALSE)"

qol-pain-intensity.md: inputs/qol-pain-intensity.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', out_dir = '.', fig_dir = 'figures/qol-pain-intensity', keep_md = FALSE)"

qol-pain-vs-no-pain.md: inputs/qol-pain-vs-no-pain.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', out_dir = '.', fig_dir = 'figures/qol-pain-vs-no-pain', keep_md = FALSE)"

factor-analysis.md: inputs/factor-analysis.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', out_dir = '.', fig_dir = 'figures/factor-analysis', keep_md = FALSE)"

