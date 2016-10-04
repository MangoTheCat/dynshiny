
all: app.R README.md README.html

app.R: README.Rmd
	R -e 'knitr::purl("$<", "$@", quiet = TRUE, documentation = 2L)'
	sed -i .bak -e '/^## ----eval/d' $@
	sed -i .bak -e '/^## ----*$$/d' $@
	sed -i .bak -e 's/^## //' $@

README.md: README.Rmd
	R -e 'knitr::knit("$<", "$@", quiet = TRUE)'

README.html: README.Rmd
	R -e 'knitr::knit("$<", "$@", quiet = TRUE)'
