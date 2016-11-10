
all: README.md README.html

app.R: README.Rmd
	R -e 'knitr::purl("$<", "$@", quiet = TRUE, documentation = 2L)'

README.md: README.Rmd
	R -e 'knitr::knit("$<", "$@", quiet = TRUE)'

README.html: README.Rmd
	R -e 'knitr::knit("$<", "$@", quiet = TRUE)'
