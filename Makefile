
all: README.md README.html app.R

## We need to postprocess because of a knitr::purl bug
## http://stackoverflow.com/questions/41038256

app.R: README.Rmd postprocess.R
	R -e 'knitr::purl("$<", "$@", quiet = TRUE, documentation = 2L)'
	R -e 'source("postprocess.R"); postprocess("app.R")'

README.md: README.Rmd
	R -e 'knitr::knit("$<", "$@", quiet = TRUE)'

README.html: README.Rmd
	R -e 'knitr::knit("$<", "$@", quiet = TRUE)'
