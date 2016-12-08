
postprocess <- function(file) {

  lines <- readLines(file)

  include <- grep("^## ---.*purl = TRUE.*$", lines)
  empty <- grep("^\\s*$", lines, perl = TRUE)

  do_chunk <- function(start) {
    start <- start + 1L
    if (start > length(lines)) return()
    ## first empty line after start
    end <- empty[empty >= start][1]
    if (is.na(end)) end <- length(lines)
    lines[start:end] <<- sub("^## ", "", lines[start:end])
  }
  
  lapply(include, do_chunk)

  writeLines(lines, con = file)
}
