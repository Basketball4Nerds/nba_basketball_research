## this function camel-cases a string
## http://www.stat.cmu.edu/~hseltman/files/camelCase.R
camelCase = function(sv, upper=FALSE, capIsNew=FALSE, alreadyTrimmed=FALSE) {
  if (!is.character(sv)) stop("'sv' must be a string vector")
  if (!alreadyTrimmed) sv = gsub("[[:space:]]*$", "", gsub("^[[:space:]]*", "", sv))
  if (capIsNew) {
    sv = gsub("([A-Z])", " \\1", sv)
    sv = gsub("^[[:space:]]", "", sv)
    sv = tolower(sv)
  }
  apart = strsplit(sv, split="[[:space:][:punct:]]")
  apart = lapply(apart, tolower)
  capitalize = function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
  if (upper) {
    apart = lapply(apart, capitalize)
  } else {
    apart = lapply(apart, function(x) c(x[1], capitalize(x[-1])))
  }
  return(sapply(apart, paste, collapse=""))
}

