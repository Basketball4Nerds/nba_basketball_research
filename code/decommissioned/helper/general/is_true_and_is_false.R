
## this function determines if a value is TRUE;
## (can work with NA values)
is.true <- function(x) {
  !is.na(x) & x
}


## this function determines if a value is FALSE;
## (can work with NA values)
is.false <- function(x) {
  !is.na(x) & !x
}


