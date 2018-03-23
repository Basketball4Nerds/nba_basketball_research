## this function returns TRUE if all vectors entered are equal
## https://stackoverflow.com/a/30850544
check_equal_vectors <- function(...) {
  vec_lst <- list(...)
  vec_classes <- unlist(lapply(vec_lst, class))
  if (length(unique(vec_classes)) > 1)
    stop('Input vectors are of different data types')
  return(length(unique(vec_lst))==1)
}


