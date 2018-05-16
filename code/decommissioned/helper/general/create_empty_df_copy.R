## this function takes in a df and returns an empty copy of the original df
create_empty_df_copy <- function(df) {
  empty_df_copy <- as.data.frame(matrix(NA, nrow=nrow(df), ncol=ncol(df)))
  colnames(empty_df_copy) <- colnames(df)
  rownames(empty_df_copy) <- rownames(df)
  return(empty_df_copy)
}

