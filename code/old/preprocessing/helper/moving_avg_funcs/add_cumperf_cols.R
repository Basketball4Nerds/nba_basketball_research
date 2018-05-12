
## edit this function so it doesn't have to calculate cumsum and cumcnt cols each time it runs!!!!
## this function adds cumulative performance columns
add_cumperf_cols <- function(master_df, rnd_dgt=3, add_opp_cols=FALSE) {
  
  ## original cols
  orig_cols <- names(master_df)
  
  ## get cumsum columns required for oeff calculations
  p_cumsum_cols <- sort(colnames(master_df)[grepl('^p_cumsum_', colnames(master_df))])
  pA_cumsum_cols <- sort(colnames(master_df)[grepl('^pA_cumsum_', colnames(master_df))])
  pos_cumsum_cols <- sort(colnames(master_df)[grepl('^pos_cumsum_', colnames(master_df))])
  posA_cumsum_cols <- sort(colnames(master_df)[grepl('^posA_cumsum_', colnames(master_df))])
  FGM_cumsum_cols <- sort(colnames(master_df)[grepl('^FGM_cumsum_', colnames(master_df))])
  FGMA_cumsum_cols <- sort(colnames(master_df)[grepl('^FGMA_cumsum_', colnames(master_df))])
  FGA_cumsum_cols <- sort(colnames(master_df)[grepl('^FGA_cumsum_', colnames(master_df))])
  FGAA_cumsum_cols <- sort(colnames(master_df)[grepl('^FGAA_cumsum_', colnames(master_df))])
  rqP_cumsum_cols <- sort(colnames(master_df)[grepl('^rqP_cumsum_', colnames(master_df))])
  rqPA_cumsum_cols <- sort(colnames(master_df)[grepl('^rqPA_cumsum_', colnames(master_df))])
  
  ## get a vector of gm-cnt columns
  gm_cnt_cols <- sort(colnames(master_df)[grepl('^n_cumcnt_', colnames(master_df))])
  
  ## create cumperf column names
  oeff_cumperf_cols <- gsub('^p_cumsum', 'oeff_cumperf', p_cumsum_cols)
  oeffA_cumperf_cols <- gsub('^pA_cumsum', 'oeffA_cumperf', pA_cumsum_cols)
  FGP_cumperf_cols <- gsub('^FGM_cumsum', 'FGP_cumperf', FGM_cumsum_cols)
  FGPA_cumperf_cols <- gsub('^FGMA_cumsum', 'FGPA_cumperf', FGMA_cumsum_cols)
  rqP_cumperf_cols <- gsub('^rqP_cumsum', 'rqP_cumperf', rqP_cumsum_cols)
  rqPA_cumperf_cols <- gsub('^rqPA_cumsum', 'rqPA_cumperf', rqPA_cumsum_cols)
  pos_cumperf_cols <- gsub('^pos_cumsum', 'pos_cumperf', pos_cumsum_cols)
  posA_cumperf_cols <- gsub('^posA_cumsum', 'posA_cumperf', posA_cumsum_cols)
  
  ## offensive efficiency: points per possesion x100
  for (i in 1:length(oeff_cumperf_cols)) {
    master_df[[oeff_cumperf_cols[i]]] <- round((master_df[[p_cumsum_cols[i]]] / master_df[[pos_cumsum_cols[i]]]) * 100, rnd_dgt)
  }
  
  ## opponent offensive efficiency: points per possession x100
  for (i in 1:length(oeffA_cumperf_cols)) {
    master_df[[oeffA_cumperf_cols[i]]] <- round((master_df[[pA_cumsum_cols[i]]] / master_df[[posA_cumsum_cols[i]]]) * 100, rnd_dgt)
  }
  
  ## field goal percentage
  for (i in 1:length(FGP_cumperf_cols)) {
    master_df[[FGP_cumperf_cols[i]]] <- round(master_df[[FGM_cumsum_cols[i]]] / master_df[[FGA_cumsum_cols[i]]], rnd_dgt)
  }
  
  ## field goal percentage allowed
  for (i in 1:length(FGPA_cumperf_cols)) {
    master_df[[FGPA_cumperf_cols[i]]] <- round(master_df[[FGMA_cumsum_cols[i]]] / master_df[[FGAA_cumsum_cols[i]]], rnd_dgt)
  }
  
  ## regular quarter points
  for (i in 1:length(rqP_cumperf_cols)) {
    master_df[[rqP_cumperf_cols[i]]] <- round(master_df[[rqP_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
  }
  
  ## regular quarter points allowed
  for (i in 1:length(rqPA_cumperf_cols)) {
    master_df[[rqPA_cumperf_cols[i]]] <- round(master_df[[rqPA_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
  }
  
  ## possessions
  for (i in 1:length(pos_cumperf_cols)) {
    master_df[[pos_cumperf_cols[i]]] <- round(master_df[[pos_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
  }
  
  ## possessions allowed
  for (i in 1:length(posA_cumperf_cols)) {
    master_df[[posA_cumperf_cols[i]]] <- round(master_df[[posA_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
  }
  
  ## round digits
  master_df <- round_df(master_df, rnd_dgt)
  
  ## fill in opponent columns
  if (add_opp_cols) { 
    new_cols <- setdiff(names(master_df), orig_cols)
    master_df <- fill_in_opp_cols(master_df, cols=new_cols) 
  }
  
  ## return
  return(master_df)
}
