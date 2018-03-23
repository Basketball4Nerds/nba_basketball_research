## this function creates odds df by combining 
## expected spreads df and expected total points df
createOddsDf <- function(spreads, totals) {
  
  ## remove payout column from the dfs
  spreads$payout <- totals$payout <- NULL
  
  ## merge dfs
  odds <- merge(spreads, totals, by=c('season', 'date', 'team', 'o_team'), all=TRUE)
  
  ## create projection cols
  odds$pts_proj_om <- (odds$total / 2) - (odds$adjustor / 2)
  odds$pts_alwd_to_o_proj_om <- (odds$total / 2) + (odds$adjustor / 2)
  odds$pts_margin_proj_om <- -odds$adjustor
  odds$win_proj_om <- ifelse(odds$pts_margin_proj_om > 0, TRUE,
                             ifelse(odds$pts_margin_proj_om < 0, FALSE, NA))
  
  ## return
  return(odds)
}


