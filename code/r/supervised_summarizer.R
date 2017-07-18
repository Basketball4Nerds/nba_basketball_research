## these functions are derived from Manuel Amunategui's Github post 
## on the topic of Supervised Summarizer:
## http://amunategui.github.io/supervised-summarizer/index.html


## this function takes in two summary stats and returns "predictive spread" 
## of a particular variable
calc_predictive_spread <- function(smry_stats_0, smry_stats_1, normalize=TRUE) {
  
  ## normalize summary stats vectors if specified
  if (normalize) {
    nrm_smry_stats_lst <- get_nrm_smry_stats_lst(smry_stats_0, smry_stats_1)
    smry_stats_0 <- nrm_smry_stats_lst[[1]]
    smry_stats_1 <- nrm_smry_stats_lst[[2]]
  }

  ## calculate spread from summary stats vectors
  spread <- (smry_stats_1[[1]] - smry_stats_0[[1]]) +
    (smry_stats_1[[2]] - smry_stats_0[[2]]) +
    (smry_stats_1[[3]] - smry_stats_0[[3]]) +
    (smry_stats_1[[4]] - smry_stats_0[[4]]) +
    (smry_stats_1[[5]] - smry_stats_0[[5]]) +
    (smry_stats_1[[6]] - smry_stats_0[[6]])
  
  ## return
  return(spread)
}


## this function takes into two comparable summary stats vectors and 
## returns a list of normalized summary stats vectors
get_nrm_smry_stats_lst <- function(smry_stats_0, smry_stats_1, rnd_dgt=3) {
  
  ## make sure min starts from 0
  min <- min(smry_stats_0['Min.'], smry_stats_1['Min.'])
  nrm_smry_stats_0 <- smry_stats_0 - min
  nrm_smry_stats_1 <- smry_stats_1 - min
  
  ## make sure max caps at 1
  max <- max(nrm_smry_stats_0['Max.'], nrm_smry_stats_1['Max.'])
  nrm_smry_stats_0 <- round(nrm_smry_stats_0 / max, rnd_dgt)
  nrm_smry_stats_1 <- round(nrm_smry_stats_1 / max, rnd_dgt)
  
  ## return 
  return(list(nrm_smry_stats_0, nrm_smry_stats_1))
}


## 
won_df <- subset(predictive_df, won)
lost_df <- subset(predictive_df, !won)


x <- c(summary(won_df[, 'line']))[1:6]
y <- c(summary(lost_df[, 'line']))[1:6]
z <- normalize_summary_stats(x, y)

x <- c(summary(won_df[, 'rqP_cumperf_gen']))[1:6]
y <- c(summary(lost_df[, 'rqP_cumperf_gen']))[1:6]
z <- normalize_summary_stats(x, y)
z[[1]]
z[[2]]

x <- c(summary(won_df[, 'FGP_cumperf_cnf']))[1:6]
y <- c(summary(lost_df[, 'FGP_cumperf_cnf']))[1:6]
x
y
stats <- data.frame('ind'=c(1:6), 
                    'won.FGP_cumperf_gen'=x,
                    'lost.FGP_cumperf_gen'=y)

p <- ggplot(data=stats, aes(x = ind)) +
  geom_line(aes(y = won.FGP_cumperf_gen, colour = "won.FGP_cumperf_gen")) +
  geom_line(aes(y = lost.FGP_cumperf_gen, colour = "lost.FGP_cumperf_gen")) +
  scale_x_discrete(breaks = 1:6,
                   labels=c("min","1q","median","mean","3q","max"))
p


stats2 <- data.frame('ind'=c(1:6), 
                    'won.FGP_cumperf_gen'=a,
                    'lost.FGP_cumperf_gen'=b)

p2 <- ggplot(data=stats2, aes(x = ind)) +
  geom_line(aes(y = won.FGP_cumperf_gen, colour = "won.FGP_cumperf_gen")) +
  geom_line(aes(y = lost.FGP_cumperf_gen, colour = "lost.FGP_cumperf_gen")) +
  scale_x_discrete(breaks = 1:6,
                   labels=c("min","1q","median","mean","3q","max"))
p2






## 
GetSummaryPlot <- function(scaled_df_0, scaled_df_1, predictor_var, plot=TRUE) {
  
  ## get summary stats vectors
  summary_stats_0 <- c(summary(scaled_df_0[, predictor_var]))[1:6]
  summary_stats_1 <- c(summary(scaled_df_1[, predictor_var]))[1:6]
  
  ## create summary stats df
  summary_stats_df <- data.frame('ind'=1:6, 
                                 'stats0'=summary_stats_0, 
                                 'stats1'=summary_stats_1)

  ## calculate spread
  spread <- calc_predictive_spread(summary_stats_0, summary_stats_1, normalize=TRUE)
    
  if (plot) {
    
    print(paste('Scaled spread:', spread))
    p <- ggplot(data=stats, aes(ind)) +
      geom_line(aes(y = stats1, colour = "stats1")) +
      geom_line(aes(y = stats0, colour = "stats0")) +
      scale_x_discrete(breaks = 1:6,
                       labels=c("min","1q","median","mean","3q","max")) +
      ylab(predictor_var) + xlab(paste('Spread:',spread))
    return (p)
  } else {
    return (spread)
  }
}