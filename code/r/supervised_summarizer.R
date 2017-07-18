## these functions are derived from Manuel Amunategui's Github post 
## on the topic of Supervised Summarizer:
## http://amunategui.github.io/supervised-summarizer/index.html



won <- subset(master_df, won==TRUE)
lost <- subset(master_df, won==FALSE)


x <- c(summary(won[, 'line']))[1:6]
y <- c(summary(lost[, 'line']))[1:6]

x <- c(summary(won[, 'FGP_cumperf_cnf']))[1:6]
y <- c(summary(lost[, 'FGP_cumperf_cnf']))[1:6]

stats <- data.frame('ind'=c(1:6), 
                    'won.FGP_cumperf_gen'=x,
                    'lost.FGP_cumperf_gen'=y)

p <- ggplot(data=stats, aes(x = ind)) +
  geom_line(aes(y = won.FGP_cumperf_gen, colour = "won.FGP_cumperf_gen")) +
  geom_line(aes(y = lost.FGP_cumperf_gen, colour = "lost.FGP_cumperf_gen")) +
  scale_x_discrete(breaks = 1:6,
                   labels=c("min","1q","median","mean","3q","max"))
p


a <- (x + 17) / 34
b <- (y + 17) / 34
stats2 <- data.frame('ind'=c(1:6), 
                    'won.FGP_cumperf_gen'=a,
                    'lost.FGP_cumperf_gen'=b)

p2 <- ggplot(data=stats2, aes(x = ind)) +
  geom_line(aes(y = won.FGP_cumperf_gen, colour = "won.FGP_cumperf_gen")) +
  geom_line(aes(y = lost.FGP_cumperf_gen, colour = "lost.FGP_cumperf_gen")) +
  scale_x_discrete(breaks = 1:6,
                   labels=c("min","1q","median","mean","3q","max"))
p2




## this function takes in two summary stats and returns "predictive spread" 
## of a particular variable
calc_predictive_spread <- function(summary_stats_0, summary_stats_2) {

  x + 17
  y + 17
  spread <- (summary_stats_1[[1]] - summary_stats_0[[1]]) +
    (summary_stats_1[[2]] - summary_stats_0[[2]]) +
    (summary_stats_1[[3]] - summary_stats_0[[3]]) +
    (summary_stats_1[[4]] - summary_stats_0[[4]]) +
    (summary_stats_1[[5]] - summary_stats_0[[5]]) +
    (summary_stats_1[[6]] - summary_stats_0[[6]])
  
}


## 
GetSummaryPlot <- function(scaled_df_0, scaled_df_1, predictor_var, plot=TRUE) {
  require(ggplot2)
  
  summary_stats_0 <- c(summary(scaled_df_0[, predictor_var]))[1:6]
  summary_stats_1 <- c(summary(scaled_df_1[, predictor_var]))[1:6]
  summary_stats_df <- data.frame('ind'=1:6, 
                                 'stats0'=summary_stats_0, 
                                 'stats1'=summary_stats_1)
  
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