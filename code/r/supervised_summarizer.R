## these functions are derived from Manuel Amunategui's Github post 
## on the topic of Supervised Summarizer:
## http://amunategui.github.io/supervised-summarizer/index.html


## this function takes into two comparable summary stats vectors and 
## returns a list of normalized summary stats vectors
get_nrm_smry_stats_lst <- function(smry_stats_0, smry_stats_1, rnd_dgt=3) {
  
  ## in case summary stats vectors are unnamed
  names(smry_stats_0) <- names(smry_stats_1) <- c('min', '1q', 'med', '3q', 'max')
  
  ## make sure min starts from 0
  min <- min(smry_stats_0['min'], smry_stats_1['min'])
  nrm_smry_stats_0 <- smry_stats_0 - min
  nrm_smry_stats_1 <- smry_stats_1 - min

  ## make sure max caps at 1
  max <- max(nrm_smry_stats_0['max'], nrm_smry_stats_1['max'])
  nrm_smry_stats_0 <- round(nrm_smry_stats_0 / max, rnd_dgt)
  nrm_smry_stats_1 <- round(nrm_smry_stats_1 / max, rnd_dgt)
  
  ## return 
  return(list(nrm_smry_stats_0, nrm_smry_stats_1))
}


## this function takes in two summary stats and returns summary stats spread
## of a particular variable
calc_smry_stats_spread <- function(smry_stats_0, smry_stats_1, normalize=TRUE, rnd_dgt=3, abs=TRUE) {
  
  ## error case
  if (length(smry_stats_0) != 5 || length(smry_stats_1) != 5)
    stop('Please provide summary stats vectors of length 5.')
  
  ## normalize summary stats vectors if specified
  if (normalize) {
    nrm_smry_stats_lst <- get_nrm_smry_stats_lst(smry_stats_0, smry_stats_1, rnd_dgt=rnd_dgt)
    smry_stats_0 <- nrm_smry_stats_lst[[1]]
    smry_stats_1 <- nrm_smry_stats_lst[[2]]
  }

  ## calculate spread from summary stats vectors
  spread <- 
    (smry_stats_1[[1]] - smry_stats_0[[1]]) + 
    (smry_stats_1[[2]] - smry_stats_0[[2]]) +
    (smry_stats_1[[3]] - smry_stats_0[[3]]) +
    (smry_stats_1[[4]] - smry_stats_0[[4]]) +
    (smry_stats_1[[5]] - smry_stats_0[[5]]) 
  
  ## round
  spread <- round(spread, rnd_dgt)
  
  ## apply absolute (relative size is important; not +/- direction)
  spread <- ifelse(abs, abs(spread), spread)
  
  ## return
  return(spread)
}


## this function takes summary stats df (consists of two summary stats vectors)
## and plots the spread
plot_smry_stats_spread <- function(df_0, df_1, predictor_var, normalize=TRUE, abs=TRUE) {

  ## get summary stats vectors
  smry_stats_0 <- c(summary(df_0[, predictor_var]))[c(1:3, 5:6)]
  smry_stats_1 <- c(summary(df_1[, predictor_var]))[c(1:3, 5:6)]

  ## calculate spread
  spread <- calc_smry_stats_spread(smry_stats_0, smry_stats_1, normalize=normalize, rnd_dgt=rnd_dgt, abs=abs)
  
  ## create summary stats df
  smry_stats_df <- data.frame(ind=1:5, 
                              smry_stats_0=smry_stats_0, 
                              smry_stats_1=smry_stats_1)

  ## create proper x-label
  xlab <- ifelse(normalize, 
                 paste('Normalized spread', spread), 
                 paste('Spread:', spread))
  
  ## create plot
  p <- ggplot(data=smry_stats_df, aes(x=ind)) +
    geom_line(aes(y=smry_stats_0, colour="smry_stats_0")) +
    geom_line(aes(y=smry_stats_1, colour="smry_stats_1")) +
    scale_x_discrete(limits=1:5,
                     labels=c("min", "1q", "median", "3q", "max")) +
    xlab(xlab) + 
    ylab(predictor_var) 

  ## return
  return(p)
}


## this function d
create_varimp_df <- function(df_0, df_1, predictor_vars, normalize=FALSE, rnd_dgt=3, abs=TRUE) {

  ## initialize empty vector to store spread 
  spread_vec <- c()

  ## for each predictor variable
  for (predictor_var in predictor_vars) {
    
    ## get summary stats vectors
    smry_stats_0 <- c(summary(df_0[, predictor_var]))[c(1:3, 5:6)]
    smry_stats_1 <- c(summary(df_1[, predictor_var]))[c(1:3, 5:6)]
    
    ## calculate spread
    spread <- calc_smry_stats_spread(smry_stats_0, smry_stats_1, normalize=normalize, rnd_dgt=rnd_dgt, abs=abs)
    
    ## add to vector
    spread_vec <-  c(spread_vec, spread)
  }
  
  ## create variable importance df
  varimp_df <- data.frame(var=predictor_vars, spread=abs(spread_vec))

  ## return
  return(varimp_df)
}






