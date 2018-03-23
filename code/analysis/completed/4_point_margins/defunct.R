#### define functions
plot_quantile_marked_normal_distribution_histogram <- function(df, var, title=NULL) {
  mean <- mean(df[[var]])
  sd <- sd(df[[var]])
  p <- ggplot(df) + 
    geom_histogram(aes_string(x=var), binwidth=1) + 
    geom_vline(xintercept=mean, color="red") + 
    geom_vline(xintercept=mean + sd, color='blue') + 
    geom_vline(xintercept=mean - sd, color='blue') + 
    geom_vline(xintercept=mean + sd * 2, color='yellow') + 
    geom_vline(xintercept=mean - sd * 2, color='yellow') 
  if (!is.null(title)) { p <- p + ggtitle(title) }
  return(p)
}

plot_quantile_marked_histogram <- function(df, var, title=NULL) {
  qntl <- quantile(abs(df[[var]]), probs=c(0, 0.25, 0.5, 0.75, 0.9))  
  x <- paste0('abs(', var, ')')
  p <- ggplot(df) + 
    geom_histogram(aes_string(x=x), binwidth=1) + 
    geom_vline(xintercept=qntl[2], color='orange') +   
    geom_vline(xintercept=qntl[3], color='yellow') +   
    geom_vline(xintercept=qntl[4], color='green') + 
    geom_vline(xintercept=qntl[5], color='blue', linetype='dotted')
  if (!is.null(title)) { p <- p + ggtitle(title) }
  return(p)
}