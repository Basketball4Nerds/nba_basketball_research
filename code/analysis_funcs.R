## this function takes in win pred acc df and plots  min_diff vs. acc 
plot_wpa <- function(wpa_df) {
  ggplot(wpa_df, aes(x=min_diff, y=acc, color=metric)) + 
    geom_point(aes(size=n_pred)) +
    geom_line(aes(group=metric)) + 
    facet_grid(. ~ min_n)
}
