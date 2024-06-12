plot_observed_heterozygosity <-
function(genoM) {
  hetObs <- colSums(genoM == 1, na.rm = TRUE) / nrow(genoM)
  library(ggplot2)
  ggplot(data.frame(hetObs), aes(x = hetObs)) +
    geom_histogram(binwidth = 0.03, fill = "blue", color = "black") +
    xlab("Observed Heterozygosity") +
    ggtitle("Distribution of Observed Heterozygosity") 
}
