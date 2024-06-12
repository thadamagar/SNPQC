plot_allele_frequency <-
function(genoM) {
  freqAvg <- rowMeans(genoM, na.rm = TRUE) # average frequency per SNP
  p <- freqAvg / 2 
  library(ggplot2)
  ggplot(data.frame(p), aes(x = p)) +
    geom_histogram(binwidth = 0.03, fill = "blue", color = "black") +
    xlab("Allele Frequency") +
    ggtitle("Distribution of Allele Frequencies") 
}
