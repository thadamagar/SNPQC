plot_sample_call_rate <-
function(genoM) {
  sample_calls <- colSums(!is.na(genoM)) / nrow(genoM)
  library(ggplot2)
  ggplot(data.frame(sample_calls), aes(x = sample_calls)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
    xlab("Sample Call Rate") +
    ggtitle("Distribution of Sample Call Rates") 
}
