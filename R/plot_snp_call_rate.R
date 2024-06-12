plot_snp_call_rate <-
function(genoM) {
  SNPcalls <- rowSums(!is.na(genoM)) / ncol(genoM)
  library(ggplot2)
  ggplot(data.frame(SNPcalls), aes(x = SNPcalls)) +
    geom_histogram(binwidth = 0.03, fill = "blue", color = "black") +
    xlab("SNP Call Rate") +
    ggtitle("Distribution of SNP Call Rates") 
}
