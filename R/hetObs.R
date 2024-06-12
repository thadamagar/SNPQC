hetObs <-
function(genoM, threshold = 0.3) {
  hetObs <- colSums(genoM == 1, na.rm = TRUE) / nrow(genoM)
  hetObs_high <- length(which(hetObs > threshold))
  hetObs_df <- data.frame(Genotype = colnames(genoM), Heterozygosity = hetObs, row.names = NULL)
  return(list(Number_of_sample_with_high_heterozygocity=hetObs_high,
              Heterozygocity_df=hetObs_df))
}
