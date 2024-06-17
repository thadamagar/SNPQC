hetObs <-
function(genoM, threshold = 0.3) {
  hetObs <- colSums(genoM == 1, na.rm = TRUE) / nrow(genoM)
  hetObs_high <- length(which(hetObs > threshold))
  list_high_hetObs<- data.frame(
    Sample = names( hetObs [ hetObs > threshold]),
    HetObs = hetObs[hetObs > threshold], row.names = NULL
  )
  hetObs_df <- data.frame(Genotype = colnames(genoM), Heterozygosity = hetObs, row.names = NULL)
  return(list(Number_of_sample_with_high_heterozygocity=hetObs_high,
              Df_high_hetObs = list_high_hetObs,
              Heterozygocity_df=hetObs_df))
}
