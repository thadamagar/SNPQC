filter_genotype_matrix <-
function(genoM, snp_callrate_threshold = 0.7, hetObs_threshold = 0.3, sample_callrate_threshold = 0.7) {
  # Calculate allele frequencies
  freqAvg <- rowMeans(genoM, na.rm = TRUE)
  p <- freqAvg / 2
  fixedSNPs <- which(p == 0 | p == 1)
  
  # Calculate SNP call rate
  SNPcalls <- rowSums(!is.na(genoM)) / ncol(genoM)
  
  # Calculate observed heterozygosity
  hetObs <- colSums(genoM == 1, na.rm = TRUE) / nrow(genoM)
  
  # Calculate sample call rate
  sample_calls <- colSums(!is.na(genoM)) / nrow(genoM)
  
  # Filter genotype matrix based on the thresholds
  filtered_genoM <- genoM[-fixedSNPs, ]
  filtered_genoM <- filtered_genoM[which(SNPcalls > snp_callrate_threshold), ]
  filtered_genoM <- filtered_genoM[, which(hetObs < hetObs_threshold)]
  filtered_genoM <- filtered_genoM[, which(colSums(!is.na(filtered_genoM)) / nrow(filtered_genoM) > sample_callrate_threshold)]
  
  return(filtered_genoM)
}
