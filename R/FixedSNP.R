FixedSNP <-
function(genoM) {
  freqAvg <- rowMeans(genoM, na.rm = TRUE) # average frequency per SNP
  p <- freqAvg / 2 # frequency for one of the alleles
  p_filtered <- p[is.finite(p)]
  fixedSNPs <- sum(p == 0, na.rm = TRUE) + sum(p == 1, na.rm = TRUE) # count fixed SNPs
  list_fixedSNPs <- which(p == 0 | p == 1) 
  return(list(
    Number_of_fixedSNPs = fixedSNPs,
    list_of_fixed_SNPs = list_fixedSNPs
  ))
}
