MAF <-
function(genoM){
  freqAvg <- rowMeans(genoM, na.rm = TRUE) # average frequency per SNP
  p <- freqAvg / 2
  MAF<-sum(p < 0.9 & p > 0.1, na.rm = TRUE) 
  message("SNPs_within_the_MAF_range = ", MAF)
  valid_indices <- which(!is.na(p))
  out_of_range_indices <- valid_indices[!(p[valid_indices] < 0.9 & p[valid_indices] > 0.1)]
  SNPs_not_in_range <- data.frame(SNP = out_of_range_indices, Frequency = p[out_of_range_indices])
  return(list(
    SNPs_within_the_MAF_range = MAF,
    SNPs_not_in_range_head = head(SNPs_not_in_range, 50)
  ))
}
