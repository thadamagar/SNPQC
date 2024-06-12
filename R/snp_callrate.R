snp_callrate <-
function(genoM, threshold = 0.9) {
  SNPcalls <- rowSums(!is.na(genoM)) / ncol(genoM)
  highSNPcall <- sum(SNPcalls >= threshold) / nrow(genoM) * 100
  snp_call_rates <- data.frame(SNP = rownames(genoM), CallRate = SNPcalls,row.names = NULL )
  return(list(
    snp_call_rate = SNPcalls,
    high_snp_call_rate_percentage = highSNPcall,
    snp_call_rate_dataframe_head = head(snp_call_rates, 50)
  ))
}
