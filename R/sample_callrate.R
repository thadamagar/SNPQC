sample_callrate <-
function(genoM, threshold = 0.9) {
  sample_calls <- colSums(!is.na(genoM)) / nrow(genoM)
  highsamplecall <- sum(sample_calls >= threshold) / ncol(genoM) * 100
  low_call_samples <- data.frame(
    Sample = names(sample_calls[sample_calls < threshold]),
    CallRate = sample_calls[sample_calls < threshold], row.names = NULL
  )
  return(list(
    sample_call_rate = sample_calls,
    high_sample_call_rate_percentage = highsamplecall,
    low_call_rate_samples = low_call_samples
  ))
}
