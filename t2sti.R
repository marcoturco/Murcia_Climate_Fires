# File: t2sti.R

STI <- function(tmean, sc) {
  # Compute the Standardized Temperature Index (STI) with monthly temperature data (tmean).
  #
  # Args:
  #   tmean: Numeric vector of monthly temperature data.
  #   sc:    Integer specifying the aggregation scale in months (e.g., 3, 6, 12).
  #
  # Returns:
  #   Numeric vector of STI values with the same length as tmean.
  #   The first (sc - 1) values are NA due to the aggregation window.
  
  # Validate inputs
  if (!is.numeric(tmean)) {
    stop("tmean must be a numeric vector.")
  }
  if (!is.numeric(sc) || length(sc) != 1 || sc <= 0) {
    stop("sc must be a single positive integer.")
  }
  
  nm <- length(tmean)  # Total number of time steps
  
  if (nm < sc) {
    warning("Length of tmean is less than the aggregation scale. Returning a vector of NAs.")
    return(rep(NA, nm))
  }
  
  # Initialize the STI vector with NAs
  STI_values <- rep(NA, nm)
  
  # Compute aggregated values using a moving window
  # Each aggregated value is the mean over 'sc' consecutive months
  for (i in sc:nm) {
    window <- tmean[(i - sc + 1):i]
    STI_values[i] <- mean(window, na.rm = TRUE)
  }
  
  # Calculate the mean and standard deviation of the aggregated values, excluding NAs
  agg_mean <- mean(STI_values, na.rm = TRUE)
  agg_sd <- 1 #sd(STI_values, na.rm = TRUE)
  
  # Standardize the aggregated values
  STI_values <- (STI_values - agg_mean) / agg_sd
  
  return(STI_values)
}
