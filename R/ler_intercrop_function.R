# Jackson Coldiron
# LER Function
# Feb 24, 2025

#' Land Equivalent Ratio (LER) Function
#' 
#' This function the LER for two crops intercropped together
#'
#' @param y_intercrop_A yield of crop A in intercrop
#' @param y_mono_A yield of crop A in monoculture
#' @param y_intercrop_B yield of crop B in intercrop
#' @param y_mono_B yield of crop B in monoculture

# Define LER function
# Four inputs are required: yield of intercrop A, yield of monocrop A, yield of intercrop B, yield of intercrop B
calculate_LER <- function(y_intercrop_A, y_mono_A, y_intercrop_B, y_mono_B) {
  
  # Error handling: Ensure inputs are numeric and positive
  if (!all(sapply(list(y_intercrop_A, y_mono_A, y_intercrop_B, y_mono_B), is.numeric))) {
    stop("All inputs must be numeric values.")
  }
  if (any(c(y_intercrop_A, y_mono_A, y_intercrop_B, y_mono_B) <= 0)) {
    stop("All inputs must be positive values.")
  }
  
  # Calculate LER
  LER <- (y_intercrop_A / y_mono_A) + (y_intercrop_B / y_mono_B)
  
  # Interpretation
  if (LER > 1) {
    message("Intercropping is more efficient than monocropping (LER > 1).")
  } else if (LER == 1) {
    message("Intercropping and monocropping have the same efficiency (LER = 1).")
  } else {
    message("Monocropping is more efficient than intercropping (LER < 1).")
  }
  
  return(LER)
}

