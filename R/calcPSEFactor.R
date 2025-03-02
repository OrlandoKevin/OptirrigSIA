#' @export
#'
calcPSEFactor <- function(niveau_R, niveau_restriction) {
  if (niveau_R == 1 | (niveau_R == 2 & niveau_restriction == "vigilance")) {
    return(0)
  } else if (niveau_R == 3 & niveau_restriction == "crise") {
    return(3)
  } else if ((niveau_R == 2 & niveau_restriction == "crise") | (niveau_R == 3 & niveau_restriction == "alerte")) {
    return(2)
  } else {
    return(1)
  }

  # Correction après discussion du 20250218 à 16h30

  # if (niveau_R == 1 | (niveau_R == 2 & niveau_restriction == "vigilance")) {
  #   return(0)
  # } else if (niveau_R == 3 & niveau_restriction == "crise") {
  #   return(1)
  # } else if (niveau_R == 2 & niveau_restriction == "crise") {
  #   return(0.3)
  # } else if (niveau_R == 3 & niveau_restriction == "alerte") {
  #   return(0.5)
  # } else {
  #   return(0.1)
  # }
}
