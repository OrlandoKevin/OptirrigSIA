#' @export
#'
calcPSEFactor <- function(niveau_RU, niveau_restriction) {
  # if (niveau_RU == 1 | (niveau_RU == 2 & niveau_restriction == "vigilance")) {
  #   return(0)
  # } else if (niveau_RU == 3 & niveau_restriction == "crise") {
  #   return(3)
  # } else if ((niveau_RU == 2 & niveau_restriction == "crise") | (niveau_RU == 3 & niveau_restriction == "alerte")) {
  #   return(2)
  # } else {
  #   return(1)
  # }

  # Correction après discussion du 20250218 à 16h30

  if (niveau_RU == 1 | (niveau_RU == 2 & niveau_restriction == "vigilance")) {
    return(0)
  } else if (niveau_RU == 3 & niveau_restriction == "crise") {
    return(10)
  } else if (niveau_RU == 2 & niveau_restriction == "crise") {
    return(3)
  } else if (niveau_RU == 3 & niveau_restriction == "alerte") {
    return(5)
  } else {
    return(1)
  }
}
