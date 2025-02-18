#' @export
#'
loadOutputsModel <- function() {
  cfg <- optirrigCORE::loadConfig(path = getDataPath("."))
  om <- optirrigCORE::getOutputModel(groups = "SIA1", cfg = cfg)
  return(om)
}
