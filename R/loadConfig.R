#' @inherit fairify::loadConfig
#' @export
loadConfig <- function(userFile = "config.yml",
                       pathDefaultCfg = system.file("config.yml", package = utils::packageName()),
                       ...) {
  fairify::loadConfig(userFile = userFile, pathDefaultCfg = pathDefaultCfg, ...)
}
