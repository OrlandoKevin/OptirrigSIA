#' @inherit fairify::getDataPath
#' @export
getDataPath <- function(path,
                        ...,
                        use_cache = TRUE,
                        cfg = loadConfig(),
                        root = cfg$data[[cfg$data$mode]],
                        cache = Sys.getenv("PKG_DATA_CACHE",
                                           file.path(dirname(tempdir()),
                                                     utils::packageName()))) {
  fairify::getDataPath(path,
                       ...,
                       use_cache = use_cache,
                       cfg = cfg,
                       root = root,
                       cache = cache)
}
