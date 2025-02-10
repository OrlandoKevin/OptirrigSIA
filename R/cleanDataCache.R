#' @inherit fairify::cleanDataCache
#' @export
cleanDataCache <- function(cache = Sys.getenv("PKG_DATA_CACHE",
                                              file.path(dirname(tempdir()),
                                                        utils::packageName()))) {
  fairify::cleanDataCache(cache = cache)
}
