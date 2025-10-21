#' @title Memoised ATRI API Getter
#' @description
#' A memoised version of \code{atri_get()} that caches results from ATRI API calls.
#' This allows repeated queries with identical arguments to return instantly from
#' cache, reducing redundant API requests and improving performance.
#'
#' @inheritParams atri_get
#'
#' @return
#' A memoised function. When called, it behaves like \code{atri_get()}, but
#' subsequent calls with the same arguments will retrieve results from the cache.
#'
#' @details
#' The memoisation is implemented using the \pkg{memoise} package.
#' Cached results persist for the duration of the R session (or longer, if a
#' persistent cache backend such as \code{cache_filesystem()} is used).
#'
#' To clear the cache, call \code{\link{clear_atri_cache}()}.

#' @rdname memoise_atri_get
#' @export
#' @importFrom memoise memoise

memoise_atri_get <- function(server, token) {
  .abcds_cache$memoised_atri_get(server, token)
}

#' @title Clear Cached ATRI Data
#' @description
#' Clears the memoised cache created by \code{memoise_get_atri()}, ensuring that
#' subsequent calls to functions using this cache will retrieve fresh data from
#' the ATRI API instead of previously stored results.
#'
#' @return
#' Invisibly returns \code{NULL}. This function is called for its side effect of
#' clearing the cached data.
#'
#' @details
#' Use this function whenever the underlying ATRI data have been updated and you
#' want to force all memoised calls (such as \code{get_health()},
#' \code{get_demographics()}, etc.) to pull new data from the API.
#'
#' This function unlinks to folder from the creation of the memoised object
#' \code{memoise_get_atri}, effectively resetting the cache.
#'
#' @rdname clear_atri_cache
#' @export

clear_atri_cache <- function() {
  .abcds_cache$clear_abcds_cache()
}
