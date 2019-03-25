
#' @title
#' Last observation carried forward
#'
#' @description
#' A helper function for making the multivariable regression table
#'
#' @param x A vector
#' @param forward Logical; default is `TRUE`. Direction to carry forward. By
#'   specifying forward = `FALSE`, you can carry the last observation backward.
#' @param maxgap Numeric; By specifying maxgap, you can choose not to bridge
#'   overly long gaps.
#' @param na.rm Logical; default is `FALSE`. Whether to remove NAs.
#'
#' @return a vector similar to x.
#'
#' @references
#' https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
#'

.repeat_last  <- function(x,
                          forward = TRUE,
                          maxgap = Inf,
                          na.rm = FALSE) {
  if (!forward) x = rev(x)           # reverse x twice if carrying backward
  ind = which(!is.na(x))             # get positions of nonmissing values
  if (is.na(x[1]) && !na.rm)         # if it begins with NA
    ind = c(1,ind)                 # add first pos
  rep_times = diff(                  # diffing the indices + length yields how often
    c(ind, length(x) + 1) )          # they need to be repeated
  if (maxgap < Inf) {
    exceed = rep_times - 1 > maxgap  # exceeding maxgap
    if (any(exceed)) {               # any exceed?
      ind = sort(c(ind[exceed] + 1, ind))      # add NA in gaps
      rep_times = diff(c(ind, length(x) + 1) ) # diff again
    }
  }
  x = rep(x[ind], times = rep_times) # repeat the values at these indices
  if (!forward) x = rev(x)           # second reversion
  x
}
