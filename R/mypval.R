

#' @title
#' Formatting p-values
#'
#' @description
#' A little helper to format p-values in the table.
#'
#' @param pvals A numeric value or vector of p-values
#' @param sig_limit Lower bound for precisions
#' @param digits Integer indicating the number of decimal places (round)
#' @param html Logical; if \code{TRUE}, uses \code{&lt;} instead of \code{<}.
#'
.mypval <- function(pvals, sig_limit = 0.001, digits = 3L, html = FALSE) {

  stopifnot(
    sig_limit > 0,
    sig_limit < 1
  )

  html <- html + 1L

  sapply(pvals, function(x, sig_limit) {

    if (is.na(x) | !nzchar(x)) return(NA)

    if (x < sig_limit) {
      sprintf(c('< %s', '&lt; %s')[html], format(sig_limit))
    } else {
      return(format(round(x, digits),
                    nsmall = max(0, digits),
                    scientific = FALSE,
                    trim = FALSE))
    }

  }, sig_limit = sig_limit)

}
