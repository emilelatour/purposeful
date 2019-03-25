
#' @title
#' Do some simple rounding
#'
#' @description
#' Just a helper function to do some rounding here and there.
#'
#' @param x A numeric or numeric vector.
#' @param digits Integer; number of decimals to round to.
#'
#' @return A character or character vector.
#'
.myround <- function(x, digits = 1L)  {
  if (digits < 1)
    stop("This is intended for the case digits >= 1.")

  if (length(digits) > 1) {
    digits <- digits[1]
    warning("Using only digits[1]")
  }

  tmp <- sprintf(paste("%.", digits, "f", sep = ""), x)

  # deal with "-0.00" case
  zero <- paste0("0.", paste(rep("0", digits), collapse = ""))
  tmp[tmp == paste0("-", zero)] <- zero

  tmp
}
