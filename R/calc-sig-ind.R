
#' @title
#' Make a significant indicator
#'
#' @description
#' Adds asterisks next to p-values like `summary` does with regression output in
#' R.
#'
#' @param p_value A numeric or numeric vector
#' @param format Doesn't do anything right now. Place holder for later. Default
#'   is "html".
#'
#' @importFrom dplyr case_when
#'
#' @return A character or character vector
#'
.calc_sig_ind <- function(p_value,
                          format = "html") {
  # Maybe add an option to see if knitting to HTML or to PDF
  dplyr::case_when(
    p_value <= 0 ~ "",
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "\\*",
    p_value <= 0.1 ~ ".",
    p_value <= 1 ~ ""
  )
}
