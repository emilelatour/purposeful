
#' @title 
#' Help do logistic regression 
#' 
#' @description 
#' A helper function when checking for confounding 
#'
#' @param data A tibble or data frame
#' @param form Character string of the regression formula
#' @param keep Variable names to keep for printing. Generally, those variable
#'   names that are included in the model but aren't the potential confounders
#'   that are being considered. We want to see the change in coefficients for
#'   these variables.
#'   
#' @importFrom broom tidy
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom stats glm
#' @importFrom stringr str_detect
#'
#' @return A tibble or data frame
#' 
#' 
.do_lr_for_confounding <- function(data, 
                                   form, 
                                   keep) { 
  
  res <- glm(formula = as.formula(form), 
             data = data, 
             family = binomial(link = "logit"))
  
  res %>% 
    broom::tidy() %>% 
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::filter(stringr::str_detect(term, 
                                      paste(keep, collapse = "|"))) %>% 
    dplyr::select(term, log_odds = estimate)
  
}
