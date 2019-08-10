

#' @title
#' Do univariable logistic regression and extract results in nice format.
#'
#' @description
#' A helper function to be used in a loop to do univariable regression and give
#' some nice lookin' results/
#'
#' @param data a data frame or tibble
#' @param formula A character string
#' @param format Display format in case I need to escape some characters. A
#'   place holder for now in case I need it in the future. Default is "html".
#' @param conf_level The confidence level to use for the confidence interval.
#'   Must be strictly greater than 0 and less than 1. Defaults to 0.95, which
#'   corresponds to a 95 percent confidence interval.
#' @param exponentiate Logical indicating whether or not to exponentiate the
#'   the coefficient estimates. This is typical for logistic and multinomial
#'   regressions, but a bad idea if there is no log or logit link. Defaults to
#'   `TRUE`.
#' @param include_last_row Adds a row at the end of each set of results to give
#'   some breathing room. Default is `TRUE`.
#' @param ... Additional arguments.
#'
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr coalesce
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr mutate_at
#' @importFrom dplyr na_if
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom labelled var_label
#' @importFrom purrr map
#' @importFrom purrr pluck
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr crossing
#' @importFrom tidyr unnest
#'
#' @return A tibble or data frame

.do_logistic_univ <- function(data,
                              formula,
                              format = "html",
                              conf_level = 0.95,
                              exponentiate = TRUE,
                              include_last_row = TRUE, ...) {


  #### Fit the model --------------------------------

  fit <- glm(formula = as.formula(formula),
             family = binomial(link = "logit"),
             data = data)

  # independent <- attr(fit$terms, "term.labels")[[1]]
  independent <- attr(fit$terms, "term.labels")
  outcome <- names(fit$model)[1]

  indep_split <- unlist(stringr::str_split(independent, ":"))
  indep_split <- paste0(indep_split, collapse = "|")

  #### Overall p-value --------------------------------

  lrt_pval <- drop1(fit, test = "LRT") %>%
    purrr::pluck(., "Pr(>Chi)", 2)

  res_overall <- tibble::tibble(
    covariate = independent,
    term = NA_character_,
    ref = NA_character_,
    estimate = NA_real_,
    lower_ci = NA_real_,
    upper_ci = NA_real_,
    p_value_wald = NA_real_,
    p_value_lrt = lrt_pval,
    signif = .calc_sig_ind(p_value_lrt, format)
  )

  #### Results by level --------------------------------

  if (any(class(data[[independent]]) %in% c("factor",
                                            "ordered",
                                            "logical",
                                            "character"))) {

    res_by_level <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      # mutate(term = stringr::str_remove(term, independent),
      mutate(term = stringr::str_remove_all(term, indep_split),
             ref = NA_character_,
             covariate = "") %>%
      dplyr::select(covariate,
                    term,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value_wald = p_value) %>%
      mutate(p_value_lrt = NA_real_,
             signif = .calc_sig_ind(p_value_wald, format))

    fct_ref_lev <- levels(data[[independent]])[[1]]

    row_one <- tibble::tibble(
      covariate = NA_character_,
      # term = as.character(glue::glue("Ref lvl = {fct_ref_lev}")),
      term = fct_ref_lev,
      ref = "Reference level",
      estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt = NA_real_,
      signif = NA_character_)

    res_by_level <- dplyr::bind_rows(row_one,
                                     res_by_level)

  } else if (any(stringr::str_detect(independent, ":"))) {


    res_by_level <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      # mutate(term = stringr::str_remove(term, independent),
      mutate(term = stringr::str_remove_all(term, indep_split),
             ref = NA_character_,
             covariate = "") %>%
      dplyr::select(covariate,
                    term,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value_wald = p_value) %>%
      mutate(p_value_lrt = NA_real_,
             signif = .calc_sig_ind(p_value_wald, format))

    # fct_ref_lev <- levels(data[[independent]])[[1]]
    fct_ref_lev <- "TBD"

    row_one <- tibble::tibble(
      covariate = NA_character_,
      # term = as.character(glue::glue("Ref lvl = {fct_ref_lev}")),
      term = fct_ref_lev,
      ref = "Reference level",
      estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt = NA_real_,
      signif = NA_character_)

    res_by_level <- dplyr::bind_rows(row_one,
                                     res_by_level)

  } else {

    res_by_level <- fit %>%
      broom::tidy(.,
                  conf.int = TRUE,
                  conf.level = conf_level,
                  exponentiate = exponentiate) %>%
      janitor::clean_names() %>%
      dplyr::filter(term != "(Intercept)") %>%
      mutate(covariate = "",
             term = "",
             ref = "") %>%
      dplyr::select(covariate,
                    term,
                    ref,
                    estimate,
                    lower_ci = conf_low,
                    upper_ci = conf_high,
                    p_value_wald = p_value) %>%
      mutate(p_value_lrt = NA_real_,
             signif = .calc_sig_ind(p_value_wald, format))

  }



  #### Combine results --------------------------------

  if (include_last_row == TRUE) {

    last_row <- tibble::tibble(
      covariate = NA_character_,
      term = NA_character_,
      ref = NA_character_,
      estimate = NA_real_,
      lower_ci = NA_real_,
      upper_ci = NA_real_,
      p_value_wald = NA_real_,
      p_value_lrt = NA_real_,
      signif = NA_character_)

    res <- dplyr::bind_rows(
      res_overall,
      res_by_level,
      last_row)

  } else {

    res <- dplyr::bind_rows(
      res_overall,
      res_by_level)

  }

  res

}
