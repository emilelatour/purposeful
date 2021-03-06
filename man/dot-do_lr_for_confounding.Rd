% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do-lr-for-confounding.R
\name{.do_lr_for_confounding}
\alias{.do_lr_for_confounding}
\title{Help do logistic regression}
\usage{
.do_lr_for_confounding(data, form, keep)
}
\arguments{
\item{data}{A tibble or data frame}

\item{form}{Character string of the regression formula}

\item{keep}{Variable names to keep for printing. Generally, those variable
names that are included in the model but aren't the potential confounders
that are being considered. We want to see the change in coefficients for
these variables.}
}
\value{
A tibble or data frame
}
\description{
A helper function when checking for confounding
}
