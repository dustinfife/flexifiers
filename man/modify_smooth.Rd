% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_modifiers.R
\name{modify_smooth}
\alias{modify_smooth}
\title{Modify the fitted line for a flexplot graphic}
\usage{
modify_smooth(p, method = "lm", se = F, color = NULL)
}
\arguments{
\item{p}{A flexplot graphic}

\item{method}{Type of smoothing (fitted) funciton used. Can be
rlm, poisson, loess, Gamma, polynomial, cubic, or lm}

\item{se}{Should standard errors (confidence bands) be displayed}
}
\value{
a flexplot graphic
}
\description{
Modify the fitted line for a flexplot graphic
}
\examples{
a = flexplot(ptsd~agility + superpower , data=avengers)
a \%>\% modify_smooth(method="lm", color=c("blue", "brown"))
}
