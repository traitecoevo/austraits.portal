% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trait_values.R
\name{plot_trait_distribution_beeswarm}
\alias{plot_trait_distribution_beeswarm}
\title{Beeswarm Trait distribution}
\usage{
plot_trait_distribution_beeswarm(
  data,
  trait_name,
  y_axis_category,
  highlight = NA,
  hide_ids = FALSE
)
}
\arguments{
\item{data}{data}

\item{trait_name}{Name of trait to plot}

\item{y_axis_category}{One of \code{dataset_id}, \code{family}}

\item{highlight}{Specify a group to highlight}

\item{hide_ids}{Logical for whether to add a label on y_axis?}
}
\description{
Plots distribution of trait values by a  grouping variable using ggbeeswarm package
}
\examples{
\dontrun{
austraits \%>\% plot_trait_distribution_beeswarm("wood_density", "dataset_id", "Westoby_2014")
}
}
\author{
Daniel Falster - daniel.falster@unsw.edu.au
}
