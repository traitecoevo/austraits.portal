% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trait_values.R
\name{plot_categorical_trait_distribution}
\alias{plot_categorical_trait_distribution}
\title{Plot Distribution of a Categorical Trait}
\usage{
plot_categorical_trait_distribution(data, trait, family_count)
}
\arguments{
\item{data}{A data frame containing trait data.}

\item{trait}{A character string specifying the name of the categorical trait to plot.}

\item{family_count}{An integer specifying the number of families to include in the plot.}
}
\value{
A ggplot2 object representing the distribution of the specified categorical trait.
}
\description{
Generates a plot showing the distribution of a specified categorical trait across families.
}
\examples{
# Example usage:
# plot_categorical_trait_distribution(my_data, "leaf_shape", 10)
}
