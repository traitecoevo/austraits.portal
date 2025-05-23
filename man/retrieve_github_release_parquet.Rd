% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/a-helper.R
\name{retrieve_github_release_parquet}
\alias{retrieve_github_release_parquet}
\title{Retrieve all assets from a GitHub Release}
\usage{
retrieve_github_release_parquet(
  version_tag = "6.0.0",
  output_dir = system.file("extdata/austraits", package = "austraits.portal")
)
}
\arguments{
\item{version_tag}{The version tag of the GitHub release (e.g., "6.0.0")}

\item{output_dir}{The local directory to save the downloaded files}
}
\value{
A vector of paths to the downloaded files
}
\description{
Retrieve all assets from a GitHub Release
}
