% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_enriched_themes.r
\name{get_enriched_themes}
\alias{get_enriched_themes}
\title{Find enriched themes in a storyset of interest relative to a background storyset.}
\usage{
get_enriched_themes(
  mystorysets,
  test_storysets = mystorysets$names,
  background_storyset = c("tos", "tas", "tng", "voy"),
  theme_levels = c("central", "peripheral"),
  min_storyset_size = 5,
  min_theme_occurrence = 1
)
}
\arguments{
\item{mystorysets}{A storysets class object that contains one or many individual storysets.}

\item{test_storysets}{The storyset names to be analysed. The default \code{mystorysets$names} is to analyse all storysets.}

\item{background_storyset}{default \code{c("tos", "tas", "tng", "voy")}. A string indicating the storyset to use as background.
Possible values are \code{c("tos", "tas")}, \code{"tng"}, \code{"voy"}, and \code{c("tos", "tas", "tng", "voy")}.}

\item{theme_levels}{\code{c("central", "peripheral")}. A string indicating the theme levels to use in the analysis.
Possible values are \code{central} and/or \code{peripheral}.}

\item{min_storyset_size}{default \code{5}. The minimum allowable size for a storyset.
For small samples the hypergeometric test may be unreliable.}

\item{min_theme_occurrence}{default \code{1}. The minimum number of times a theme must occur in a storyset.
For small samples the hypergeometric test may be unreliable.}
}
\value{
Returns a data frame where each row corresponds to a theme.
The column Pvalue contains raw enrichment scores, i.e., hypergeometric 
test P-values.
}
\description{
\code{get_enriched_themes} calculates enrichment scores for themes in
a storset relative to a background set of stories
according to the hypergeometric test.
}
\examples{
#######################################################################
# Load the Star Trek series storysets smt file and storysets object   #
#######################################################################
file <- system.file("storysets", "series.smt", package = "stoRy")
mystorysets <- storysets$new(file)
print(mystorysets)

#######################################################################
# Perform the theme enrichment analysis for each of TOS, TAS, and TNG #
# relative to default TOS/TAS/TNG background                          #
#######################################################################
results <- get_enriched_themes(mystorysets)

#######################################################################
# Output top twenty enriched TOS themes                               #
#######################################################################
results$TOS[1:20,]

#######################################################################
# Output top ten enriched TAS themes                                  #
#######################################################################
results$TAS[1:10,]

#######################################################################
# Output top twenty enriched TNG themes                               #
#######################################################################
results$TNG[1:20,]
}
\references{
Onsjö, M., and Sheridan, P. (2017): 
"Theme Enrichment Analysis: A Statistical Test for Identifying Significantly Enriched Themes in a List of Stories with an Application to the Star Trek Television Franchise", 
ArXiv.
}
