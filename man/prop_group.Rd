% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_group.R
\name{prop_group}
\alias{prop_group}
\title{prop_group}
\usage{
prop_group(
  data,
  group,
  prop_exp,
  facet_var = NULL,
  filter_exp = NULL,
  prop_method = "beta",
  ...,
  unit = "\%",
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  xlab = NULL,
  ylab = NULL,
  scale = 100,
  digits = 0,
  show_labs = TRUE,
  show_n = FALSE,
  show_value = TRUE,
  dodge = 0.9,
  reorder = F,
  error_bar = T,
  fill = "deepskyblue3",
  na.rm.group = T,
  total_name = "Total",
  font = "Roboto",
  wrap_width = 25,
  export_path = NULL
)
}
\arguments{
\item{data}{A dataframe or an object from the survey package or an object from the srvyr package.}

\item{group}{A variable defining groups be compared.}

\item{prop_exp}{An expression that define the proportion to be computed.}

\item{facet_var}{A variable defining the faceting group.}

\item{filter_exp}{An expression that filters the data, preserving the design.}

\item{prop_method}{Type of proportion method to use. See svyciprop in survey package for details. Default is the beta method.}

\item{...}{All options possible in as_survey_design in srvyr package.}

\item{unit}{Unit showed in the graphic. Default is percent.}

\item{caption}{Caption of the graphic.}

\item{title}{Title of the graphic.}

\item{subtitle}{Subtitle of the graphic.}

\item{xlab}{X label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.}

\item{ylab}{Y label on the graphic. As coord_flip() is used in the graphic, xlab refers to the x label on the graphic, after the coord_flip(), and not to the x variable in the data.}

\item{scale}{Denominator of the proportion. Default is 100 to interprets numbers as percentages.}

\item{digits}{Numbers of digits showed on the values labels on the graphic. Default is 0.}

\item{show_labs}{TRUE if you want to show axes, titles and caption labels. FALSE if you do not want to show any label on axes and titles. Default is TRUE.}

\item{show_n}{TRUE if you want to show on the graphic the number of individuals in the sample in each group. FALSE if you do not want to show this number. Default is FALSE.}

\item{show_value}{TRUE if you want to show the proportion in each group on the graphic. FALSE if you do not want to show the proportion.}

\item{dodge}{Width of the bar, between 0 and 1.}

\item{reorder}{TRUE if you want to reorder the groups according to the proportion. NA value, in case if na.rm.group = FALSE, is not included in the reorder.}

\item{error_bar}{TRUE if you want to show the error bars on the graphic. FALSE if you do not want to show the error bars.}

\item{fill}{Colour of the bars. NA bar, in case if na.rm.group = FALSE, and total bar are always in grey.}

\item{na.rm.group}{TRUE if you want to remove observations with NA on the group variable or NA on the facet variable. FALSE if you want to create a group with the NA value for the group variable and a facet with the NA value for the facet variable. NA in the variables included in prop_exp are not affected in this argument. All the observation with a NA in the variables included in prop_exp are excluded.}

\item{total_name}{Name of the total bar on the graphic. Default is Total.}

\item{font}{Font used in the graphic. Available fonts, included in the package itself, are "Roboto", "Montserrat" and "Gotham Narrow". Default is "Roboto".}

\item{wrap_width}{Number of characters before before going to the line. Applies to the labels of the groups. Default is 25.}

\item{export_path}{Path to export the results in an xlsx file. The file includes two sheets : the table and the graphic.}
}
\value{
A list that contains a table, a graphic and a statistical test
}
\description{
Function to compare proportions in different groups from complex survey data. It produces a table, a graphic and a statistical test.
}
\examples{
# Loading of data
data(eusilc, package = "laeken")

# Recoding eusilc$pl030 into eusilc$pl030_rec
eusilc$pl030_rec <- NA
eusilc$pl030_rec[eusilc$pl030 == "1"] <- "Working full time"
eusilc$pl030_rec[eusilc$pl030 == "2"] <- "Working part time"
eusilc$pl030_rec[eusilc$pl030 == "3"] <- "Unemployed"
eusilc$pl030_rec[eusilc$pl030 == "4"] <- "Student"
eusilc$pl030_rec[eusilc$pl030 == "5"] <- "Retired"
eusilc$pl030_rec[eusilc$pl030 == "6"] <- "Permanently disabled"
eusilc$pl030_rec[eusilc$pl030 == "7"] <- "Fulfilling domestic tasks"

# Computation, taking sample design into account
eusilc_prop <- prop_group(
eusilc,
group = pl030_rec,
prop_exp = py090n > 0,
strata = db040,
ids = db030,
weight = rb050,
title = "\% of ind. receiving unemployment benefits in their hh",
subtitle = "Example with austrian SILC data from 'laeken' package"
)
eusilc_prop$graph

}
