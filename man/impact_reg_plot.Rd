% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FlowRegEnvCosts.R
\name{impact_reg_plot}
\alias{impact_reg_plot}
\title{Plots the daily environmental impact of flow regulation (high- and low-flow impact)}
\usage{
impact_reg_plot(River_name, First_year, Last_year, Year_evaluated, Year_impact)
}
\arguments{
\item{River_name}{Name of the river written as character (e.g.: River_name = "Esla")}

\item{First_year}{First year to consider in the analysis starting on October 1st (e.g.: First_year = 1964)}

\item{Last_year}{First year to consider in the analysis finishing on September 30th (e.g.: Last_year = 2011)}

\item{Year_evaluated}{Year when the environmental impact is evaluated (e.g.: Year_evaluated = 2010)}

\item{Year_impact}{Year when the human impact started (the construction of a dam) (e.g.: Year_impact = 1988)}
}
\value{
Plots the daily environmental impact of flow regulation (high- and low-flow impact).
}
\description{
Plots the daily environmental impact of flow regulation (high- and low-flow impact)
}
\examples{
data(flowdata)
impact_reg_plot(River_name = "Esla", First_year=1964,
Last_year=2011, Year_evaluated=2010, Year_impact=1988)
}
