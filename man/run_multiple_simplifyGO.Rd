% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_multiple_simplifyGO.R
\name{run_multiple_simplifyGO}
\alias{run_multiple_simplifyGO}
\title{Run multiple simplifyGOFromMultipleLists}
\usage{
run_multiple_simplifyGO(all_selection_GO, padj_column, padj_cutoff, ont)
}
\arguments{
\item{all_selection_GO}{A list of dataframes. The GO object for the simplifyGOFromMultipleLists function.}

\item{padj_column}{Character. The column name for the p-value adjustment.}

\item{padj_cutoff}{Numeric. The cutoff for the p-value adjustment.}

\item{ont}{Character. The ontology for the simplifyGOFromMultipleLists function.}
}
\value{
A named list of heatmaps generated by the simplifyGOFromMultipleLists function.
}
\description{
This function runs the simplifyGOFromMultipleLists function with
different combinations of semantic similarity measures and clustering methods.
}
