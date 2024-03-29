% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions_minimal.R
\name{read_csv_or_zip}
\alias{read_csv_or_zip}
\title{read_csv_or_zip
Read input files, adapting to multiple csv's or a single zip}
\usage{
read_csv_or_zip(input_files, workers = 1, only_list = FALSE)
}
\arguments{
\item{input_files}{multiple csv's or a single zip}

\item{workers}{Workers for data.table::fread}

\item{only_list}{Only list the files, but do not read them}
}
\value{
Either a list of files or a dataframe after reading the files
}
\description{
read_csv_or_zip
Read input files, adapting to multiple csv's or a single zip
}
\examples{
read_csv_or_zip(system.file("extdata/", "999.zip", package = "jsPsychHelpeR"))
}
