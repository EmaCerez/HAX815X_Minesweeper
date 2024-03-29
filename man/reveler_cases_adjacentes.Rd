% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demine.R
\name{reveler_cases_adjacentes}
\alias{reveler_cases_adjacentes}
\title{Do we have to reveal the adjacent cells of a cell?}
\usage{
reveler_cases_adjacentes(grille, visible, i, j)
}
\arguments{
\item{grille}{A matrix of integers. The value of each cell is the number of mines adjacent to it or a bomb itself.}

\item{visible}{A matrix of booleans. The value of each cell is TRUE if the cell is revealed, FALSE otherwise.}

\item{i}{The row of the cell.}

\item{j}{The column of the cell.}
}
\value{
visible updated with the new revealed cells.
}
\description{
Do we have to reveal the adjacent cells of a cell?
}
\examples{

reveler_cases_adjacentes(matrix(0, nrow = 2, ncol =2), matrix(c(TRUE,FALSE,FALSE,FALSE), nrow = 2), 1, 1)

}
