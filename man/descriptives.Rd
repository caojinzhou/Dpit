\name{descriptives}
\alias{descriptives}
\title{
descriptives
}
\description{
Returns a data frame containing descriptive statistics per sample.
}
\usage{
descriptives(x)	
}

\arguments{
\item{x}{A data set}
		   }
\value{

\describe{

\item{\code{Negative.values?}}{Whether or not there are any negative values in a sample. If there is at least one negative value, then the program will print out "Negative values detected" in the relevant cell. If there are no negative values, then the program will leave the cell blank.}
\item{\code{zeros?}}{Whether or not there are any zero values in a sample. If there is at least one zero value, then the program will print out "zero values detected" in the relevant cell. If there are no negative values, then the program will leave the cell blank.}
\item{\code{N}}{Number of observations in a sample--before the package removes any non-positive values that lead to incalculable expressions (e.g., the log of zero is undefined).}
\item{\code{median}}{Median value of a sample--before removing any non-positive values.}
\item{\code{mean}}{Mean value of a sample--before removing any non-positive values.}
\item{\code{SD}}{Standard deviation in a sample--before removing any non-positive values.}
\item{\code{skewness}}{Skew, or the amount of non-symmetry, in a sample--before removing any non-positive values.}
\item{\code{kurtosis}}{Kurtosis in a sample--before removing any non-positive values.}
\item{\code{minimum}}{Minimum value of a sample--before removing any non-positive values.}
\item{\code{maximum}}{Maximum value of a sample--before removing any non-positive values.}
\item{\code{No.of.SDs}}{Number of standard deviations contained by a sample. That is: (maximum value - minimum value) / standard deviation.}

		}
	  }

\examples{
\dontrun{
#The following example uses FourSamples.rda, which is a data set included in the package.

data(file = "FourSamples.rda")
out<-descriptives(FourSamples)
}  
}
