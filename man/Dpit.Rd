\name{Dpit}
\alias{Dpit}
\title{Distribution Pitting}

\description{
Compares distributions with one another in terms of their fit to each sample in a dataset that contains multiple samples, as described in Joo, Aguinis, and Bradley (2017). Users can examine the fit of seven distributions per sample: pure power law, lognormal, exponential, power law with an exponential cutoff, normal, Poisson, and Weibull. Automation features allow the user to compare all distributions for all samples with a single command line, which creates a separate row containing results for each sample until the entire dataset has been analyzed. Automation features also skip over any unsuccessful calculations and continues analyzing the remainder of the samples. When calculations fail (e.g., sample size was too small), "NA" entries will be printed in the relevant cells of the results matrix before continuing with subsequent calculations.
      }

\usage{
Dpit(x)	
	  }
	  
\arguments{
\item{x}{A data set}
		   }

\value{
This function returns a data frame containing the complete detailed results of distribution pitting. In the data frame, each row corresponds to a sample, or data vector. That is, the first row in the data frame is sample #1, the second row is sample #2, etc. Each column in the data frame shows a distribution pitting statistic per sample. The data frame contains a large number of columns--as described in detail below:

\describe{

\item{\code{PLvCut.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a pure power law (PL) fits the focal sample better than a power law with an exponential cutoff (Cut). So, a positive value means that the power law with an exponential cutoff fits worse, whereas a negative value means that the pure power law fits worse. This loglikelihood ratio is not normalized--hence, the word "raw" in the label.}
\item{\code{PLvCut.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or PLvCut.rawLR. The p value indicates the extent to which random fluctuations alone likely explain the presence of a non-zero loglikelihood ratio, such that loglikelihood ratio = 0 constitutes the null hypothesis. The lower the p value, the less likely that the loglikelihood ratio is simply due to chance.}

\item{\code{PLvWeib.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a pure power law (PL) fits the focal sample better than a Weibull distribution (Weib). So, a positive value means that the Weibull distribution fits worse, whereas a negative value means that the pure power law fits worse.}
\item{\code{PLvWeib.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or PLvWeib.rawLR.}
\item{\code{PLvWeib.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or PLvWeib.normLR.}

\item{\code{PLvLgN.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a pure power law (PL) fits the focal sample better than a lognormal distribution (LgN). So, a positive value means that the lognormal distribution fits worse, whereas a negative value means that the pure power law fits worse.}
\item{\code{PLvLgN.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or PLvLgN.rawLR.}
\item{\code{PLvLgN.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or PLvLgN.normLR.}

\item{\code{PLvExp.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a pure power law (PL) fits the focal sample better than an exponential distribution (Exp). So, a positive value means that the exponential distribution fits worse, whereas a negative value means that the pure power law fits worse.}
\item{\code{PLvExp.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or PLvExp.rawLR.}
\item{\code{PLvExp.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or PLvExp.normLR.}

\item{\code{PLvPoi.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a pure power law (PL) fits the focal sample better than a Poisson distribution (Poi). So, a positive value means that the Poisson distribution fits worse, whereas a negative value means that the pure power law fits worse.}
\item{\code{PLvPoi.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or PLvPoi.rawLR.}
\item{\code{PLvPoi.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or PLvPoi.normLR.}

\item{\code{CutvWeib.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a power law with an exponential cutoff (Cut) fits the focal sample better than a Weibull distribution (Weib). So, a positive value means that the Weibull distribution fits worse, whereas a negative value means that the power law with an exponential cutoff fits worse.}
\item{\code{CutvWeib.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or CutvWeib.rawLR.}
\item{\code{CutvWeib.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or CutvWeib.normLR.}

\item{\code{CutvLgN.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a power law with an exponential cutoff (Cut) fits the focal sample better than a lognormal distribution (LgN). So, a positive value means that the lognormal distribution fits worse, whereas a negative value means that the power law with an exponential cutoff fits worse.}
\item{\code{CutvLgN.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or CutvLgN.rawLR.}
\item{\code{CutvLgN.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or CutvLgN.normLR.}

\item{\code{CutvExp.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a power law with an exponential cutoff (Cut) fits the focal sample better than an exponential distribution (Exp). So, a positive value means that the exponential distribution fits worse, whereas a negative value means that the power law with an exponential cutoff fits worse.}
\item{\code{CutvExp.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or CutvExp.rawLR.}
\item{\code{CutvExp.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or CutvExp.normLR.}

\item{\code{CutvPoi.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a power law with an exponential cutoff (Cut) fits the focal sample better than a Poisson distribution (Poi). So, a positive value means that the Poisson distribution fits worse, whereas a negative value means that the power law with an exponential cutoff fits worse.}
\item{\code{CutvPoi.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or CutvPoi.rawLR.}
\item{\code{CutvPoi.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or CutvPoi.normLR.}

\item{\code{WeibvLgN.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a Weibull distribution (Weib) fits the focal sample better than a lognormal distribution (LgN). So, a positive value means that the lognormal distribution fits worse, whereas a negative value means that the Weibull distribution fits worse.}
\item{\code{WeibvLgN.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or WeibvLgN.rawLR.}
\item{\code{WeibvLgN.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or WeibvLgN.normLR.}

\item{\code{WeibvExp.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a Weibull distribution (Weib) fits the focal sample better than an exponential distribution (Exp). So, a positive value means that the exponential distribution fits worse, whereas a negative value means that the Weibull distribution fits worse.}
\item{\code{WeibvExp.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or WeibvExp.rawLR.}
\item{\code{WeibvExp.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or WeibvExp.normLR.}

\item{\code{WeibvPoi.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a Weibull distribution (Weib) fits the focal sample better than a Poisson distribution (Poi). So, a positive value means that the Poisson distribution fits worse, whereas a negative value means that the Weibull distribution fits worse.}
\item{\code{WeibvPoi.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or WeibvPoi.rawLR.}
\item{\code{WeibvPoi.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or WeibvPoi.normLR.}

\item{\code{LgNvExp.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a lognormal distribution (LgN) fits the focal sample better than an exponential distribution (Exp). So, a positive value means that the exponential distribution fits worse, whereas a negative value means that the lognormal distribution fits worse.}
\item{\code{LgNvExp.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or LgNvExp.rawLR.}
\item{\code{LgNvExp.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or LgNvExp.normLR.}

\item{\code{LgNvPoi.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a lognormal distribution (LgN) fits the focal sample better than a Poisson distribution (Poi). So, a positive value means that the Poisson distribution fits worse, whereas a negative value means that the lognormal distribution fits worse.}
\item{\code{LgNvPoi.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or LgNvPoi.rawLR.}
\item{\code{LgNvPoi.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or LgNvPoi.normLR.}

\item{\code{ExpvPoi.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which an exponential distribution (Exp) fits the focal sample better than a Poisson distribution (Poi). So, a positive value means that the Poisson distribution fits worse, whereas a negative value means that the exponential distribution fits worse.}
\item{\code{ExpvPoi.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or ExpvPoi.rawLR.}
\item{\code{ExpvPoi.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or ExpvPoi.normLR.}

\item{\code{NvPL.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a normal distribution (N) fits the focal sample better than a pure power law (PL). So, a positive value means that the pure power law fits worse, whereas a negative value means that the normal distribution fits worse.}
\item{\code{NvPL.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or NvPL.rawLR.}
\item{\code{NvPL.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or NvPL.normLR.}

\item{\code{NvCut.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a normal distribution (N) fits the focal sample better than a power law with an exponential cutoff (Cut). So, a positive value means that the power law with an exponential cutoff fits worse, whereas a negative value means that the normal distribution fits worse.}
\item{\code{NvCut.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or NvCut.rawLR.}
\item{\code{NvCut.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or NvCut.normLR.}

\item{\code{NvWeib.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a normal distribution (N) fits the focal sample better than a Weibull disribution (Weib). So, a positive value means that the Weibull distribution fits worse, whereas a negative value means that the normal distribution fits worse.}
\item{\code{NvWeib.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or NvWeib.rawLR.}
\item{\code{NvWeib.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or NvWeib.normLR.}

\item{\code{NvLgN.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a normal distribution (N) fits the focal sample better than a lognormal disribution (LgN). So, a positive value means that the lognormal distribution fits worse, whereas a negative value means that the normal distribution fits worse.}
\item{\code{NvLgN.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or NvLgN.rawLR.}
\item{\code{NvLgN.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or NvLgN.normLR.}

\item{\code{NvExp.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a normal distribution (N) fits the focal sample better than an exponential disribution (Exp). So, a positive value means that the exponential distribution fits worse, whereas a negative value means that the normal distribution fits worse.}
\item{\code{NvExp.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or NvExp.rawLR.}
\item{\code{NvExp.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or NvExp.normLR.}

\item{\code{NvPoi.rawLR}}{
A loglikelihood ratio (LR) quantifying the degree to which a normal distribution (N) fits the focal sample better than a Poisson disribution (Poi). So, a positive value means that the Poisson distribution fits worse, whereas a negative value means that the normal distribution fits worse.}
\item{\code{NvPoi.normLR}}{
Normalized value of the previously mentioned loglikelihood ratio, or NvPoi.rawLR.}
\item{\code{NvPoi.p}}{
The p value associated with the previously mentioned loglikelihood ratio, or NvPoi.normLR.}
		 
		}
	  }
	  
\details{
For a given sample, the Dpit() function does not truncate (i.e., discard) data points that fall below a certain threshold, or xmin. More precisely, Dpit() sets xmin at the lowest positive number in the sample. This is because, theoretically, the function focuses on assessing the fit of distributions in their entirety rather than their tail ends. In other words, the goal of Dpit() is to conclude whether a sample itself follows a certain type of distribution, not whether the tail end of the sample follows a certain type of distribution.
}

\note{The Dpit() function is based on the R code found at:
\url{http://tuvalu.santafe.edu/~aaronc/powerlaws/}.  
In particular, we borrowed heavily from Cosma R. Shalizi's code. But our code differs from the aforementioned code in mainly three ways. First, the Dpit() function sets xmin at the lowest positive number in a sample to assess the fit of distributions in their entirety rather than their tail ends. Second, our code allows for the comparison of non-pure power law distributions with one another. Third, our code includes automation features that allow the user to compare all distributions for all samples with a single command line, which creates a separate row containing results for each sample until the entire dataset has been analyzed. Automation features clean each sample by removing missing cases and non-positive values that lead to incalculable expressions (e.g., the log of zero is undefined). Automation features also skip over any unsuccessful calculations and continues analyzing the remainder of the samples. When calculations fail (e.g., sample size was too small), "NA" entries will be printed in the relevant cells of the results matrix before continuing with subsequent calculations.
}

\references{
Clauset, A., Shalizi, C. R., & Newman, M. E. J. 2009. Power-law distributions in empirical data. SIAM Review, 51, 661-703. Available at: http://arxiv.org/abs/0706.1062

Joo, H., Aguinis, H., & Bradley, K. J. 2017. Not all nonnormal distributions are created equal: Improved theoretical and measurement precision. Journal of Applied Psychology. Advance online publication. doi: 10.1037/apl0000214
}

\examples{
\dontrun{
#The following example uses FourSamples.rda, which is a data set included in the package.

data(file = "FourSamples.rda")
out<-Dpit(FourSamples)

#Full results are shown in Table 4 in Joo, Aguinis, and Bradley (2017)

#Next, to draw conclusions regarding the fit of a certain type of distribution per sample, 
#we suggest that users implement three decision rules, which are described in 
#the Analysis section in Joo, Aguinis, and Bradley (2017).

#Conclusions regarding the fit of distributions to the four samples in the focal data set
#--after applying the three decision rules--can be found in 
#the first two and last two rows in Table 3, in Joo, Aguinis, and Bradley (2017).
}
}