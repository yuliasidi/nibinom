% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_cont.R
\name{sim_cont}
\alias{sim_cont}
\title{Data simulation}
\usage{
sim_cont(p_C, p_T, n_arm, mu1, mu2, sigma1, sigma2, r12, b1, b2)
}
\arguments{
\item{p_C}{numeric, Probsability of events in control arm}

\item{p_T}{numeric, Probsability of events in treatment arm}

\item{n_arm}{numeric, number of observations per arm (balanced)}

\item{mu1}{numeric, mean of continuous covariate 1 at baseline}

\item{mu2}{numeric, mean of continuous covariate 2 at baseline}

\item{sigma1}{numeric, standard deviation of continuous covariate 1 at
baseline}

\item{sigma2}{numeric, standard deviation of continuous covariate 2 at
baseline}

\item{r12}{numeric, correlation between covariate 1 and 2}

\item{b1}{numeric, effect of covariate 1 on the outcome}

\item{b2}{numeric, effect of covariate 2 on the outcome}
}
\value{
tibble
}
\description{
Simulates data for noninferiority trials with binomal proportions.
}
\details{
The output contains a tibble with 6 columns: pat_id, trt, trtn, y,
   x1, x2.
}
\examples{

 sim_cont(
 p_C = 0.6, p_T = 0.5, n_arm = 200,
 mu1 = 4, mu2 = 100, sigma1 = 1,
 sigma2 = 20, r12 = -0.3,
 b1 = 0.1, b2 = -0.01)

}
\seealso{
\code{\link[MASS]{mvrnorm}}
 \code{\link[tibble]{tibble}}
 \code{\link[stats]{Uniform}}
}
