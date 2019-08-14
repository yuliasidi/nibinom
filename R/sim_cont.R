#' @title Data simulation
#' @description Simulates data for noninferiority trials with binomal proportions.
#' @param p_C numeric, Probsability of events in control arm
#' @param p_T numeric, Probsability of events in treatment arm
#' @param n_arm numeric, number of observations per arm (balanced)
#' @param mu1 numeric, mean of continuous covariate 1 at baseline
#' @param mu2 numeric, mean of continuous covariate 2 at baseline
#' @param sigma1 numeric, standard deviation of continuous covariate 1 at
#'   baseline
#' @param sigma2 numeric, standard deviation of continuous covariate 2 at
#'   baseline
#' @param r12 numeric, correlation between covariate 1 and 2
#' @param b1 numeric, effect of covariate 1 on the outcome
#' @param b2 numeric, effect of covariate 2 on the outcome
#' @return tibble
#' @details The output contains a tibble with 6 columns: pat_id, trt, trtn, y,
#'    x1, x2.
#' @examples
#'
#'  sim_cont(
#'  p_C = 0.6, p_T = 0.5, n_arm = 200,
#'  mu1 = 4, mu2 = 100, sigma1 = 1,
#'  sigma2 = 20, r12 = -0.3,
#'  b1 = 0.1, b2 = -0.01)
#'
#' @seealso
#'  \code{\link[MASS]{mvrnorm}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[stats]{Uniform}}
#' @rdname sim_cont
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom tibble tibble
#' @import dplyr
#' @importFrom stats runif
sim_cont <- function(p_C, p_T, n_arm, mu1 ,mu2, sigma1, sigma2, r12, b1, b2){

  xcov <- matrix(c(sigma1^2, sigma1*sigma2*r12, sigma1*sigma2*r12, sigma2^2), nrow = 2, ncol = 2)

  bt <- log(p_T*(1-p_C)/((1-p_T)*p_C))

  b0 <- log(p_C/(1-p_C)) - b1*mu1 - b2*mu2

  x.T <- MASS::mvrnorm(n = n_arm, mu = c(mu1,mu2), Sigma = xcov)
  x.C <- MASS::mvrnorm(n = n_arm, mu = c(mu1,mu2), Sigma = xcov)

  dt <- tibble::tibble(x1 = x.T[,1], x2 = x.T[,2], trt = "T")%>%
    dplyr::bind_rows(tibble::tibble(x1 = x.C[,1], x2 = x.C[,2], trt = "C"))%>%
    dplyr::mutate(trtn = dplyr::case_when(trt=='T' ~ 1,
                                   TRUE ~ 0))%>%
    dplyr::mutate(p.y = 1/(1+exp(-b0 - b1*x1 - b2*x2 - bt*trtn)),
                  p.thresh = stats::runif(dplyr::n()))%>%
    dplyr::mutate(y = as.numeric(p.y >= p.thresh),
                  pat_id = seq(1,dplyr::n(),1))%>%
    dplyr::select(pat_id, trt, trtn, y, x1, x2)

  return(dt)

}
