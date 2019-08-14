#' @title impose missing values
#' @description imposes missing values
#' @param dt dataframe, on which missing values are imposed
#' @param b_trt numeric, group missingness parameter, Default: 0
#' @param b_y numeric, outcome missingness parameter, Default: 0
#' @param b_x1 numeric, covariate x1 missingness parameter, Default: 0
#' @param b_x2 numeric, covariate x2 missingness parameter, Default: 0
#' @param b_ty numeric, covariatet treatment by group interaction, Default: 0
#' @param do numeric, target drop-out rate, Default: 0.1
#' @param seed numeric, seed specification before deleting observations
#' @return original dataframe with missing outcome values
#' @details this functtion returns the oroginal dataframe, where outcome
#' variable y is replaces with incomplete outcome y.m
#' @examples
#'  t <- sim_cont(
#'  p_C = 0.6, p_T = 0.5, n_arm = 200,
#'  mu1 = 4, mu2 = 100, sigma1 = 1,
#'  sigma2 = 20, r12 = -0.3,
#'  b1 = 0.1, b2 = -0.01)
#'  tm <- mimpose(t, b_x1 = 0.2, seed = 1555)
#' @seealso
#'  \code{\link[stats]{Uniform}}
#' @rdname mimpose
#' @export
#' @import dplyr
#' @importFrom stats runif
mimpose <- function(dt, b_trt = 0, b_y = 0, b_x1 = 0, b_x2 = 0, b_ty = 0, do = 0.1, seed)
{

  if (b_ty==0){

    tmp <- dt%>%
      dplyr::summarise_at( .vars = c('trtn', 'x1', 'x2', 'y'), .funs= list(mean="mean"))%>%
      dplyr::mutate(int = log(do/(1-do)) - b_trt*trtn_mean - b_y*y_mean - b_x1*x1_mean/10 - b_x2*x2_mean/100)

    set.seed(seed)

    dt.miss <- dt%>%
      dplyr::mutate(p = 1/(1+exp(-1*(tmp$int + b_trt*trtn + b_y*y + b_x1*x1/10 + b_x2*x2/100))),
                    p.thresh = stats::runif(dplyr::n()))%>%
      dplyr::mutate(y.m = ifelse(p >= p.thresh,NA_integer_,y))%>%
      dplyr::select(-c(p, p.thresh, y))
  }

  else{

    tmp.t <- dt%>%
      dplyr::filter(trt == 'T')%>%
      dplyr::summarise_at( .vars = c('x1', 'x2', 'y'), .funs= list(mean="mean"))%>%
      dplyr::mutate(int = log(do/(1-do)) - b_y*y_mean - b_x1*x1_mean/10 - b_x2*x2_mean/100-
                      b_ty*y_mean)

    tmp.c <- dt%>%
      dplyr::filter(trt == 'C')%>%
      dplyr::summarise_at( .vars = c('x1', 'x2', 'y'), .funs= list(mean="mean"))%>%
      dplyr::mutate(int = log(do/(1-do)) - b_y*y_mean - b_x1*x1_mean/10 - b_x2*x2_mean/100)


    set.seed(seed)

    dt.miss <-
      dt%>%
      dplyr::filter(trt=="T")%>%
      dplyr::mutate(p = 1/(1+exp(-1*(tmp.t$int + (b_y+b_ty)*y + b_x1*x1/10 + b_x2*x2/100))))%>%
      dplyr::mutate(p.thresh = stats::runif(dplyr::n()))%>%
      dplyr::bind_rows(dt%>%
                  dplyr::filter(trt=="C")%>%
                  dplyr::mutate(p = 1/(1+exp(-1*(tmp.c$int + b_y*y + b_x1*x1/10 + b_x2*x2/100))))%>%
                  dplyr::mutate(p.thresh = stats::runif(dplyr::n())))%>%

      dplyr::mutate(y.m = ifelse(p >= p.thresh,NA_integer_,y))%>%
      dplyr::select(-c(p, p.thresh, y))

  }

  return(dt.miss)
}
