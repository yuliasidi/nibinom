#' @title combintion following nested multiple imputation for Wilson/Newcombe
#' @description combines data following nested multiple imputation if Wilson/
#' Newcombe method is used for difference in binomial proportions
#' @param dt dataframe
#' @param alpha numeric, one-sided alpha, Default: 0.025
#' @return dataframe, final result following multiple imputation and
#' combination
#' @seealso
#'  \code{\link[stats]{Normal}}
#' @rdname mi2comb_wn
#' @import dplyr
#' @importFrom stats qnorm
mi2comb_wn <- function(dt, alpha = 0.025){

  #this is a plug-in WN method
  sum.all <- dt%>%
    dplyr::summarise(qbar.c = mean(C_phat), qbar.t = mean(T_phat),
                     n.c = mean(C_n), n.t = mean(T_n))%>%
    dplyr::mutate(qbar.d = qbar.c - qbar.t,
                  z = stats::qnorm(1-alpha),
                  l.c = (qbar.c + z^2/(2*n.c) -
                           z*sqrt((qbar.c*(1-qbar.c)+z^2/(4*n.c))/n.c))/(1+z^2/n.c),
                  u.c = (qbar.c + z^2/(2*n.c) +
                           z*sqrt((qbar.c*(1-qbar.c)+z^2/(4*n.c))/n.c))/(1+z^2/n.c),

                  l.t = (qbar.t + z^2/(2*n.t) -
                           z*sqrt((qbar.t*(1-qbar.t)+z^2/(4*n.t))/n.t))/(1+z^2/n.t),
                  u.t = (qbar.t + z^2/(2*n.t) +
                           z*sqrt((qbar.t*(1-qbar.t)+z^2/(4*n.t))/n.t))/(1+z^2/n.t),

                  ci_l = qbar.d-sqrt((qbar.c-l.c)^2+(u.t-qbar.t)^2),
                  ci_u = qbar.d+sqrt((u.c-qbar.c)^2+(qbar.t-l.t)^2)
    )%>%
    dplyr::select(qbar.c, qbar.t, qbar.d, ci_l, ci_u)

  return(sum.all)

}

