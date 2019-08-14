#' @title combintion following nested multiple imputation
#' @description combines data following nested multiple imputation
#' @param dt dataframe
#' @param alpha numeric, one-sided alpha, Default: 0.025
#' @return dataframe, final result following multiple imputation and
#' combination
#' @seealso
#'  \code{\link[stats]{cor}},\code{\link[stats]{TDist}}
#' @rdname mi2comb
#' @import dplyr
#' @importFrom stats var qt
mi2comb <- function(dt,alpha = 0.025){

  #summaries per imputation model
  sum.by.m <-
    dt%>%
    dplyr::group_by(m)%>%
    dplyr::summarise(qbar.m = mean(phat_d),
                     q.m.var = stats::var(phat_d),
                     ubar.m = mean(var_d),
                     n = dplyr::n())

  #overall summary
  sum.all <-
    sum.by.m%>%
    dplyr::summarise(qbar = mean(qbar.m),
                     ubar = mean(ubar.m),
                     b = stats::var(qbar.m),
                     w = mean(q.m.var),
                     n = mean(n),
                     m = dplyr::n())%>%
    dplyr::mutate(t = ubar + (1 + 1/m)*b + (1 - 1/n)*w,
                  v_1 = ((1 + 1/m)*b/t)^2/(m-1) + ((1 - 1/n)*w/t)^2/(m*(n-1)),
                  v = floor(1/v_1)
    )%>%
    dplyr::mutate(ci_l = qbar - stats::qt(1-alpha, v)*sqrt(t),
                  ci_u = qbar + stats::qt(1-alpha, v)*sqrt(t))

  return(sum.all)

}

