#' @title Newcombe confidence interval for difference in proportions
#' @description calculates (1-alpha)100\% confidence interval for difference in
#' binomial proportions, and rejects/accepts the null hypothesis with respect
#' to the pre-specifid fixed margin
#' @param df dataframe, output from sim_cont function
#' @param m2 numeric, pre-specified fixed margin
#' @param y string, name of the outcome column
#' @param alpha numeric, one-sided alpha, Default: 0.025
#' @return dataframe
#' @details returns dataframe with 8 columns and 1 row, inlcuding estimated
#' proportion/n in each group, difference in proportions(group 'c' - group 't'),
#' variance of that difference (using Wald's method), lower and upper bounds of
#' the confidence interval and null hypothesis decision based on m2.
#' @examples
#'  t <- sim_cont(
#'  p_C = 0.6, p_T = 0.5, n_arm = 200,
#'  mu1 = 4, mu2 = 100, sigma1 = 1,
#'  sigma2 = 20, r12 = -0.3,
#'  b1 = 0.1, b2 = -0.01)
#'  wn_ci(t, m2 = 0.1, 'y', alpha = 0.025)
#' @seealso
#'  \code{\link[rlang]{sym}}
#'  \code{\link[reshape2]{recast}}
#'  \code{\link[stats]{Normal}}
#' @rdname wn_ci
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom reshape2 recast
#' @importFrom stats qnorm
wn_ci <- function(df, m2, y, alpha = 0.025){

  df%>%
    dplyr::group_by(trt)%>%
    dplyr::summarise(phat = mean(!!rlang::sym(y), na.rm = T), n = sum(!is.na(!!rlang::sym(y))))%>%
    reshape2::recast(1 ~ trt + variable, measure.var = c("phat",'n'))%>%
    dplyr::mutate(z = stats::qnorm(1-alpha),
                  phat_d = C_phat-T_phat,
                  l_C = (C_phat + z^2/(2*C_n) -
                           z*sqrt((C_phat*(1-C_phat)+z^2/(4*C_n))/C_n))/(1+z^2/C_n),
                  u_C = (C_phat + z^2/(2*C_n) +
                           z*sqrt((C_phat*(1-C_phat)+z^2/(4*C_n))/C_n))/(1+z^2/C_n),

                  l_T = (T_phat + z^2/(2*T_n) -
                           z*sqrt((T_phat*(1-T_phat)+z^2/(4*T_n))/T_n))/(1+z^2/T_n),
                  u_T = (T_phat + z^2/(2*T_n) +
                           z*sqrt((T_phat*(1-T_phat)+z^2/(4*T_n))/T_n))/(1+z^2/T_n),

                  ci_l =  phat_d - sqrt((C_phat-l_C)^2+(u_T-T_phat)^2),
                  ci_u =  phat_d + sqrt((u_C-C_phat)^2+(T_phat-l_T)^2),
                  reject_h0 = dplyr::case_when(ci_u < m2 ~ 1, TRUE ~ 0))%>%
    dplyr::select(C_phat, C_n, T_phat, T_n, phat_d, ci_l, ci_u, reject_h0, -`1`)
}

