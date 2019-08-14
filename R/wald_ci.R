#' @title Wald confidence interval for difference in proportions
#' @description calculates (1-alpha)100\% confidence interval for difference in
#' binomial proportions, and rejects/accepts the null hypothesis with respect
#' to the pre-specifid fixed margin
#' @param df dataframe, output from sim_cont function
#' @param m2 numeric, pre-specified fixed margin
#' @param y string, name of the outcome column
#' @param alpha numeric, one-sided alpha, Default: 0.025
#' @return dataframe
#' @details returns dataframe with 9 columns and 1 row, inlcuding estimated
#' proportion/n in each group, difference in proportions(group 'c' - group 't'),
#' variance of that difference (using Wald's method), lower and upper bounds of
#' the confidence interval and null hypothesis decision based on m2.
#' @examples
#'  t <- sim_cont(
#'  p_C = 0.6, p_T = 0.5, n_arm = 200,
#'  mu1 = 4, mu2 = 100, sigma1 = 1,
#'  sigma2 = 20, r12 = -0.3,
#'  b1 = 0.1, b2 = -0.01)
#'  wald_ci(t, m2 = 0.1, 'y', alpha = 0.025)
#' @seealso
#'  \code{\link[rlang]{sym}}
#'  \code{\link[reshape2]{recast}}
#'  \code{\link[stats]{Normal}}
#' @rdname wald_ci
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom reshape2 recast
#' @importFrom stats qnorm
wald_ci <- function(df, m2, y, alpha = 0.025){
  df%>%
    dplyr::group_by(trt)%>%
    dplyr::summarise(phat = mean(!!rlang::sym(y), na.rm = T), n = sum(!is.na(!!rlang::sym(y))))%>%
    reshape2::recast(1 ~ trt + variable, measure.var = c("phat",'n'))%>%
    dplyr::mutate(phat_d = C_phat-T_phat,
           var_d = C_phat*(1-C_phat)/(C_n) + T_phat*(1-T_phat)/(T_n),
           ci_l = C_phat-T_phat - stats::qnorm(1-alpha)*sqrt(var_d),
           ci_u = C_phat-T_phat + stats::qnorm(1-alpha)*sqrt(var_d),
           reject_h0 = dplyr::case_when(ci_u < m2 ~ 1, TRUE ~ 0))%>%
    dplyr::select(-`1`)
}

