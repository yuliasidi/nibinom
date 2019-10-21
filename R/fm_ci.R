#' @title Farrington-Manning confidence interval for difference in proportions
#' @description calculates (1-alpha)100\% confidence interval for difference in
#' binomial proportions, and rejects/accepts the null hypothesis with respect
#' to the pre-specifid fixed margin
#' @param df dataframe, output from sim_cont function
#' @param m2 numeric, pre-specified fixed margin
#' @param y string, name of the outcome column
#' @param alpha numeric, one-sided alpha, Default: 0.025
#' @return dataframe
#' @details returns dataframe with 11 columns and 1 row, inlcuding estimated
#' proportion/n in each group, difference in proportions(group 'c' - group 't'),
#' variance of that difference (using Wald's method), lower and upper bounds of
#' the confidence interval and null hypothesis decision based on m2.
#' @examples
#'  t <- sim_cont(
#'  p_C = 0.6, p_T = 0.5, n_arm = 200,
#'  mu1 = 4, mu2 = 100, sigma1 = 1,
#'  sigma2 = 20, r12 = -0.3,
#'  b1 = 0.1, b2 = -0.01)
#'  fm_ci(t, m2 = 0.1, 'y', alpha = 0.025)
#' @seealso
#'  \code{\link[rlang]{sym}}
#'  \code{\link[reshape2]{recast}}
#'  \code{\link[stats]{Normal}}
#' @rdname fm_ci
#' @export
#' @import dplyr
#' @importFrom rlang sym
#' @importFrom reshape2 recast
#' @importFrom stats qnorm
fm_ci <- function(df, m2, y, alpha = 0.025){
  df%>%
    dplyr::group_by(trt)%>%
    dplyr::summarise(phat = mean(!!rlang::sym(y), na.rm = T), n = sum(!is.na(!!rlang::sym(y))))%>%
    reshape2::recast(1 ~ trt + variable, measure.var = c("phat",'n'))%>%
    #dplyr::rename(p_T = T_phat, p_C = C_phat)%>%
    p_rmle(M2 = m2)%>%
    dplyr::mutate(phat_d = T_phat - C_phat,
           var_d = p_C.rmle*(1-p_C.rmle)/C_n + p_T.rmle*(1-p_T.rmle)/T_n,
           ci_l = phat_d - stats::qnorm(1-alpha)*sqrt(var_d),
           ci_u = phat_d + stats::qnorm(1-alpha)*sqrt(var_d),
           reject_h0 = dplyr::case_when(ci_u < m2 ~ 1, TRUE ~ 0))%>%
    dplyr::select(-`1`)
}

