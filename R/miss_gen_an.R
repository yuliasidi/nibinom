#' @title generation and analysis of incomplete data
#' @description imposes missing values, and performs analysis of incomplete data
#' @param dt dataframe, on which missing values are imposed
#' @param m2 numeric, margin
#' @param b_trt numeric, group missingness parameter, Default: 0
#' @param b_y numeric, outcome missingness parameter, Default: 0
#' @param b_x1 numeric, covariate x1 missingness parameter, Default: 0
#' @param b_x2 numeric, covariate x2 missingness parameter, Default: 0
#' @param b_ty numeric, covariatet treatment by group interaction, Default: 0
#' @param do numeric, target drop-out rate, Default: 0.1
#' @param seed numeric, seed specification before deleting observations
#' @param dt_out logic, determines if the incomplete dataset needs to be
#' returned (if returned, no analysis will be done), Default: FALSE
#' @param sing_anal logic, determines if single value analysis needs to be
#' performed (complete case analysis, best and worst case scenario imputation),
#'Default: TRUE
#' @param mice_anal logic, determines if multiple imputation via MICE needs to
#' be performed (nested imputation), Default: FALSE
#' @param bw_anal logic, determines if the following single value imputation
#' needs to be performed: best case scenario for group 'c' and worst case
#' scenario for 't', Default: FALSE
#' @param mu_C numeric, mean parameter for k multiplier normal distribution
#' for group 'c', Default: 1
#' @param sd_C numeric, stadnard deviation parameter for k multiplier normal
#' distribution for group 'c', Default: 0
#' @param mu_T Pnumeric, mean parameter for k multiplier normal distribution
#' for group 't',Default: 1
#' @param sd_T numeric, stadnard deviation parameter for k multiplier normal
#' distribution for group 't', Default: 0
#' @param ci_method ci function name, Default: wald_ci
#' @param alpha numeric, one-sided alpha, Default: 0.025
#' @param seed_mice numeric, Default: seed_mice
#' @param n_mi numeric, number of multiple imputed patient level data
#' @param m_mi numeric, number of multiple imputed models
#' @param method character, method to construct confidence interval,
#'   Default: c('wald','wn','fm')
#' @param pro_wnmi logic, specifies whether proper wn cobminaiton rules should
#' be used, if TRUE, then bin2mi package needs to be used, Default: FALSE
#' @return list
#' @examples
#'  t <- sim_cont(
#'  p_C = 0.6, p_T = 0.5, n_arm = 200,
#'  mu1 = 4, mu2 = 100, sigma1 = 1,
#'  sigma2 = 20, r12 = -0.3,
#'  b1 = 0.1, b2 = -0.01)
#'  miss_gen_an(t, m2 = 0.1 , b_x1 = 0.5, do = 0.1, seed = 1555, seed_mice = 1555, alpha = 0.025)
#' @seealso
#'  \code{\link[tidyr]{spread}}
#'  \code{\link[stats]{glm}}
#'  \code{\link[purrr]{set_names}}
#' @rdname miss_gen_an
#' @export
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom stats glm
#' @importFrom purrr set_names
miss_gen_an <- function(dt,
                        m2,
                        b_trt = 0,
                        b_y = 0,
                        b_x1 = 0,
                        b_x2 = 0,
                        b_ty = 0,
                        do = 0.1,
                        seed,
                        dt_out = FALSE,
                        sing_anal = TRUE,
                        mice_anal = FALSE,
                        bw_anal = FALSE,
                        mu_C =  1, sd_C = 0,
                        mu_T =  1, sd_T = 0,
                        ci_method = wald_ci,
                        seed_mice,
                        alpha = 0.025,
                        n_mi,
                        m_mi,
                        method = c('wald','wn','fm'),
                        pro_wnmi = FALSE)
{

  out <- mimpose(dt = dt, b_trt = b_trt, b_y = b_y, b_x1 = b_x1, b_x2 = b_x2, b_ty = b_ty, do = do, seed = seed)

  do.arm <- out%>%
    dplyr::group_by(trt)%>%
    dplyr::summarise(do = mean(is.na(y.m)))%>%
    tidyr::spread(key = trt, value = 'do')

  if (dt_out){
    return(out)
  }

  else{
    if (sing_anal){
      #CCA
      out.ci.cca <- ci_method(out, m2, 'y.m', alpha)%>%
        dplyr::select(C_phat, T_phat, phat_d,  ci_l,  ci_u, reject_h0)%>%
        dplyr::mutate(strategy = 'cca')


      #best-case scenario
      out.ci.best <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,1))%>%
        ci_method(m2, 'y', alpha)%>%
        dplyr::select(C_phat, T_phat, phat_d,  ci_l,  ci_u, reject_h0)%>%
        dplyr::mutate(strategy = 'best')


      #worst-case scenario
      out.ci.worst <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,0))%>%
        ci_method(m2, 'y', alpha)%>%
        dplyr::select(C_phat, T_phat, phat_d,  ci_l,  ci_u, reject_h0)%>%
        dplyr::mutate(strategy = 'worst')

      out.ci <- dplyr::bind_rows(out.ci.cca, out.ci.best, out.ci.worst)

    }


    #mice
    if (mice_anal){

      out.ci <- out%>%
        mice_nonign(n_mi = n_mi, m_mi = m_mi, ci_method=ci_method, m2=m2, seed_mice = seed_mice,
                    mu_C =  mu_C, sd_C = sd_C,
                    mu_T =  mu_T, sd_T = sd_T,
                    method = method,
                    alpha=alpha,
                    pro_wnmi = pro_wnmi)%>%
        dplyr::mutate(strategy = "nested mice",
                      k.C.spec = sprintf("normal(%s, %s)", mu_C, sd_C),
                      k.T.spec = sprintf("normal(%s, %s)", mu_T, sd_T))
    }

    #bw for mnars only
    if (bw_anal){

      out.ci <- out%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,
                               ifelse(trt=="C",1,0)))%>%
        ci_method(m2,'y', alpha)%>%
        dplyr::select(C_phat, T_phat, phat_d,  ci_l,  ci_u, reject_h0)%>%
        dplyr::mutate(strategy = 'bw')
    }



    anal.return <- list(out.ci, do.arm)%>%purrr::set_names(c("ci","do"))

    return(anal.return)

  }

}


