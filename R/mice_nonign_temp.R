#' @title Non-ingorably MI for difference in proportions via MICE
#' @description two-stage MI for difference in proportions using MICE
#' @param dt dataframe
#' @param n_mi numeric, number of patient level imputations, Default: 5
#' @param m_mi numeric, number of model imputations, Default: 10
#' @param m2 numeric, margin
#' @param ci_method ci function name, Default: ci_method
#' @param seed_mice numeric
#' @param mu_C numeric, mean parameter for k multiplier normal distribution
#' for group 'c', Default: 1
#' @param sd_C numeric, stadnard deviation parameter for k multiplier normal
#' distribution for group 'c', Default: 0
#' @param mu_T Pnumeric, mean parameter for k multiplier normal distribution
#' for group 't',Default: 1
#' @param sd_T numeric, stadnard deviation parameter for k multiplier normal
#' distribution for group 't', Default: 0
#' @return final result following nested multiple imputation
#' @seealso
#'  \code{\link[mice]{make.predictorMatrix}},\code{\link[mice]{mice}}
#'  \code{\link[rlang]{get_env}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[stats]{Uniform}},\code{\link[stats]{Normal}}
#'  \code{\link[tidyr]{unnest}},\code{\link[tidyr]{nest}}
#' @rdname mice_nonign_temp
#' @import dplyr
#' @importFrom mice make.predictorMatrix mice
#' @importFrom tibble tibble
#' @importFrom purrr map map2
#' @importFrom stats runif rnorm
#' @importFrom tidyr unnest nest
mice_nonign_temp <- function(dt, n_mi = 5, m_mi = 10, m2,
                            ci_method=ci_method, seed_mice, mu_C = 1, sd_C = 0, mu_T = 1, sd_T = 0){

  set_env <- function(){

    thisenv <- new.env()
    thisenv$mice.out.phat <- list()
    thisenv

  }

  dt.C <- dt%>%
    dplyr::filter(trt=="C")%>%
    dplyr::select(pat_id, x1, x2, y.m)%>%
    dplyr::mutate(y.m = as.factor(y.m))

  dt.T <- dt%>%
    dplyr::filter(trt=="T")%>%
    dplyr::select(pat_id, x1, x2, y.m)%>%
    dplyr::mutate(y.m = as.factor(y.m))

  predM.C <- mice::make.predictorMatrix(data=dt.C)
  predM.C[, "pat_id"] <- 0

  predM.T <- mice::make.predictorMatrix(data=dt.T)
  predM.T[, "pat_id"] <- 0


  set.seed(seed_mice)

  thisenv <<- set_env()

  maxit <- 20

  #MICE for control arm
  dt.C.imp <- mice::mice(
    data = dt.C,
    m=n_mi,
    method = "logreg.p",
    predictorMatrix=predM.C,
    seed = seed_mice,
    maxit = maxit,
    printFlag = FALSE
  )

  phats <- thisenv$mice.out.phat[seq(maxit,length(thisenv$mice.out.phat),maxit)]

  imp.n <- tibble::tibble(n = seq(1,n_mi,1))

  #add the imputed phats per subject to the original incomplete data
  dt.C.phat <- imp.n%>%
    dplyr::mutate(dt.phat = purrr::map(n, .f = function(n){
      dt.C%>%
        dplyr::mutate(wy = seq(1,dplyr::n(),1),
                      trt = "C")%>%
        dplyr::left_join(phats[[n]]%>%
                           dplyr::mutate(p.thresh = stats::runif(dplyr::n())), by = 'wy')
    }))

  thisenv <<- set_env()

  #MICE for the new teratment
  dt.T.imp <- mice::mice(
    data = dt.T,
    m=n_mi,
    method = "logreg.p",
    predictorMatrix=predM.T,
    seed = seed_mice,
    maxit = 20,
    printFlag = FALSE
  )

  phats <- thisenv$mice.out.phat[seq(maxit,length(thisenv$mice.out.phat),maxit)]

  #add the imputed phats per subject to the original incomplete data
  dt.T.phat <- imp.n%>%
    dplyr::mutate(dt.phat = purrr::map(n, .f = function(n){
      dt.T%>%
        dplyr::mutate(wy = seq(1,dplyr::n(),1),
                      trt = "T")%>%
        dplyr::left_join(phats[[n]]%>%
                           dplyr::mutate(p.thresh = stats::runif(dplyr::n())), by = 'wy')
    }))


  set.seed(seed_mice)

  k.mults <- tibble::tibble(m = seq(1, m_mi,1),
                    k.C = stats::rnorm(m_mi, mu_C, sd_C),
                    k.T = stats::rnorm(m_mi, mu_T, sd_T))


  # multiply phats for each arm separetly, set 0/1 for incomplete outcomes and combine imputed perrr arm data
  nested.mi.res <-
    k.mults%>%
    dplyr::mutate(res = purrr::map2(k.C, k.T, .f = function(k.C, k.T){

      dt.C <-
        dt.C.phat%>%
        dplyr::mutate(dt.new = purrr::map(dt.phat, .f = function(df){
          df%>%
            dplyr::mutate(p.new = k.C * p)%>%
            dplyr::mutate(y.k = ifelse(p.new >= p.thresh,1,0),
                          y.m = ifelse(is.na(y.m) == T, y.k, as.numeric(y.m) - 1))%>%
            dplyr::select(pat_id, x1, x2, y.m, trt)

        }))%>%
        dplyr::select(-dt.phat)

      dt.T <-
        dt.T.phat%>%
        dplyr::mutate(dt.new = purrr::map(dt.phat, .f = function(df){
          df%>%
            dplyr::mutate(p.new = k.T * p)%>%
            dplyr::mutate(y.k = ifelse(p.new >= p.thresh,1,0),
                          y.m = ifelse(is.na(y.m) == T, y.k, as.numeric(y.m) - 1))%>%
            dplyr::select(pat_id, x1, x2, y.m, trt)

        }))%>%
        dplyr::select(-dt.phat)


      dt.all <-
        dplyr::bind_rows(dt.C%>%tidyr::unnest(),
                  dt.T%>%tidyr::unnest())%>%
        tidyr::nest(-n)%>%
        dplyr::mutate(ci.out = purrr::map(data, ci_method, m2 = m2, y = "y.m"))%>%
        dplyr::select(-data)%>%
        tidyr::unnest()#%>%
      #dplyr::select(n, phat_d, var_d)


    }))%>%
    tidyr::unnest()


  if(method!='wn')
  {
    mice.res <- nested.mi.res%>%
      dplyr::select(m, k.C, k.T, n, phat_d, var_d)%>%
      mi2comb()%>%
      dplyr::mutate(reject_h0 = dplyr::case_when(ci_u < m2 ~ 1, TRUE ~ 0))

  }

  if(method=='wn')
  {

    mice.res <- nested.mi.res%>%
      mi2comb_wn()%>%
      dplyr::mutate(reject_h0 = dplyr::case_when(ci_u < m2 ~ 1, TRUE ~ 0))

    if(pro_wnmi){
      mice.res <- nested.mi.res%>%
        proper_wnmi()%>%
        dplyr::mutate(reject_h0 = dplyr::case_when(ci_u < m2 ~ 1, TRUE ~ 0))
    }

  }

  return(mice.res)

}
