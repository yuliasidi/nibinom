#' @title Missingness parameter values
#' @description assigns missingness parameter values
#' @param do numeric, Default: 0.2, drop out rate
#' @param atype string, analysis type: "sing" or mice
#' @return tibble
#' @details returns tibble with 6 columns: missingness type, and parameter
#' used in imposing missing data
#' @examples
#' mpars(atype = "sing")
#' @seealso
#'  \code{\link[tibble]{tibble}}
#' @rdname mpars
#' @export
#' @importFrom tibble tibble
mpars <- function(do = 0.2, atype){


  if(do == 0.2 & atype =="sing"){

    m_param <- tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mnar1",
                                  "mnar2"),
                      bt    = c(0,  0,  0.3,  0.6,  0.9, -0.3, -0.6, -.9,   0,    0),
                      bx2   = c(0,  2,  2,    2,    2,    2,    2,     2,   0,    0),
                      bx1   = c(0,  0,  0,    0,    0,    0,    0,     0,   0,    0),
                      by    = c(0,  0,  0,    0,    0,    0,    0,     0,  -0.4,    2),
                      b.ty  = c(0,  0,  0,    0,    0,    0,    0,     0,  -0.8,   -2))
  }

  if(atype%in%c("mice","bw")){

    m_param <- tibble::tibble(missing = c( "mnar1", "mnar2"),
                      bt    = c(   0,    0),
                      bx2   = c(   0,    0),
                      bx1   = c(   0,    0),
                      by    = c(-0.4,    2),
                      b.ty  = c(-0.8,   -2))
  }

  if(do == 0.15 & atype =="sing"){

    m_param <- tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mnar1", "mnar2"),
                      bt   = c(0, 0, 0.4, 0.8, -0.4, -0.8, 0, 0),
                      bx2  = c(0, 1.5, 1.5, 1.5, 1.5, 1.5, 0, 0),
                      bx1  = c(0, 0, 0, 0, 0, 0, 0, 0),
                      by   = c(0, 0, 0, 0, 0, 0, -0.4,  2),
                      b.ty = c(0, 0, 0, 0, 0, 0, -0.8, -2))

  }


  if(do == 0.1 & atype =="sing"){

    m_param <- tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mnar1", "mnar2"),
                      bt   = c(0, 0, 0.5, -0.5, 0, 0),
                      bx2  = c(0, 1.5, 1.5, 1.5, 0, 0),
                      bx1  = c(0, 0, 0, 0, 0, 0),
                      by   = c(0, 0, 0, 0, -0.4,  2),
                      b.ty = c(0, 0, 0, 0, -0.8, -2))

  }

  if(do == 0.05 & atype =="sing"){

    m_param <- tibble::tibble(missing = c("mcar", "mar", "mnar1", "mnar2"),
                      bt   = c(0, 0, 0, 0),
                      bx2  = c(0, 1.5, 0, 0),
                      bx1  = c(0, 0, 0, 0),
                      by   = c(0, 0, -0.4,  2),
                      b.ty = c(0, 0, -0.8, -2))

  }

  return(m_param)
}
