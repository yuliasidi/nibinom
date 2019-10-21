#' @import dplyr
p_rmle <- function(df, M2){
  df%>%
    dplyr::mutate(theta = T_n/C_n,
                  a = 1 + theta,
                  b = -1*(1 + theta + C_phat + theta*T_phat + M2*(2 + theta)),
                  c = M2^2 + M2*(2*C_phat + theta + 1) + C_phat + theta*T_phat,
                  d = -C_phat*M2*(1 + M2))%>%
    dplyr::mutate(v = (b/(3*a))^3 - b*c/(6*a^2) + d/(2*a),
                  u = sign(v)*sqrt((b/(3*a))^2 - c/(3*a)),
                  u = ifelse(u!=0,u,0.00001),
                  w = (pi+acos(v/(u^3)))/3)%>%
    dplyr::mutate(p_C.rmle = 2*u*cos(w) - b/(3*a),
                  p_T.rmle = p_C.rmle - M2)%>%
    dplyr::select(-a, -b, -c, -d, -v, -u, -w, -theta)
}
