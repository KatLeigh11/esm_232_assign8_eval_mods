#' flowmetrics
#'
#' Performance metrics: the Pearson’s Correlation Coefficients of estimated annual minimal and maximal flow versus the annual minimal and maximal of actual observations
#' 
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @param  wy water year
#' @param  grouping grouping variable, could be month, day, year or water year
#' 
#' @return annual_max_cor and annual_min_cor
#' @author Pat Byrne, Kat Leigh, Ruoyu Wang


perform_metric = function(m,o, month, day, year, wy, grouping) {
  
  flow = cbind.data.frame(m,o, month, day, year,wy, grouping)
  
  
  tmp = flow %>% 
    group_by(grouping) %>% 
    summarize(mino=min(o),
              minm=min(m),
              maxo=max(o),
              maxm=max(m))
  
  annual_min_cor = cor(tmp$minm, tmp$mino)
  annual_max_cor = cor(tmp$maxm, tmp$maxo)
  
  return(list(annual_min_cor=annual_min_cor,
              annual_max_cor=annual_max_cor))
}
