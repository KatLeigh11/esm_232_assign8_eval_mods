# Performance metric
## - metric: estimated annual minimal and maximal flow must both have a Pearson’s Correlation Coefficient greater than 0.6.

#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @return annual_min_cor


kat_pat_ruoyu_perform_metric = function(m,o, month, day, year, wy, wyd, grouping) {
  
  flow = cbind.data.frame(m,o, month, day, year,wy, wyd, grouping)
  # first lets get minimum yearly values
  
  tmp = flow %>% group_by(grouping) %>% summarize(mino=min(o),
                                            minm=min(m),
                                            maxo=max(o),
                                            maxm=max(m))
  
  annual_min_cor = cor(tmp$minm, tmp$mino)
  annual_max_cor = cor(tmp$maxm, tmp$maxo)
  
  return(list(annual_min_cor=annual_min_cor,
              annual_max_cor=annual_max_cor))
}
