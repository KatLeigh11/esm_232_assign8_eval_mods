#' lowflowmetrics
#'
#' Compute percent error between observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  month month
#' @param  day day
#' @param  year year
#' @return annual_min_cor


check_minannual = function(m,o, month, day, year,wy) {

  flow = cbind.data.frame(m,o, month, day, year,wy)
  # first lets get minimum yearly values
  
  tmp = flow %>% group_by(wy) %>% summarize(mino=min(o), minm=min(m))

    annual_min_cor = cor(tmp$minm, tmp$mino)
  
  
  return(annual_min_cor=annual_min_cor)
}
