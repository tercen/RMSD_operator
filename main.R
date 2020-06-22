library(tercen)
library(dplyr)

do.rmsd <- function(df) {
  
  out <- data.frame(
    .ci = df$.ci[1],
    .ri = df$.ri[1],
    rmsd = NaN
  )
  
  out$rmsd <- sqrt(mean((df$.x - df$.y)^2)) 
  return(out)
}

(ctx = tercenCtx())  %>%
  select(.x, .y, .ci, .ri) %>%
  group_by(.ci, .ri) %>%
  do(do.rmsd(.)) %>%
  ctx$addNamespace() %>%
  ctx$save()
