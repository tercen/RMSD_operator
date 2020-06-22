library(tercen)
library(dplyr)

options("tercen.workflowId" = "d945b82176ce8b251b3ab7bfde057cd4")
options("tercen.stepId"     = "9bf0b2c6-988f-4e85-a853-526dda922d12")

getOption("tercen.workflowId")
getOption("tercen.stepId")

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
