library(tercen)
library(dplyr)

#library(tim)
#options("tercen.workflowId" = "6015a4dd34cef273755e1a1b1500427b")
#options("tercen.stepId"     = "d31241f6-173f-473a-9307-2b4b3c5c0882")

do.rmsd <- function(df) {
  
  out <- data.frame(
    .ci = df$.ci[1],
    .ri = df$.ri[1],
    rmsd = NaN
  )
  
  out$rmsd <- sqrt(mean((df$.x - df$.y)^2)) 
  return(out)
}

ctx <- tercenCtx()

df.out<- ctx  %>%
  select(.x, .y, .ci, .ri) %>%
  group_by(.ci, .ri) %>%
  do(do.rmsd(.)) %>%
  ctx$addNamespace() 

df.out%>%
  ctx$save()

#tim::build_test_data(res_table = df.out, ctx = ctx, test_name = "test1")
