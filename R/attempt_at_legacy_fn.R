library(tidyverse)
library(rlang)
library(data.table)
library(glue)




DT <- data.table(
  V1 = 1:100,
  V2 = round(rnorm(100),2),
  V3 = factor(sample(c("Bird","Dog","Cat"), 100, T))
)

DT %>% group_by(test)

DT %>%
  mutate(
    V4= toupper(V3),
    V5 = case_when(V1%%2==1 ~ V4,
                   TRUE ~ tolower(V4)),
    V6 = V1 + V2
  )

DT <- DT %>% group_by(V3, V5)

DT2 <- copy(DT) %>% 
  mutate(across(c(V1,V2),
                list(t1 = ~ sum(.x), t2 = as.character),
                .names = "{.col}_{.fn}"),
         V7 = 20)


translate_quos <- function(x){
  
}


