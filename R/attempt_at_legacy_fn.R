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
    V4= paste(V3,V2),
    V5 = case_when(V1%%2==1 ~ paste(V4, V3),
                   TRUE ~ ""),
    V6 = paste(V1, V2)
  )

DT <- DT %>% group_by(V3)

DT2 <- copy(DT) %>% mutate(across(.fns = list(chr = as.character, fct = factor), .names = "{.col}_{.fn}"))

my_mask <- function(.data) {
  top <- new_environment(list(c = base::paste))
  bottom <- env(top,
    head = function(x, n = 5, ...){
      .data[1:n,]
    }
  )
  
  # env_ <- new_environment(list(.data = .data))
  # environment(bottom$head) <- env_
  
  new_data_mask(bottom, top)
  
}

eval_tidy(quo(head(mtcars, n = 10)), m)
