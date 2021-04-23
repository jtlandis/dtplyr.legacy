library(tidyverse)
library(rlang)
library(data.table)
library(glue)

"%||%" <- function(a,b) if(is.null(a)) b else a

dependent_quos <- function(quos){
  str_ <- map_chr(quos, as_label)
  n <- length(quos)
  pmap_lgl(list(1:n, list(names(str_)), str_),
           function(x,y,z, n){
             if(x==1) return(FALSE)
             variable_present(y[1:x],z)
           }, n = n)
}

is_scoped_fn <- function(.dots){
  #check if first argument matches across, if_any, if_all
  str_detect(as_label(.dots[[1]]), "^(across|if_(any|all))")
}

unscope_dots <- function(.dots, .data){
  browser()
  mask <- new_dt_translate_mask()
  .args <- call_args(get_expr(.dots[[1]]))
  ..cols <- .args$.cols %||% quo(everything())
  loc <- tidyselect::eval_select(expr(), .data)
  loc <- exclude_group_vars(loc, .data, notify = T)
  return(.dots)
}

mutate.data.table <- function(.data, ...) {
  browser()
  dt <- .data
  .dots <- enquos(..., .named = T)
  if(is_scoped_fn(.dots)){
    .dots <- unscope_dots(.dots, dt)
  }

  is_dpndnt <- dependent_quos(.dots)
  if(any(is_dpndnt)){
    dpndnt_indx <- which(is_dpndnt)

    args_expr <- .dots[1:(dpndnt_indx-1)]
    dependent_expr <- .dots[dpndnt_indx:length(.dots)]
    mutate(
      eval_tidy(
        quo(dt[,`:=`(!!!args_expr)]),
        dt
      ),
      !!!dependent_expr
    )
  } else {
    eval_tidy(
      quo(dt[,`:=`(!!!.dots)]),
      dt
    )
  }
}

new_dt_translate_mask(.data){
  top <- new_environment(list(
    cur_data = quo(.SD),
    cur_group_id = quo(.GRP),
    cur_group = quo(.BY),
    cur_group_rows = quo(.I),
    n = quo(.N)
  ))

  bottom <- env(top,
             across = function(.cols = everything(),
                               .fns, ...,
                               .names = NULL){
               browser()
               loc <- tidyselect::eval_select(expr(.cols), .dt$.data)
               loc <- exclude_group_vars(loc, .dt_data)
               return(loc)
             })
  mask <- new_data_mask(bottom, top)
  mask$.dt <- as_data_pronoun(list(.data = .data))
  return(mask)
}


ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(group_vars(data), names(data))
  missing <- setdiff(group_loc, loc)
  if (length(missing) > 0) {
    vars <- names(data)[missing]
    if (notify) {
      inform(glue("Adding missing grouping variables: ",
                  paste0("`", names(data)[missing], "`",
                         collapse = ", ")))
    }
    loc <- c(set_names(missing, vars), loc)
  }
  loc
}

exclude_group_vars <- function(loc, data, notify = TRUE){
  group_loc <- match(group_vars(data), names(data))
  matches <- intersect(loc, group_loc)
  if (length(matches) > 0) {
    vars <- names(data)[matches]
    if (notify) {
      inform(glue("grouping variables cannot be imputed on: ",
                  paste0("`", names(data)[matches], "`",
                         collapse = ", ")))
    }
    loc <- setdiff(loc, matches)
  }
  loc
}

select.data.table <- function(.data, ...) {
  dt <- .data
  loc <- tidyselect::eval_select(expr(c(...)), .data)
  loc <- ensure_group_vars(loc, .data, notify = T)
  eval_tidy(quo(dt[,.(!!!syms(names(loc)))]), dt)
}

group_by.data.table <- function(.data, ...){
  dt <- .data
  .dots <- enquos(...)
  names_ <- map_chr(.dots, as_label)
  all_in <- map_lgl(names_, function(x, cols){any(x %in% cols)}, cols = colnames(dt))
  if(!all(all_in)){
    abort(glue("The following column(s) are not found in .data:\n",
               glue_collapse(glue("<{names_[!all_in]}>"), sep = ", ", last = " and ")))
  }

  attr(dt, "groups") <- names_
  structure(dt, class = c("grouped_dt",setdiff(class(dt), "grouped_dt")))

}

group_vars.grouped_dt <- function(x) attr(x, "groups")
groups.grouped_dt <- function(x) syms(attr(x, "groups"))

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
    V3 = paste(V1, V2)
  )

DT %>% mutate(across(.fns = as.character()))

flanking <- "[\\!\\%\\^\\(\\)\\&\\|\\*\\-\\+\\=\\/\\, ]"

variable_present <- function(vars, expr_str){
  str_detect(expr_str, str_c(flanking,
                             "(",str_c(vars,
                                       collapse = "|"),
                             ")",flanking))
}
