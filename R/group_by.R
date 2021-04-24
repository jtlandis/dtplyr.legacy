

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