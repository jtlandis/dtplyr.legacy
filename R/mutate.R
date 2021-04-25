

mutate.data.table <- function(.data, ...) {
  
  dt <- .data
  .dots <- enquos(..., .named = T)
  scoped_fn <- is_scoped_fn(.dots)
  if(any(scoped_fn)){
    .dots_list <- as.list(.dots)
    .dots_list[scoped_fn] <- unscope_dots(.dots_list[scoped_fn], dt)
    names(.dots_list)[scoped_fn] <- ""
    .dots <- quos(!!!unlist(.dots_list))
  }
  
  is_dpndnt <- dependent_quos(.dots)
  out <- if(any(is_dpndnt)){
    dpndnt_indx <- which(is_dpndnt)
    
    args_expr <- .dots[1:(dpndnt_indx-1)]
    dependent_expr <- .dots[dpndnt_indx:length(.dots)]
    mutate(
      eval_tidy(
        quo(dt[,`:=`(!!!args_expr)][]),
        dt
      ),
      !!!dependent_expr
    )
  } else {
    eval_tidy(
      quo(dt[,`:=`(!!!.dots)][]),
      dt
    )
  }
 as_tbl_dt(out)
}


mutate.grouped_dt <- function(.data, ...) {
  
  dt <- .data
  .dots <- enquos(..., .named = T)
  scoped_fn <- is_scoped_fn(.dots)
  if(any(scoped_fn)){
    .dots_list <- as.list(.dots)
    .dots_list[scoped_fn] <- unscope_dots(.dots_list[scoped_fn], dt)
    names(.dots_list)[scoped_fn] <- ""
    .dots <- quos(!!!unlist(.dots_list))
  }
  
  is_dpndnt <- dependent_quos(.dots)
  out <- if(any(is_dpndnt)){
    dpndnt_indx <- which(is_dpndnt)
    
    args_expr <- .dots[1:(dpndnt_indx-1)]
    dependent_expr <- .dots[dpndnt_indx:length(.dots)]
    mutate(
      eval_tidy(
        quo(dt[,`:=`(!!!args_expr), by = .(!!!groups(dt))][]),
        dt
      ),
      !!!dependent_expr
    )
  } else {
    eval_tidy(
      quo(dt[,`:=`(!!!.dots), by = .(!!!groups(dt))][]),
      dt
    )
  }
  as_tbl_dt(out)
}


