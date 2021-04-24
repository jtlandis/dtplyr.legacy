

mutate.data.table <- function(.data, ...) {
  
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
}