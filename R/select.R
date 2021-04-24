



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
    loc <- loc[setdiff(loc, matches)]
  }
  loc
}

select.data.table <- function(.data, ...) {
  dt <- .data
  loc <- tidyselect::eval_select(expr(c(...)), .data)
  loc <- ensure_group_vars(loc, .data, notify = T)
  eval_tidy(quo(dt[,.(!!!syms(names(loc)))]), dt)
}
