is_scoped_fn <- function(.dots){
  #check if first argument matches across, if_any, if_all
  str_detect(as_label(.dots[[1]]), "^(across|if_(any|all))")
}

unscope_dots <- function(.dots, .data){
  mask <- new_dt_translate_mask(.data)
  .dots <- eval_tidy(.dots[[1]], mask)
  return(.dots)
}
new_dt_translate_mask <- function(.data){
  .dt <- .data
  top <- new_environment(list(
    cur_data = quo(.SD),
    cur_group_id = quo(.GRP),
    cur_group = quo(.BY),
    cur_group_rows = quo(.I),
    n = quo(.N)
  ))
  
  bottom <- env(top,
                across = function(.cols = everything(),
                                  .fns = NULL, ...,
                                  .names = NULL){
                  loc <- tidyselect::eval_select(expr(.cols), .dt)
                  loc <- exclude_group_vars(loc, .dt)
                  quo_names <- map(syms(names(loc)), ~quo(!!.x))
                  .fn <- names(.fns) %||% 1:length(.fns)
                  quo_ <- pmap(list(
                    quo_names,
                    list(.fns),
                    list(.fn)),
                    function(.col, .fns, .fn, .names, ...){
                      n <- length(list(...))
                      if(n==0) {
                        thequo <- map(.fns, ~quo(.x(!!.col)))
                      } else {
                        thequo <- map(.fns, ~quo(.x(!!.col, ...)))
                      }
                      
                      .col <- as_label(.col)
                      if(is.null(.names)){
                        if(length(.fn)==1){
                          names(thequo) <- glue("{.col}")
                        } else {
                          names(thequo) <- glue("{.col}_{.fn}")
                        }
                      } else {
                        names(thequo) <- glue(.names)
                      }
                      thequo
                    }, .names = .names, ...)
                  quo_ <- unlist(quo_)
                  return(quos(!!!quo_))
                })
  mask <- new_data_mask(bottom, top)
  
  return(mask)
}