is_scoped_fn <- function(.dots){
  #check if first argument matches across, if_any, if_all
  str_detect(map_chr(.dots, as_label), "^(across|if_(any|all))")
}

unscope_dots <- function(.dots, .data){
  mask <- new_dt_translate_mask(.data)
  .dots <- map(.dots, eval_tidy, data = mask)
  return(.dots)
}

map_fns_quo <- function(.fns, quo, ...) UseMethod('map_fns_quo')

map_fns_quo.function <- function(.fns, quo,...) {}

interpret_fns <- function(.fns, ...) UseMethod('interpret_fns')

interpret_fns.formula <- function(.fns, ...) as_mapper(.fns, ...)
interpret_fns.list <- function(.fns, ...) map(.fns, interpret_fns, ...)
interpret_fns.function <- function(.fns, ...) .fns

to_list <- function(x) {
  if(inherits(x, "list")) return(x)
  list(x)
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
                  
                  loc <- tidyselect::eval_select(expr({{.cols}}), .dt)
                  loc <- exclude_group_vars(loc, .dt)
                  quo_names <- map(syms(names(loc)), ~quo(!!.x))
                  .fns <- to_list(interpret_fns(.fns, ...))
                  .fn <- names(.fns) %||% 1:length(.fns)
                  quo_ <- pmap(
                    list(
                      quo_names,
                      list(.fns),
                      list(.fn)
                      ),
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
