
"%||%" <- function(a,b) if(is.null(a)) b else a
flanking <- "[\\!\\%\\^\\(\\)\\&\\|\\*\\-\\+\\=\\/\\, ]"

variable_present <- function(vars, expr_str){
  str_detect(expr_str, str_c(flanking,
                             "(",str_c(vars,
                                       collapse = "|"),
                             ")",flanking))
}
dependent_quos <- function(quos){
  str_ <- map_chr(quos, as_label)
  n <- length(quos)
  pmap_lgl(list(1:n, list(names(str_)), str_),
           function(x,y,z, n){
             if(x==1) return(FALSE)
             variable_present(y[1:x],z)
           }, n = n)
}