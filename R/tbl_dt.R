
as_tbl_dt <- function(x) UseMethod('as_tbl_dt')

as_tbl_dt.data.frame <- function(x) {
  x <- as.data.table(x)
  as_tbl_dt(x)
}

as_tbl_dt.data.table <- function(x) {
  if(inherits(x, 'tbl_dt')) return(x)
  structure(x, class = c('tbl_dt', class(x)))
}


print.tbl_dt <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
  pillar:::print.tbl(x, width = width, ..., n = n, n_extra = n_extra)
}

format.tbl_dt <- pillar:::format.tbl

tbl_format_setup.tbl_dt <- pillar:::tbl_format_setup.tbl
ctl_new_compound_pillar.tbl_dt <- pillar:::ctl_new_compound_pillar.tbl
ctl_new_pillar.tbl_dt <- pillar:::ctl_new_pillar.tbl
tbl_format_header.tbl_dt <- pillar:::tbl_format_header.tbl
tbl_format_body.tbl_dt <- pillar:::tbl_format_body.tbl
tbl_format_footer.tbl_dt <- pillar:::tbl_format_footer.tbl

test <- function(x){
  #browser()
  print(as_tbl_dt(x))
}
