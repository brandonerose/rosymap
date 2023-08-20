#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
#' @importFrom shiny column
col_12 <- function(...) {
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...) {
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...) {
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...) {
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...) {
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...) {
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...) {
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...) {
  column(1, ...)
}

drop_if <- function(x,drops) {
  x[which(!x%in%drops)]
}

list.files.real<-function(path){
  grep('~$', list.files(path), fixed = TRUE, value = TRUE, invert = TRUE)
}
mytheme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#434C5E"
  ),
  fresh::adminlte_sidebar(
    width = "300px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#000000"
  ),
  fresh::adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9",
    info_box_bg = "#D8DEE9"
  )
)
# .h1, h1,.h2, h2,.h3, h3,.p, p,.small-box h3{
#   color: #000000;
# }

fix_date<-function(dates){
  dates %>% strptime(format = "%m/%d/%Y") %>% as.character()
}
numbers_only <- function(x){!grepl("\\D", x)}

percent <- function(x){
  paste0(x*100," %")
}


sample1 <- function(x){
  sample(x,1)
}

make_table<-function(DF,selection="single"){
  # %>% DT::formatStyle(
  #   colnames(DF),
  #   color = "#000"
  # )
  DF %>% DT::datatable(
    selection = selection,
    editable = F,
    rownames = F,
    options = list(
      columnDefs = list(list(className = 'dt-center',targets = "_all")),
      paging = T,
      pageLength = 50,
      fixedColumns = TRUE,
      ordering = TRUE,
      scrollY = "300px",
      scrollX = T,
      # autoWidth = T,
      searching = T,
      # dom = 'Bfrtip',
      # buttons = c('csv', 'excel',"pdf"),
      scrollCollapse = F,
      stateSave = F
    ),
    class = "cell-border",
    filter = 'top',
    escape =F
  )
}
