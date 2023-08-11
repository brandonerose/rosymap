#' @import shiny
#' @import shinydashboard
dbBody<-function(){
  dashboardBody(
    tabItems(
      #home--------
      tabItem(
        "home",
        fluidRow(
          box(
            title = h1("Main Map"),
            width = 6,
            height = 600,
            plotly::plotlyOutput("main_plot",height = 500)
          ),
          box(
            title = h1("Comparison Map"),
            width = 6,
            height = 600,
            plotly::plotlyOutput("compare_plot",height = 500)
          )
        )
        # dt_output('edit rows but disable certain columns (editable = list(target = "row", disable = list(columns = c(2, 4, 5))))', 'x10')
      )
    )
  )
}
