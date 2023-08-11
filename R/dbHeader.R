#' @import shiny
#' @import shinydashboard
dbHeader<-function(){
  shinydashboardPlus::dashboardHeader(
    title = tagList(
      span(class = "logo-lg", .packageName),
      tags$a(
        href="https://thecodingdocs.com",
        target="_blank",
        tags$img(src = "www/logo.png", width="100%")
      )
    )
  )
}
