#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #reactivevalues-------
  values<-reactiveValues()
  values$DB<- load_DB()
  observeEvent(input$zoom,{
    values$DB <- values$DB %>% make_map(zoom = input$zoom,maptype = input$maptype)
    values$DB <- values$DB %>% run_kmeans(k = input$kclusters)
  })
  observeEvent(input$maptype,{
    values$DB <- values$DB %>% make_map(zoom = input$zoom,maptype = input$maptype)
    values$DB <- values$DB %>% run_kmeans(k = input$kclusters)
  })
  observeEvent(input$kclusters,{
    values$DB <- values$DB %>% run_kmeans(k = input$kclusters)
  })
  output$main_plot <- plotly::renderPlotly({
    values$DB$other$main_plot %>% plotly::ggplotly()
  })
  output$compare_plot <- plotly::renderPlotly({
    values$DB$other$main_plot %>% plotly::ggplotly()
  })
  observeEvent(input$random_colors,ignoreInit = T,{
    values$DB <- values$DB %>%
      k_means_plot(
        palette=sample1(rownames(RColorBrewer::brewer.pal.info))
      )
  })
  observeEvent(input$default_colors,ignoreInit = T,{
    values$DB <- values$DB %>%
      k_means_plot(
        palette=NULL
      )
  })
}
