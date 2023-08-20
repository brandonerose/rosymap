#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #UI init -------
  output$com_ui_lat <- renderUI({
    selectizeInput(
      inputId = "com_ui_lat_",
      label = "Latitude Column",
      choices = NULL
    )
  })
  output$com_ui_lon <- renderUI({
    selectizeInput(
      inputId = "com_ui_lon_",
      label = "Longitude Column",
      choices = NULL
    )
  })
  output$com_ui_group <- renderUI({
    selectizeInput(
      inputId = "com_ui_group_",
      label = "Group Column",
      choices = NULL
    )
  })
  output$eoi_ui_lat <- renderUI({
    selectizeInput(
      inputId = "eoi_ui_lat_",
      label = "Latitude Column",
      choices = NULL
    )
  })
  output$eoi_ui_lon <- renderUI({
    selectizeInput(
      inputId = "eoi_ui_lon_",
      label = "Longitude Column",
      choices = NULL
    )
  })
  output$eoi_ui_group <- renderUI({
    selectizeInput(
      inputId = "eoi_ui_group_",
      label = "Group Column",
      choices = NULL
    )
  })
  output$int_ui_lat <- renderUI({
    selectizeInput(
      inputId = "int_ui_lat_",
      label = "Latitude Column",
      choices = NULL
    )
  })
  output$int_ui_lon <- renderUI({
    selectizeInput(
      inputId = "int_ui_lon_",
      label = "Longitude Column",
      choices = NULL
    )
  })
  output$int_ui_group <- renderUI({
    selectizeInput(
      inputId = "int_ui_group_",
      label = "Group Column",
      choices = NULL
    )
  })
  #reactivevalues-------
  values<-reactiveValues()
  values$DB<- load_DB()
  values$uploads<- NULL

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
  #obserevent files -------
  observeEvent(input$eoi_upload_file,ignoreInit = T,{
    # values$DB <-
    req(input$eoi_upload_file)
    values$uploads$m1_eoi <-rio::import(input$eoi_upload_file$datapath)

  })
  observeEvent(input$int_upload_file,ignoreInit = T,{
    # values$DB <-
    req(input$int_upload_file)
    values$uploads$m1_int <-rio::import(input$int_upload_file$datapath)

  })
  observeEvent(input$com_upload_file,ignoreInit = T,{
    # values$DB <-
    req(input$com_upload_file)
    values$uploads$m2_com <-rio::import(input$com_upload_file$datapath)
    cols <- values$uploads$m2_com%>% colnames() %>% as.character()
    updateSelectizeInput(
      session,
      inputId ="com_ui_lat_",
      choices = cols,
      selected =  ifelse("latitude"%in%cols,"latitude",ifelse("Latitude"%in%cols,"Latitude",ifelse("lat"%in%cols,"lat",ifelse("Lat"%in%cols,"Lat",NULL))))
    )
    updateSelectizeInput(
      session,
      inputId ="com_ui_lon_",
      choices = cols,
      selected =  ifelse("longitude"%in%cols,"longitude",ifelse("Longitude"%in%cols,"Longitude",ifelse("lon"%in%cols,"lon",ifelse("Lon"%in%cols,"Lon",NULL))))
    )
    updateSelectizeInput(
      session,
      inputId ="com_ui_group_",
      choices = cols,
      selected =  ifelse("group"%in%cols,"group",ifelse("Group"%in%cols,"Group",NULL))
    )
  })
  #observevent save -----
  observeEvent(input$com_upload_save,ignoreInit = T,{
    values$DB <-load_DB(blank = T)
    coordinates <- values$uploads$m2_com
    if(!is.null(input$com_ui_lat_)){
      lat <- coordinates[[input$com_ui_lat_]]
      coordinates[[input$com_ui_lat_]] <- NULL
      coordinates$latitude <- lat
    }
    if(!is.null(input$com_ui_lon_)){
      lon <- coordinates[[input$com_ui_lon_]]
      coordinates[[input$com_ui_lon_]] <- NULL
      coordinates$longitude <- lon
    }
    if(!is.null(input$com_ui_group_)){
      group <- coordinates[[input$com_ui_group_]]
      coordinates[[input$com_ui_group_]] <- NULL
      coordinates$group <- group
    }
    values$DB$data$coordinates <- coordinates
    values$DB <- values$DB %>% make_map(zoom = input$zoom)
    values$DB <- values$DB %>% run_kmeans(input$kclusters)
  })
  #renderDT ----------
  output$eoi_upload_table <- DT::renderDT({values$uploads$m1_eoi %>% make_table()})
  output$int_upload_table <- DT::renderDT({values$uploads$m1_int %>% make_table()})
  output$com_upload_table <- DT::renderDT({values$uploads$m2_com %>% make_table()})

  observeEvent(input$use_sample_data,ignoreInit = T,{
    values$DB <- load_DB(blank = T)
    values$DB$data$coordinates <- sample_rosymap_data
    values$DB <- values$DB %>% make_map(zoom = input$zoom)
    values$DB <- values$DB %>% run_kmeans(input$kclusters)
    values$DB$project_name <- "projname"

  })
  observeEvent(input$save_your_work,ignoreInit = T,{
    values$DB %>% save_DB()
  })
  #end-------
}
