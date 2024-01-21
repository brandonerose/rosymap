#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_server <- function(input, output, session) {
  #UI init -------
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
  # output$eoi_ui_group <- renderUI({
  #   selectizeInput(
  #     inputId = "eoi_ui_group_",
  #     label = "Group Column",
  #     choices = NULL
  #   )
  # })
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
  output$int_ui_size <- renderUI({
    selectizeInput(
      inputId = "int_ui_size_",
      label = "Size Column (optional)",
      choices = NULL
    )
  })
  # output$int_ui_group <- renderUI({
  #   selectizeInput(
  #     inputId = "int_ui_group_",
  #     label = "Group Column",
  #     choices = NULL
  #   )
  # })
  #reactivevalues sidebar-------
  values<-reactiveValues()
  values$DB<- sample_DB
  values$upload_eoi<- NULL
  values$upload_int<- NULL
  values$plot_params <- reactive({
    list(
      kclusters = input$kclusters,
      maptype = input$maptype,
      zoom = input$zoom,
      event_color = input$event_color,
      intervention_color = input$intervention_color,
      center_color = input$center_color,
      color_events_scale = input$color_events_scale,
      output_min = input$output_min,
      output_max = input$output_max,
      opacity = input$opacity
    )
  })


  observeEvent(values$plot_params(),{
    values$DB <- values$DB %>% run_kmeans(k = values$plot_params()$kclusters)
    values$DB <- values$DB %>% k_means_plot(
      maptype = values$plot_params()$maptype,
      zoom = values$plot_params()$zoom,
      event_color = values$plot_params()$event_color,
      intervention_color = values$plot_params()$intervention_color,
      center_color = values$plot_params()$center_color,
      color_events_scale = values$plot_params()$color_events_scale,
      output_min = values$plot_params()$output_min,
      output_max = values$plot_params()$output_max,
      opacity = values$plot_params()$opacity
    )
    print("observed plot")
  })

  #obserevent files -------
  observeEvent(input$eoi_upload_file,ignoreInit = T,{
    req(input$eoi_upload_file)
    values$upload_eoi <-rio::import(input$eoi_upload_file$datapath)
    colnames(values$upload_eoi) <- colnames(values$upload_eoi) %>% tolower()
    cols <- values$upload_eoi%>% colnames() %>% as.character()
    updateSelectizeInput(
      session,
      inputId ="eoi_ui_lat_",
      choices = cols,
      selected =  ifelse("latitude"%in%cols,"latitude",ifelse("Latitude"%in%cols,"Latitude",ifelse("lat"%in%cols,"lat",ifelse("Lat"%in%cols,"Lat",NULL))))
    )
    updateSelectizeInput(
      session,
      inputId ="eoi_ui_lon_",
      choices = cols,
      selected =  ifelse("longitude"%in%cols,"longitude",ifelse("Longitude"%in%cols,"Longitude",ifelse("lon"%in%cols,"lon",ifelse("Lon"%in%cols,"Lon",NULL))))
    )
    # updateSelectizeInput(
    #   session,
    #   inputId ="eoi_ui_group_",
    #   choices = cols,
    #   selected =  ifelse("group"%in%cols,"group",ifelse("Group"%in%cols,"Group",NULL))
    # )
  })
  observeEvent(input$int_upload_file,ignoreInit = T,{
    # values$DB <-
    req(input$int_upload_file)
    # print(input$int_upload_file$datapath)
    values$upload_int <-rio::import(input$int_upload_file$datapath)
    # values$upload_int <-get_dir() %>% file.path("input","Stop the Bleed Trainings_5.12.23.xlsx") %>%  rio::import()
    colnames(values$upload_int) <- colnames(values$upload_int) %>% tolower()
    cols <- values$upload_int%>% colnames() %>% as.character()
    guess_lat <- NULL
    if("latitude"%in%cols){
      guess_lat <- "latitude"
    }else{
      if("lat"%in%cols){
        guess_lat <- "lat"
      }
    }
    guess_lon <- NULL
    if("longitude"%in%cols){
      guess_lon <- "longitude"
    }else{
      if("lon"%in%cols){
        guess_lon <- "lon"
      }
    }
    guess_size <- NULL
    if("size"%in%cols){
      guess_size <- "size"
    }
    updateSelectizeInput(
      session,
      inputId ="int_ui_lat_",
      choices = cols,
      selected =  guess_lat
    )
    updateSelectizeInput(
      session,
      inputId ="int_ui_lon_",
      choices = cols,
      selected =  guess_lon
    )
    updateSelectizeInput(
      session,
      inputId ="int_ui_size_",
      choices = cols,
      selected =  guess_size
    )
  })
  #observevent save -----
  observeEvent(input$upload_save,ignoreInit = T,{
    values$DB <-load_DB(blank = T)

    coordinates <- values$upload_eoi
    if(!is.null(input$eoi_ui_lat_)){
      lat <- coordinates[[input$eoi_ui_lat_]]
      coordinates[[input$eoi_ui_lat_]] <- NULL
      coordinates$latitude <- lat
    }
    if(!is.null(input$eoi_ui_lon_)){
      lon <- coordinates[[input$eoi_ui_lon_]]
      coordinates[[input$eoi_ui_lon_]] <- NULL
      coordinates$longitude <- lon
    }
    coordinates$group <- "Event"
    if(!is.null(values$upload_int)){
      coordinates2 <- values$upload_int
      if(!is.null(input$int_ui_lat_)){
        lat <- coordinates2[[input$int_ui_lat_]]
        coordinates2[[input$int_ui_lat_]] <- NULL
        coordinates2$latitude <- lat
      }
      if(!is.null(input$int_ui_lon_)){
        lon <- coordinates2[[input$int_ui_lon_]]
        coordinates2[[input$int_ui_lon_]] <- NULL
        coordinates2$longitude <- lon
      }
      if(!is.null(input$int_ui_size_)){
        size <- coordinates2[[input$int_ui_size_]]
        coordinates2[[input$int_ui_size_]] <- NULL
        coordinates2$size <- size
      }
      coordinates2$group <- "Intervention"
      coordinates <- coordinates %>% dplyr::bind_rows(coordinates2)
    }

    values$DB$data$coordinates <- coordinates
    values$DB <- values$DB %>% run_kmeans(k = values$plot_params()$kclusters)
    values$DB <-    values$DB %>% k_means_plot(
      maptype = values$plot_params()$maptype,
      zoom = values$plot_params()$zoom,
      event_color = values$plot_params()$event_color,
      intervention_color = values$plot_params()$intervention_color,
      center_color = values$plot_params()$center_color,
      color_events_scale = values$plot_params()$color_events_scale,
      output_min = values$plot_params()$output_min,
      output_max = values$plot_params()$output_max,
      opacity = values$plot_params()$opacity
    )
    values$DB$project_name <-"test" # change to UI later or remove
  })

  #renderDT ----------
  output$eoi_upload_table <- DT::renderDT({values$upload_eoi %>% make_table()})
  output$int_upload_table <- DT::renderDT({values$upload_int %>% make_table()})

  output$cluster_table <- DT::renderDT({values$DB$data$cluster_df %>% make_table()})
  output$eoi_table <- DT::renderDT({
    OUT <- values$DB$data$coordinates_plot[which(values$DB$data$coordinates_plot$group=="Event"),]
    OUT$label <- NULL
    OUT <- OUT[ , colSums(is.na(OUT))==0]
    # OUT <- OUT %>% dplyr::select(group,id, latitude,longitude,eoi_to_int_min_distance_km)
    make_table(OUT)
  })
  output$int_table <- DT::renderDT({
    if("Intervention"%in%values$DB$data$coordinates_plot$group){
      OUT <- values$DB$data$coordinates_plot[which(values$DB$data$coordinates_plot$group=="Intervention"),]
      OUT$label <- NULL
      OUT <- OUT[ , colSums(is.na(OUT))==0]
      # OUT <- OUT %>% dplyr::select(group,id, latitude,longitude,eoi_to_int_min_distance_km)
      make_table(OUT)
    }else{
      NULL
    }
  })

  #actionbuttons ---------

  observeEvent(input$use_sample_data,{
    values$DB <- sample_DB
    values$DB <- values$DB %>% run_kmeans(k = values$plot_params()$kclusters)
    values$DB <- values$DB %>% k_means_plot(
      maptype = values$plot_params()$maptype,
      zoom = values$plot_params()$zoom,
      event_color = values$plot_params()$event_color,
      intervention_color = values$plot_params()$intervention_color,
      center_color = values$plot_params()$center_color,
      color_events_scale = values$plot_params()$color_events_scale,
      output_min = values$plot_params()$output_min,
      output_max = values$plot_params()$output_max,
      opacity = values$plot_params()$opacity
    )
  })
  observeEvent(input$use_directory_data,ignoreInit = T,{
    values$DB <- load_DB()
    values$DB <- values$DB %>% run_kmeans(k = values$plot_params()$kclusters)
    values$DB <- values$DB %>% k_means_plot(
      maptype = values$plot_params()$maptype,
      zoom = values$plot_params()$zoom,
      event_color = values$plot_params()$event_color,
      intervention_color = values$plot_params()$intervention_color,
      center_color = values$plot_params()$center_color,
      color_events_scale = values$plot_params()$color_events_scale,
      output_min = values$plot_params()$output_min,
      output_max = values$plot_params()$output_max,
      opacity = values$plot_params()$opacity
    )
  })
  observeEvent( input$rerun_kmeans,ignoreInit = T,{
    values$DB <- values$DB %>% run_kmeans(k = values$plot_params()$kclusters)
    values$DB <- values$DB %>% k_means_plot(
      maptype = values$plot_params()$maptype,
      zoom = values$plot_params()$zoom,
      event_color = values$plot_params()$event_color,
      intervention_color = values$plot_params()$intervention_color,
      center_color = values$plot_params()$center_color,
      color_events_scale = values$plot_params()$color_events_scale,
      output_min = values$plot_params()$output_min,
      output_max = values$plot_params()$output_max,
      opacity = values$plot_params()$opacity
    )
  })
  observeEvent(input$run_all_kmeans,ignoreInit = T,{
    values$DB <- values$DB %>% DB_run_all_kmeans() %>% DB_plot_all_kmeans()
    values$DB <- values$DB %>% run_kmeans(k = values$plot_params()$kclusters)
    values$DB <- values$DB %>% k_means_plot(
      maptype = values$plot_params()$maptype,
      zoom = values$plot_params()$zoom,
      event_color = values$plot_params()$event_color,
      intervention_color = values$plot_params()$intervention_color,
      center_color = values$plot_params()$center_color,
      color_events_scale = values$plot_params()$color_events_scale,
      output_min = values$plot_params()$output_min,
      output_max = values$plot_params()$output_max,
      opacity = values$plot_params()$opacity
    )
    # print("ran it")
  })
  observeEvent(input$save_your_work,ignoreInit = T,{
    values$DB %>% save_DB()
  })
  #plots -------
  output$main_plot <- plotly::renderPlotly({
    values$DB$other$main_plotly
  })
  output$hist <- plotly::renderPlotly({
    values$DB$other$hist
  })
  output$all_kmeans_plot <- renderPlot({
    values$DB$other$all_kmeans_plot
  })
  #testext=-------
  # output$testtext <- renderText({
  #   values$upload_int
  # })
  #listviewer -------
  output$listviewdb <- listviewer::renderJsonedit({
    listviewer::jsonedit(values$DB)
  })
  #valueboxes-------
  output$vb1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_pred_min_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Predicted Cluster Minimum Distance",
      # icon = ,
      color = "green"
    )
  })
  output$vb2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_pred_median_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Predicted Cluster Median Distance",
      # icon = ,
      color = "green"
    )
  })
  output$vb3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_pred_mean_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Predicted Cluster Mean Distance",
      # icon = ,
      color = "green"
    )
  })
  output$vb4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_pred_max_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Predicted Cluster Maximum Distance",
      # icon = ,
      color = "green"
    )
  })
  output$vb5 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_int_min_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Intervention Minimum Distance",
      # icon = ,
      color = "light-blue"
    )
  })
  output$vb6 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_int_median_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Intervention Median Distance",
      # icon = ,
      color = "light-blue"
    )
  })
  output$vb7 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_int_mean_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Intervention Mean Distance",
      # icon = ,
      color = "light-blue"
    )
  })
  output$vb8 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = values$DB$data$eoi_to_int_max_min_distance_km %>% km_to_mi() %>% round(1) %>% paste(" Mi"),
      subtitle = "Events of Interest to Intervention Maximum Distance",
      # icon = ,
      color = "light-blue"
    )
  })
  output$vb9 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = (values$DB$data$coordinates_plot$group=="Event") %>% which() %>% length(),
      subtitle = "Events of Interest",
      # icon = ,
      color = "orange"
    )
  })
  output$vb10 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = (values$DB$data$coordinates_plot$group=="K-Mean Center") %>% which() %>% length(),
      subtitle = "Predicted Clusters",
      # icon = ,
      color = "green"
    )
  })
  output$vb11 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = (values$DB$data$coordinates_plot$group=="Intervention") %>% which() %>% length(),
      subtitle = "Interventions",
      # icon = ,
      color = "light-blue"
    )
  })
  #end-------
}
