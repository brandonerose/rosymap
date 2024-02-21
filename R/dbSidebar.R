#' @import shiny
#' @import shinydashboard
dbSidebar<-function(){
  shinydashboardPlus::dashboardSidebar(
    minified = F,
    collapsed = F,
    TCD_SBH(),
    sidebarMenu(
      id="sb1",
      menuItem(
        text="Home",
        tabName = "home",
        icon =shiny::icon("home")
      ),
      menuItem(
        text="Upload",
        tabName = "upload",
        icon =shiny::icon("upload")
      ),
      menuItem(
        text="Backend",
        tabName = "backend",
        icon =shiny::icon("gear")
      )
    ),
    actionButton("use_sample_data", "Use Sample DB"),
    actionButton("use_directory_data", "Use Directory DB"),
    # actionButton("use_environment_data", "Use Environment DB"),
    numericInput(
      inputId = "kclusters",
      label = "K clusters",
      value = 8,
      min = 1,
      max = 50,
      step = 1,
      width = NULL
    ),
    numericInput(
      inputId = "zoom",
      label = "Map Zoom",
      value = 10,
      min = 1,
      max = 20,
      step = 1,
      width = NULL
    ),
    selectizeInput(
      "maptype",
      "Map Type",
      choices=mapstyles,
      selected = mapstyles[2]
    ),
    selectizeInput(
      "event_color",
      "Event color",
      choices=rosymap_colors,
      selected = "black"
    ),
    selectizeInput(
      "intervention_color",
      "Intervention color",
      choices=rosymap_colors,
      selected = "red"
    ),
    selectizeInput(
      "center_color",
      "Center color",
      choices=rosymap_colors,
      selected = "steelblue"
    ),
    checkboxInput(
      inputId = "color_events_scale",
      label = "Use color scale",
      value = FALSE
    ),
    numericInput(
      inputId = "output_min",
      label = "Min Point Size",
      value = 9,
      min = 1,
      max = 20,
      step = 1,
      width = NULL
    ),
    numericInput(
      inputId = "output_max",
      label = "Max Point Size",
      value = 28,
      min = 1,
      max = 50,
      step = 1,
      width = NULL
    ),
    numericInput(
      inputId = "opacity",
      label = "Opacity",
      value =1,
      min = 0,
      max = 1,
      step = 0.05,
      width = NULL
    ),
    actionButton("rerun_kmeans", "Rerun Kmeans"),
    # actionButton("random_shapes", "Random Colors"),
    # actionButton("default_colors", "Default Colors"),
    actionButton("run_all_kmeans", "Run Kmeans All!"),
    actionButton("save_your_work", "Save Your Work!"),
    # textOutput("testtext"),
    TCD_SBF()
  )
}


