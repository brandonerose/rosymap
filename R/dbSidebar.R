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
      value = 9,
      min = 1,
      max = 20,
      step = 1,
      width = NULL
    ),
    selectizeInput(
      "maptype",
      "Map Type",
      choices=mapstyles,
      selected = mapstyles[1]
    ),
    actionButton("random_colors", "Random Colors/Shapes"),
    # actionButton("random_shapes", "Random Colors"),
    # actionButton("default_colors", "Default Colors"),
    actionButton("run_all_kmeans", "Run Kmeans All!"),
    actionButton("save_your_work", "Save Your Work!"),
    # textOutput("testtext"),
    TCD_SBF()
  )
}


