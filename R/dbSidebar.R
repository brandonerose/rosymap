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
      value = 6,
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
      choices=c("terrain", "terrain-background", "terrain-labels", "terrain-lines",
                "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                "toner-labels", "toner-lines", "toner-lite", "watercolor"),
      selected = "toner-hybrid"
    ),
    actionButton("random_colors", "Random Colors/Shapes"),
    # actionButton("random_shapes", "Random Colors"),
    # actionButton("default_colors", "Default Colors"),
    actionButton("save_your_work", "Save Your Work!"),
    # textOutput("testtext"),
    TCD_SBF()
  )
}


