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
            # height = 600,
            plotly::plotlyOutput("main_plot",height = 500)
          ),
          box(
            title = h1("Comparison Map"),
            width = 6,
            # height = 600,
            plotly::plotlyOutput("compare_plot",height = 500)
          )
        )
      ),
      tabItem(
        "upload",
        h1("Method 1 (two different files)"),
        fluidRow(
          box(
            title = h1("Upload Events of Interest"),
            width = 6,
            # height = 600,
            fileInput(
              "eoi_upload_file",
              "Choose Data File",
              multiple = F,
              accept = c(".csv",".xlsx")
            ),
            DT::DTOutput("eoi_upload_table"),
            textInput("eoi_group_name","Group Name",placeholder = "what should events be called?"),
            uiOutput("eoi_ui_lat"),
            uiOutput("eoi_ui_lon"),
            uiOutput("eoi_ui_group"),
            actionButton("eoi_upload_save","Save")
          ),
          box(
            title = h1("Upload Intervention (optional)"),
            width = 6,
            # height = 600,
            fileInput(
              "int_upload_file",
              "Choose Data File",
              multiple = F,
              accept = c(".csv",".xlsx")
            ),
            DT::DTOutput("int_upload_table"),
            textInput("int_group_name","Group Name",placeholder = "what should events be called?"),
            uiOutput("int_ui_lat"),
            uiOutput("int_ui_lon"),
            uiOutput("int_ui_group"),
            actionButton("int_upload_save","Save")
          )
        ),
        h1("Method 2 (one file)"),
        fluidRow(
          box(
            title = h1("Upload Events of Interest"),
            width = 12,
            # height = 600,
            fileInput(
              "com_upload_file",
              "Choose Data File",
              multiple = F,
              accept = c(".csv",".xlsx")
            ),
            DT::DTOutput("com_upload_table"),
            uiOutput("com_ui_lat"),
            uiOutput("com_ui_lon"),
            uiOutput("com_ui_group"),
            actionButton("com_upload_save","Save")
          )
        )
        # dt_output('edit rows but disable certain columns (editable = list(target = "row", disable = list(columns = c(2, 4, 5))))', 'x10')
      )
    )
  )
}
