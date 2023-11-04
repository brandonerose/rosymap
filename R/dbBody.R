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
            title = h1("Rosymap Planning your Intervention"),
            width = 12,
            # height = 600,
            plotly::plotlyOutput("main_plot",height = 500)
          )
        ),
        fluidRow(
          box(
            title = h1("Events of Interest"),
            width = 6,
            # height = 600,
            DT::DTOutput("eoi_table",height = 500)
          ),
          box(
            title = h1("Interventions"),
            width = 6,
            # height = 600,
            DT::DTOutput("int_table",height = 500)
          )
        ),
        fluidRow(
          shinydashboard::valueBoxOutput("vb9",width = 12)
        ),
        fluidRow(
          shinydashboard::valueBoxOutput("vb1",width = 3),
          shinydashboard::valueBoxOutput("vb4",width = 3),
          shinydashboard::valueBoxOutput("vb5",width = 3),
          shinydashboard::valueBoxOutput("vb8",width = 3)
        ),
        fluidRow(
          shinydashboard::valueBoxOutput("vb2",width = 3),
          shinydashboard::valueBoxOutput("vb3",width = 3),
          shinydashboard::valueBoxOutput("vb6",width = 3),
          shinydashboard::valueBoxOutput("vb7",width = 3)
        ),
        fluidRow(
          shinydashboard::valueBoxOutput("vb10",width = 6),
          shinydashboard::valueBoxOutput("vb11",width = 6)
        ),
        fluidRow(
          box(
            title = h1("Histogram of Distances Events of Interest Compared to Predicted Clusters"),
            width = 6,
            # height = 600,
            plotly::plotlyOutput("hist_pred",height = 500)
          ),
          box(
            title = h1("Histogram of Distances Events of Interest Compared to Interventions"),
            width = 6,
            # height = 600,
            plotly::plotlyOutput("hist_int",height = 500)
          )
        ),
        fluidRow(
          box(
            title = h1("Clusters (Predicted)"),
            width = 12,
            # height = 600,
            DT::DTOutput("cluster_table",height = 500)
          )
        )
      ),
      tabItem(
        "upload",
        h1("Upload your files here!"),
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
            # textInput("eoi_group_name","Group Name",placeholder = "what should events be called?"),
            uiOutput("eoi_ui_lat"),
            uiOutput("eoi_ui_lon")
            # uiOutput("eoi_ui_group"),
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
            # textInput("int_group_name","Group Name",placeholder = "what should events be called?"),
            uiOutput("int_ui_lat"),
            uiOutput("int_ui_lon")
            # uiOutput("int_ui_group"),
          ),
          box(
            title = h1("Click the Button Below to Save"),
            width = 12,
            actionButton("upload_save","Save your above files!"),
            p("After you click save you can return to the home tab.")
          )
        )
       ),
      tabItem(
        "backend",
        fluidRow(
          box(
            title = h1("Here is what the DB object looks like on the R 'backend'"),
            width = 12,
            listviewer::jsoneditOutput("listviewdb")
          )
        )
      )
    )
  )
}
