dashboardPage(
  title = 'DB Table Writer',
  dashboardHeader(
    title= span(icon("map-signs"), 'DB Table Writer')
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Manage Data",  icon = icon('save'), tabName="menuItem")
    )
  ),
  dashboardBody(
    source('www/R/tags.R', local=T)[[1]],
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(tabName='menuItem',
        tagAppendAttributes(
        box(
          title = "Manage Data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          fluidRow(
            column(
              width = 2,
              uiOutput('selected_table')
            ),
            column(
              width = 2,
              textInput('new_table_name', 'New Table Name', value = "")
            ),
            column(
              width = 2,
              downloadButton('download_to_csv', 'Download to CSV', icon('download'), style = "width:100%; margin-top: 25px;")
            ),
            column(
              width = 2,
              fileInput('csv_file', 'Upload a CSV', accept = c('.csv'))     
            ),
            column(
              width = 2,
              actionButton('refresh_from_db', 'Refresh from DB', icon('refresh'), style = "width:100%; margin-top: 25px;")
            ),
            column(
              width = 2,
              actionButton('write_to_db', 'Write to DB', icon('pen'), style = "width:100%; margin-top: 25px;")
            )
          ),
          fluidRow(
            column(
              width = 12,
              DT::dataTableOutput('table')
            )
          ),
          fluidRow(
            column(
              width = 2,
              actionButton("delete_table", "Delete Table", icon("trash"), style = "width:100%; margin-top: 25px;")
            )
          )
        ), id = "tw_box")
    )
  )
)
)
