server <- function(input, output, session) {

    # set the maximum request size to 500MB
    options(shiny.maxRequestSize = 500*1024^2)

    # on start, disable the new table name input
    # (will enable automaticlly based on input$selected_table)
    shinyjs::disable("new_table_name")

    # add the spinner to the box to preven interaction
    add_spinner <- function() {
        shinyjs::runjs("aaFuncs.addSpinner($('#tw_box > .box'))")
    }

    # remove the spinner from the box to allow interaction
    remove_spinner <- function() {
        shinyjs::runjs("aaFuncs.removeSpinner($('#tw_box > .box'))")
    }

    # a reactive vector of the avialable table names in the DB
    table_list_r <- reactiveVal(get_master_list(T))

    # a reactive vector of choices. The table list and an option to add a new table
    table_choices <- reactive({
        c(table_list_r(), "Add a New Table")
    })

    # a reactive value of the default table name to display
    # on load set to the first table in the list
    default_table <- reactiveVal(isolate(table_choices()[[1]]))

    # when the available tables change, or the default table changes
    # update the table select input to reflect the changes
    output$selected_table <- renderUI({
        selectInput("table_list", "Select Table", choices = table_choices(), selected = default_table())
    })

    # a reactive value of the active table name
    # a table has not active_name until its written to the DB
    active_name <- reactiveVal()
    
    # a reactive value of the active table data
    # this can be data pulled from the DB, or the csv
    active_data  <- reactiveVal()

    ########################################################################################
    # "Select Table" dropdown input ########################################################
    ########################################################################################
    
    # when a new table is selected from the list,
    # set the name and active data
    observeEvent(input$table_list, {
        # add the spinner to the box to prevent interaction
        add_spinner() 

        # get the selected table name
        selected_table <- input$table_list
        
        # reset the file input to its default
        reset('csv_file')

        if(selected_table == "Add a New Table") { 
            # if the user wants to add a new table
            # allow the user to enter a new table name
            shinyjs::enable("new_table_name") 
            # block deleteing (table doesnt exist yet)
            shinyjs::disable("delete_table") 
            # block refreshing (table doesnt exist yet)
            shinyjs::disable("refresh_from_db") 
            # no active table name for a new table
            active_name("") 
            # empty df as placeholder
            active_data(data.frame(KEY = numeric(), VALUE = numeric())) 
        } else {
            # if the user selects an existing table        
            # disable the new table name input. names cannont be changed
            shinyjs::disable("new_table_name")           
            # enable the delete and refresh buttons
            shinyjs::enable("delete_table")
            shinyjs::enable("refresh_from_db")
            # set the active table name to the selected table
            active_name(selected_table)
            # set the active data to the data from the selected table
            active_data(read_table(selected_table))
        }

        # remove the spinner to the box to allow interaction
        remove_spinner()
    })


    ########################################################################################
    # "active_name" observers ##############################################################
    ########################################################################################

    # when the active_name changes, set the name text input to the name
    # (makes the text input consistent with the active table name)
    observeEvent(active_name(), {
       updateTextInput(session, "new_table_name", value = active_name())
    })

    ########################################################################################
    # "active_data" observers ##############################################################
    ########################################################################################

    # when the active data changes (csv or refresh), change the 
    # available features
    observeEvent(active_data(), {
        # the number of rows int he active data
        row_count <- nrow(active_data()) 
        if(row_count > 0) { # if there is data
            # enable the dowloading and writing of the data
            shinyjs::enable("download_to_csv")
            shinyjs::enable("write_to_db")
        } else { # if there is no data
            # disable the downloading and writing of the data
            shinyjs::disable("download_to_csv")
            shinyjs::disable("write_to_db")
        }
    })

    # the datatable to display the active data
    table_dt <-  eventReactive(active_data(), {     
        DT::datatable(
            active_data(),
            rownames = FALSE,
            extensions = 'Responsive',
            options = list(
                language = list(
                    emptyTable = "Upload a CSV to get started"
                )
            )
        )
    })

    # render the active_data datatable
    output$table <- DT::renderDataTable(table_dt())

    ########################################################################################
    # "Download to CSV" file input #########################################################
    ########################################################################################
    # download the current working data to a csv file
    output$download_to_csv <- downloadHandler(
      filename = function() {
        file_prefix <- gsub(" ", "_", active_name())
        paste(file_prefix, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(active_data(), file, row.names = FALSE)
      }
    )

    ########################################################################################
    # "Upload a CSV" file input ############################################################
    ########################################################################################
    # when a csv file is uploaded
    observeEvent(input$csv_file, {
      # add the spinner to the box to prevent interaction  
      add_spinner()
      # read the csv file
      new_data <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
      # set the active data to the new data
      active_data(new_data)
      # remove the spinner to the box to allow interaction
      remove_spinner()
      
    })

    ########################################################################################
    # "Refresh from DB" button input #######################################################
    ########################################################################################
    # when the refresh button is clicked
    observeEvent(input$refresh_from_db, {
        # add the spinner to the box to prevent interaction
        add_spinner()
        # use the active name to query the DB for the table
        active_data(read_table(active_name()))
        # reset the file input to its default
        reset('csv_file')
        # remove the spinner to the box to allow interaction
        remove_spinner()
    })

    ########################################################################################
    # "Write to DB" button input ###########################################################
    ######################################################################################## 
    # when the write to db button is clicked
    observeEvent(input$write_to_db, {
        # add the spinner to the box to prevent interaction
        add_spinner()     
        # get the new tables compliatn name
        current_table <- namer(input$new_table_name) 
        if(input$table_list == "Add a New Table") {
            # if 'write' was clicked while adding a new table
            # add the table to the master list and write the data
            tw_add(current_table, active_data())
            # refresh the table list (get the new table name in the list)
            table_list_r(get_master_list(T))
        } else {
            # if 'write' was clicked while updating an existing table
            # overwrite the table with the active data
            tw_overwrite(active_name(), active_data())
        }  
        # set the default selected table to the new table
        default_table(current_table)       
    })

    ########################################################################################
    # "Delete Table" button input ##########################################################
    ######################################################################################## 
    # when the delete button is clicked
    observeEvent(input$delete_table, {
        # add the spinner to the box to prevent interaction
        add_spinner()
        # remove the table from the master list and drop it
        tw_remove(active_name())  
        # refresh the table list (get the new table name in the list)
        table_list_r(get_master_list(T))
        # remove the spinner to the box to allow interaction
        remove_spinner()
    })

}
