# packages
library("shiny")
library("ROracle")
library("shinydashboard")
library("shinyjs")
library("DT")

# DW staging connection variables
DB_DSN <- Sys.getenv("DB_DSN")
DB_USER <- Sys.getenv("DB_USER")
DW_PW <- Sys.getenv("DW_PW")

# prefix to be added to table names before writing to the database
TABLE_PREFIX <- "TW_"
# name of the master list table in the database
MASTER_TABLE <- "TW_MASTER_LIST"
# number of tables allowed to be added to the database
MAX_TABLES <- 5
# number of columns allowed in a table
COL_LIMIT <- 100
# number of rows allowed in a table
ROW_LIMIT <- 5e5

################################################################################
# UTILITY FUNCTIONS ############################################################
################################################################################

# safely connects to the database and disconnects on exit
safe_con <- function(f, ...) {
    # connect to the database
    con <- ROracle::dbConnect(
        ROracle::Oracle(),
        username = DB_USER,
        password = DB_PW,
        dbname = DB_DSN
    )
    # disconnect on exit
    on.exit(ROracle::dbDisconnect(con))
    # execute the function
    result <- f(con, ...)
    return(result)
}

# creates a valid table name from a character string
# appends "prefix to the beginning of the string
# only if it is not already present
namer <- function(name, prefix = TABLE_PREFIX){
    # number of charcters in the prefix
    num_chars <- nchar(prefix)
    # if the prefix is not present, add it
    if (substr(name, 1, num_chars) != prefix) {
        name <- paste0(prefix, name)
    }   
    # remove all non-alphanumeric characters
    # (for DB compatibility)
    name <- gsub("[^[:alnum:]_]", "_", name)
    # convert to uppercase
    name <- toupper(name)
    # limit the name to 100 characters
    name <- substr(name, 1, 100)
    return(name)
}

# returns the master list (data.frame) of tables written to the database from the app
get_master_list <- function(vec = FALSE) {
    # get the master list from the master table on the database
    current_tables <- safe_con(function(con) {
        ROracle::dbGetQuery(con, sprintf("SELECT * FROM %s", MASTER_TABLE))
    })
    # return the master list as a vector or data.frame
    if(vec) {
        return(current_tables[[1]])
    } else {
        return(current_tables)
    }
}

# checks if a dataframe is small enough to be written to the database
# less than the column and row limits
small_enough_table <- function(df, max_cols = COL_LIMIT, max_rows = ROW_LIMIT) {
    return(ncol(df) < max_cols && nrow(df) < max_rows)
}

# checks if more tables can be written to the database based on a max_table_count
# and current number o tables in the database
space_for_table <- function(max_table_count = MAX_TABLES) {
    current_table_count <- nrow(get_master_list())
    return(current_table_count < max_table_count)
}


# checks if a table name is new (not in the master list)
is_new_table_name <- function(name) {
    # make the name valid
    name <- namer(name)
    # get the current master list as a data.frame
    old_tables <- get_master_list()
    # return TRUE if the table name is not in the master list
    return(!(name %in% old_tables$TABLE_NAME))
}

# adds a table name to the master list in the db
add_to_ml <- function(name) {
    # make the name valid
    name <- namer(name)
    # error if the table name already exists in the master list
    if(!is_new_table_name(name)) stop("Table already exists")
    # add the table name to the insert query
    query <- sprintf("INSERT INTO %s (TABLE_NAME) VALUES ('%s')", MASTER_TABLE, name)
    # execute the query
    safe_con(function(con) {
        ROracle::dbSendQuery(con, query)
    })
}

# removes a table name from the master list in the db
remove_from_ml <- function(name) {
    # make the name valid
    name <- namer(name)
    # create the query to delete the table name from the master list
    query <- sprintf("DELETE FROM %s WHERE TABLE_NAME = '%s'", MASTER_TABLE, name)
    # execute the query
    safe_con(function(con) {
        ROracle::dbSendQuery(con, query)
    })
}


# writes a dataframe to the database as a table
write_table <- function(name, data) {
    # make the name valid
    name <- namer(name)
    # write the table to the database
    safe_con(function(con) {
        ROracle::dbWriteTable(con, name, data, overwrite = TRUE)
    })
    return(data)
}

# reads a table from the database
read_table <- function(name) {
    # make the name valid
    name <- namer(name)
    # create the query to select * from the table
    query <- paste0("SELECT * FROM ", name)
    # execute the query
    data <- safe_con(function(con) {
       ROracle::dbGetQuery(con, query)
    })
    return(data)
}

# drops a table from the database
drop_table <- function(name) {
    # make the name valid
    name <- namer(name)
    # create the query to drop the table
    query <- paste0("DROP TABLE ", name)
    # execute the query
    safe_con(function(con) {
        ROracle::dbSendQuery(con, query)
    })
}

################################################################################
# TW Functions #################################################################
################################################################################

# generalized function to add a table to the database from the app
tw_add <- function(table_name, table_data) {
    # make the name valid
    valid_name <- namer(table_name)
    # error if the table name already exists in the master list
    if(!is_new_table_name(valid_name)) stop("Table already exists")
    # error if there are too many tables in the database
    if(!space_for_table()) stop("No space for new table. Over Limit.") 
    # error if the data.frame is too large
    if(!small_enough_table(table_data)) stop("Table too large")
    # add the table name to the table list
    add_to_ml(valid_name)
    # write the table to the DB
    write_table(valid_name, table_data) 
    return(table_data)
}

# generalized function to remove a table from the database from the app
tw_remove <- function(table_name) {
    # make the name valid
    valid_name <- namer(table_name)
    # check that the table exists
    name_chk <- !is_new_table_name(valid_name)
    if(!name_chk) stop("Table does not exist")
    # remove the table's name from the master list
    remove_from_ml(valid_name)
    # drop the table from the database
    drop_table(valid_name)  
}

# generalized function to overwrite a table in the database from the app
tw_overwrite <- function(table_name, table_data) {
    # make the name valid
    valid_name <- namer(table_name)
    # check that the exists
    name_chk <- !is_new_table_name(valid_name)
    # check that the data is small enough
    size_chk <- small_enough_table(table_data)
    if(!name_chk) stop("Table does not exist")
    if(!size_chk) stop("Table too large") 
    # write the table to the DB
    write_table(valid_name, table_data)  
    return(table_data)
}

