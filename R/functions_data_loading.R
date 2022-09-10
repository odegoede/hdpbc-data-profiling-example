## Functions for database connection and data loading


#' make_connection(): create a connection between the R session and the SQLServer database
#' 
#' Leads to a pop-up for password input and MFA approval. Returns an odbc connection.
#' Required inputs: username and database name.
make_connection <- function(username = NULL, db = NULL) {
  
  checkmate::assert_character(username)
  checkmate::assert_character(db)
  
  connection_info <- paste("Connecting user", username, "to", db)
  message(connection_info)
  
  con <- DBI::dbConnect(odbc::odbc(),
                        uid = username,
                        driver="ODBC Driver 17 for SQL Server",
                        server = "spdbshdp002.e2c7cffdef57.database.windows.net", 
                        database = db,
                        authentication = "ActiveDirectoryInteractive")
  
  return(con)
  
}



#' load_full_table(): Read in an entire table from the database into the R session.
#' 
#' Returns the table as a data.frame. Make sure the table isn't too big, otherwise it will crash your session!
#' Required inputs: the data holding name and the table name.
load_full_table <- function(data_holding = NULL, table_name = NULL) {
  
  query <- glue::glue_sql(.con = con,
                    'SELECT * FROM {`data_holding`}.{`table_name`}')

  res <- DBI::dbGetQuery(con, query)
  
  return(res)
  
}



#' load_random_subset(): Read in a semi-random subset of a table from the database into the R session.
#' 
#' Returns the table subset as a data.frame.
#' Required inputs: the data holding name and the table name; id_var is the name of the column to be used for 
#' random selection (should be an ID column, like PHN_e or PRAC_KEY_e); denom_exp is the base-10 exponent of 
#' the fraction of the table you want (e.g. if you want one-thousandth of the table, that is 1/1000 or 1/10^3,
#' so the denom_exp value is 3)
load_random_subset <- function(data_holding = NULL, table_name = NULL, id_var = NULL, denom_exp = 3) {
  
  id_selector <- sample(c(1:(10^denom_exp) - 1), 1) %>% as.character()
  if (nchar(id_selector) < denom_exp) {
    id_selector <- paste0(rep("0", (denom_exp - nchar(id_selector))), id_selector)
  }
  
  selection_info <- paste("Selecting rows with last digits of", id_var, "equal to", id_selector)
  message(selection_info)
  
  substring_adjust <- denom_exp - 1
  
  query <- glue_sql(.con = con,
                          'SELECT * FROM {`data_holding`}.{`table_name`}
                          WHERE (SUBSTRING(TRY_CAST({`id_var`} AS CHAR), 
                                           (LEN({`id_var`}) - {substring_adjust}), 
                                           LEN({`id_var`})) = {id_selector})')
  res <- DBI::dbGetQuery(con, query)
  
  return(res)
  
}
