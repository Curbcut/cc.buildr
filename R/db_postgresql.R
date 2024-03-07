#' Connect to the AWS PostgreSQL database
#'
#' Connect to the AWS PostgreSQL database using variables saved in the .Renviron
#'
#' @return returns an S4 object that inherits from \code{\link[DBI]{DBIConnection-class}}.
#' This object is used to communicate with the database engine.
#' @export
db_connect_prod <- function() {
  if (Sys.getenv("CURBCUT_DB_USER") == "") {
    stop(paste0("You do not have a Curbcut database user access."))
  }

  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    user = Sys.getenv("CURBCUT_DB_USER"),
    password = Sys.getenv("CURBCUT_DB_PASSWORD"),
    host = "cc-prod-instance-1.cplnwzthenux.us-east-1.rds.amazonaws.com",
    port = 5432,
    dbname = "postgres"
  )
}

#' Disconnect from the AWS PostgreSQL database
#'
#' @param conn A \code{\link[DBI]{DBIConnection-class}} object, as returned by
#' \code{\link[cc.data]{db_connect}}.
#' @return A message out of \code{\link[DBI]{dbDisconnect}}
#' @export
db_disconnect_prod <- function(conn) {
  DBI::dbDisconnect(conn)
}

#' Try to execute a database function and disconnect on error
#'
#' This function attempts to execute a given database-related function
#' (`fun`) using an existing database connection (`conn`). If an error
#' occurs during the execution of `fun`, the function ensures that the
#' database connection is properly disconnected by calling
#' `db_disconnect_prod`. This mechanism is useful for maintaining clean
#' database connections and ensuring they are closed properly in the event
#' of an error.
#'
#' @param conn A database connection object. This should be an object
#' representing an open connection to a database, as created by
#' `DBI::dbConnect()`.
#' @param fun A function that performs an operation on the database using
#' the provided `conn`. This function should use the connection to
#' execute a query, fetch results, write data, etc.
#'
#' @return The result of executing `fun`, if successful. If an error
#' occurs during the execution of `fun`, the database connection is
#' closed, and the error is re-thrown.
#' @export
db_try_disconnect <- function(conn, fun) {
  tryCatch(fun,
           error = function(e) {
             db_disconnect_prod(conn)
             stop(e)
           })
}

#' Write dataframe to PostgreSQL with Primary Key
#'
#' This function writes a given data frame to a specified table in a PostgreSQL
#' database. If the table exists, it is overwritten. The function also sets a
#' specified column as the primary key of the table. The operation is wrapped in
#' `db_try_disconnect` to ensure the database connection is properly managed.
#'
#' @param df <`data.frame`> A data frame containing the data to be written to the
#' database table.
#' @param table_name <`character`> A string specifying the name of the database table.
#' @param schema <`character`> A string specifying the database schema, which is
#' the local prefix which is also the same prefix of tilesets. E.g. for Montreal, `mtl`.
#' @param scale <`character`> The scale at which is the data. It is coupled
#' with `table_name` for the name of the table in the database. If `NULL` (default),
#' only `table_name` is used. It stays NULL only for the general scale table, not
#' for table containing data.
#' @param primary_key <`character`> A string specifying the name of the column
#' to be used as the primary key. Defaults to "ID".
#' @param index <`character`> or <`character vector`> specifying the name(s) of
#' the column(s) to be indexed. If `NULL` (default), no additional indexes are created.
#' Multiple columns can be provided for creating a composite index. Defaults to `NULL`.
#'
#' @return Invisibly returns `NULL`. The primary effect is the side effect of
#' writing data to the database and setting a primary key.
#'
#' @export
db_write_prod <- function(df, table_name, schema, scale = NULL, primary_key = "ID",
                          index = NULL) {

  # Name the table inside the postgresql
  tn <- if (is.null(scale)) table_name else paste(scale, table_name, sep = "_")

  # Arrange the columns for efficiency. Place the fixed size data type first
  # (integer, logical) ad the variable ones last (character). We tested and have
  # seen that tables are 6% smaller when this is true. Idea taken from:
  # https://www.youtube.com/watch?v=m8ogrogKjXo
  # Define the desired order of data types
  desired_order <- c("logical", "integer", "numeric", "character", "list")
  # Create a vector of the data types in the order they appear in the DataFrame
  data_types <- sapply(df, class)
  # Create a function to find the index of each data type in the desired order
  order_index <- function(type) which(type == desired_order)
  # Apply the function to get the order index for each column's data type
  order_indices <- sapply(data_types, order_index)
  # Order the columns based on the indices of their data types in the desired order
  ordered_df <- df[, order(order_indices)]

  # Switch list columns to JSON (for a JSONB PostgreSQL column)
  list_cols <- names(which(order_indices == which(desired_order == "list")))
  for (c in list_cols) {
    # If it's already json, pass to the next
    if (all(sapply(ordered_df[[c]], class) == "json")) {
      ordered_df[[c]] <- as.character(ordered_df[[c]])
      next
    }
    ordered_df[[c]] <- sapply(ordered_df[[c]], jsonlite::toJSON, auto_unbox = TRUE)
  }

  # Connect to the db
  conn <- db_connect_prod()

  # Write this table in the right schema
  db_try_disconnect(
    conn = conn,
    fun = DBI::dbWriteTable(conn = conn,
                            name = RPostgres::Id(schema = schema, table = tn),
                            value = ordered_df, overwrite = TRUE, row.names = FALSE)
  )

  # If a column was a list and now JSONB, alter the table
  for (c in list_cols) {
    query <- sprintf("ALTER TABLE %s.%s ALTER COLUMN %s TYPE JSONB USING %s::jsonb;",
                     schema, tn, c, c)
    DBI::dbExecute(conn, query)
  }

  # Make the (normally) ID column the primary key
  if (!is.null(primary_key)) {
    db_try_disconnect(
      conn = conn,
      fun = DBI::dbExecute(conn = conn,
                           statement = sprintf('ALTER TABLE %s.%s ADD PRIMARY KEY ("%s");',
                                               schema, tn, primary_key))
    )
  }

  # Add indexes if specified
  if (!is.null(index)) {
    # If index is a single column, make it a vector for consistent processing
    index <- if (!is.vector(index)) { c(index) } else { index }

    # Create an index for each specified column
    for (idx in index) {
      # Define index name based on table name and column name
      index_name <- paste0("idx_", tn, "_", idx)
      # Generate SQL command to create index
      create_index_sql <- sprintf('CREATE INDEX %s ON %s.%s ("%s");',
                                  index_name, schema, tn, idx)
      # Execute SQL command to create index
      db_try_disconnect(
        conn = conn,
        fun = DBI::dbExecute(conn = conn, statement = create_index_sql)
      )
    }
  }

  # Disconnect
  db_disconnect_prod(conn)

  return(invisible())

}

#' Create or Verify existence of a PostgreSQL schema
#'
#' Connects to the PostgreSQL database, checks if the specified schema exists,
#' and creates it if it does not. If the schema already exists, it does nothing except
#' inform the user. It uses a database connection, defined by `db_connect_prod()`,
#' and ensures safe execution and disconnection using `db_try_disconnect()`.
#' Normally, as schema is used for an instance of Curbcut. mtl, to, van, etc.
#'
#' @param schema_name <`character`> The name of the schema to check or create.
#' Expected to be a character string representing the valid schema name in PostgreSQL.
#' @param allow_read_webapp <`logical`> Wether if the read only webapp user in the
#' database should have read access to this schema. Defaults to TRUE.
#'
#' @return A character string message indicating whether the schema was created or
#' already existed.
#' @export
db_create_schema_prod <- function(schema_name, allow_read_webapp = TRUE) {

  # Connect to the db
  conn <- db_connect_prod()

  # Check if the schema exists
  query <- sprintf(paste0("SELECT schema_name FROM information_schema.schemata",
                          " WHERE schema_name = '%s';"), schema_name)
  exists <- DBI::dbGetQuery(conn, checkSchemaExistsQuery)

  # If the schema does not exist, create it
  if (nrow(exists) == 0) {
    query <- sprintf("CREATE SCHEMA \"%s\";", schema_name)
    db_try_disconnect(
      conn = conn,
      fun = DBI::dbExecute(conn, query)
    )
    out <- sprintf("Schema '%s' has been created.\n", schema_name)

    # Allow usage access to the readonly_webapp_user the schema
    if (allow_read_webapp) {
      DBI::dbExecute(conn, sprintf("GRANT USAGE ON SCHEMA %s TO readonly_webapp_user",
                                   schema_name))
      DBI::dbExecute(conn, sprintf("GRANT SELECT ON ALL TABLES IN SCHEMA %s TO readonly_webapp_user",
                                   schema_name))
      DBI::dbExecute(conn, sprintf("ALTER DEFAULT PRIVILEGES IN SCHEMA %s GRANT SELECT ON TABLES TO readonly_webapp_user",
                                   schema_name))
    }

  } else {
    out <- sprintf("Schema '%s' already exists.\n", schema_name)
  }


  # Disconnect
  db_disconnect_prod(conn)

  # Return text
  return(out)
}

#' Get Pretty-Printed Table Size from PostgreSQL Database
#'
#' This function retrieves the disk space used by a specified table within a
#' PostgreSQL database and returns it in a human-readable format. It uses
#' PostgreSQL's `pg_size_pretty` and `pg_relation_size` functions to obtain and
#' format the table size.
#'
#' @param conn <`DBIConnection`> Represents an active connection to a PostgreSQL database.
#' @param table_name <`character`> The name of the table for which the size
#' is to be retrieved. The table name should be fully qualified if it is not in
#' the default schema, i.e., "schema.table_name".
#'
#' @return A data frame with a single column and a single row containing the
#' pretty-printed size of the table. The column name is `pg_size_pretty`.#'
#' @export
db_table_size <- function(table_name) {

  # Connect to the db
  conn <- db_connect_prod()

  #
  out <- db_try_disconnect(
    conn = conn,
    fun = DBI::dbGetQuery(conn,
                          sprintf("SELECT pg_size_pretty(pg_relation_size('%s'))",
                                  table_name))
  )

  # Disconnect
  db_disconnect_prod(conn)

  # Return text
  return(out)
}

#' List Tables in a PostgreSQL Schema
#'
#' Retrieves a list of all tables within a specified schema in a PostgreSQL
#' database. The PostgreSQL database is accessed through `db_connect_prod`.
#'
#' @param schema <`character`> name of the schema whose tables you want to list.
#'
#' @return A character vector containing the names of all tables within the
#' specified schema.
#'
#' @export
db_list_tables <- function(schema) {
  conn <- db_connect_prod()
  query <- sprintf(paste0("SELECT table_name FROM information_schema.tables ",
                          "WHERE table_schema = '%s'"), schema)
  result <- DBI::dbGetQuery(conn, query)
  db_disconnect_prod(conn)
  return(result$table_name)
}

#' List scales in a PostgreSQL schema
#'
#' Retrieves a list of all tables within a specified schema in a PostgreSQL
#' database. The PostgreSQL database is accessed through `db_connect_prod`, and only
#' tables which DO NOT contain underscores are extracted (they are Curbcut `scales`).
#'
#' @param schema <`character`> name of the schema whose tables you want to list.
#'
#' @return A character vector containing the names of all tables (scales) within the
#' specified schema.
#'
#' @export
db_list_scales <- function(schema) {
  conn <- db_connect_prod()
  query <- sprintf(paste0("SELECT table_name FROM information_schema.tables ",
                          "WHERE table_schema = '%s' AND table_name NOT LIKE ",
                          "'%%\\_%%' ESCAPE '\\'"), schema)
  result <- DBI::dbGetQuery(conn, query)
  return(result$table_name)
}

#' List Tables in a PostgreSQL Schema Starting with 'grd30'
#'
#' Retrieves a list of all tables within a specified schema in a PostgreSQL
#' database that start with 'grd30'. The PostgreSQL database is accessed
#' through `db_connect_prod`.
#'
#' @param schema <`character`> name of the schema whose tables you want to list.
#' @param scale <`character`> name of the scale for which we want to list all
#' the data tables
#'
#' @return A character vector containing the names of tables within the
#' specified schema that start with 'grd30'.
#'
#' @export
db_list_tables_of_scale <- function(schema, scale) {
  conn <- db_connect_prod()
  # Modify the query to filter tables that start with 'grd30'
  query <- sprintf(paste0("SELECT table_name FROM information_schema.tables ",
                          "WHERE table_schema = '%s' ",
                          "AND table_name LIKE '%s\\_%%'"), schema, scale)
  result <- DBI::dbGetQuery(conn, query)
  db_disconnect_prod(conn)
  return(result$table_name)
}
