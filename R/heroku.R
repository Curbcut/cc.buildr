#' Deploy an app in Heroku
#'
#' The function initiates the heroku login process and authenticates the
#' container. After successful authentication, it pushes the web application
#' to the specified Heroku account and finally, releases the web application.
#'
#' @param app_name <`character`> Must correspond to an existing app on the
#' Heroku account that the user will login.
#'
#' @return Opens a terminal and disconnect from the current R session.
#' @export
heroku_deploy <- function(app_name) {
  if (Sys.info()["sysname"] != "Windows") {
    stop("As of now, this function is only adapted for Windows.")
  }

  cancel <- readline(
    prompt =
      paste0(
        "Pursuing will open a terminal and terminate ",
        "this R session. Write `ok` to proceed: "
      )
  )

  if (cancel != "ok") {
    return(cat("Aborted succesfully."))
  }

  cmds <- c(
    paste0("cd ", getwd()),
    "heroku login",
    "heroku container:login",
    paste0("heroku container:push web -a ", app_name),
    paste0("heroku container:release web -a ", app_name)
  )

  tmp <- tempfile(fileext = "ps1")

  paste0(cmds, collapse = "\n") |>
    writeLines(tmp)

  shell(paste0(
    "start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ",
    tmp
  ))

  quit(save = "no")
}

#' Restart dyno for the provided app
#'
#' @param app_name <`character`> Name of the application.
#' @param dyno <`character`> Name of the dyno.
#'
#' @return Opens a terminal asking to login, then restarts the dyno.
#' @export
heroku_restart_dyno <- function(app_name, dyno) {
  cmds <- c(
    "heroku login",
    paste0("heroku restart ", dyno, " -a ", app_name)
  )

  tmp <- tempfile(fileext = "ps1")

  paste0(cmds, collapse = "\n") |>
    writeLines(tmp)

  shell(paste0(
    "start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ",
    tmp
  ))
}
