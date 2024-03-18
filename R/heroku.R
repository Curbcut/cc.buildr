#' Deploy an app in Heroku
#'
#' The function initiates the heroku login process and authenticates the
#' container. After successful authentication, it pushes the web application
#' to the specified Heroku account and finally, releases the web application.
#'
#' @param app_name <`character`> Must correspond to an existing app on the
#' Heroku account that the user will login.
#' @param curbcut_branch <`character`> Is there a specific branch of Curbcut
#' that should be installed?
#' @param wd <`character`> Directory of the Curbcut-city repo.
#' @param GA <`logical`> Should google analytics be pushed with the image? Defaults
#' to FALSE. The function will also suggest to turn it to TRUE if the app name
#' is cc-montreal.
#'
#' @return Opens a terminal and disconnect from the current R session.
#' @export
heroku_deploy <- function(app_name, curbcut_branch = "HEAD", wd = getwd(),
                          GA = FALSE, task_name = "CurbcutDeploy") {
  if (Sys.info()["sysname"] != "Windows") {
    stop("As of now, this function is only adapted for Windows.")
  }

  # By default do not upload anything to a bucket
  bucket <- FALSE

  # If sending to cc-montreal, request if google analytics should be enabled
  if (app_name == "cc-montreal" & !GA) {
    ga_ask <- readline(
      prompt =
        paste0(
          "Do you want to include google analytics to your image? Y or N: "
        )
    )

    if (ga_ask %in% c("y", "Y", "yes", "YES")) {
      GA <- TRUE
    }

    bucket_ask <- readline(
      prompt =
        paste0(
          "Do you want to add the data to the bucket `curbcut.montreal.data`? Y or N: "
        )
    )

    bucket <- bucket_ask %in% c("y", "Y", "yes", "YES")
  }

  # Create the UI generation object
  modules <- qs::qread("data/modules.qs")
  tryCatch(UIs <- curbcut::modules_panel(modules = modules), error = function(e) {
    stop(paste0(
      "Calculation of modules_panel failing. Run the app in this R ",
      "session first, so all the UI functions are ready to be ",
      "sourced from the environment. The exact state of these UI functions ",
      "will be the one used on the web app (to save calculation time)."
    ))
  })
  qs::qsave(UIs, "data/modules_panel_calculated.qs")

  # Update packages in renv
  renv::install(sprintf("Curbcut/curbcut@%s", curbcut_branch))
  renv::install("Curbcut/cc.landing")
  renv::install("Curbcut/cc.map")
  renv::snapshot()

  cancel <- readline(
    prompt =
      paste0(
        "Pursuing will open a terminal (in the next 60 seconds) and ",
        "terminate this R session. Write `ok` to proceed: "
      )
  )

  if (cancel != "ok") {
    return(cat("Aborted successfully."))
  }

  # Download the SSL certificates. Place it in the root directory which
  # will be copied in the docker image
  aws.s3::save_object(region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
                      object = "global-bundle.pem",
                      bucket = "curbcut.misc",
                      file = "global-bundle.pem",
                      overwrite = TRUE)
  aws.s3::save_object(region = Sys.getenv("CURBCUT_BUCKET_DEFAULT_REGION"),
                      key = Sys.getenv("CURBCUT_BUCKET_ACCESS_ID"),
                      secret = Sys.getenv("CURBCUT_BUCKET_ACCESS_KEY"),
                      object = "Dockerfile",
                      bucket = "curbcut.misc",
                      file = "Dockerfile",
                      overwrite = TRUE)

  # Update config variables for the database access
  config_set <- c(sprintf("heroku config:set CURBCUT_PROD_DB_HOST=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_HOST"), app_name),
                  sprintf("heroku config:set CURBCUT_PROD_DB_PORT=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_PORT"), app_name),
                  sprintf("heroku config:set CURBCUT_PROD_DB_NAME=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_NAME"), app_name),
                  sprintf("heroku config:set CURBCUT_PROD_DB_USER=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_USER"), app_name),
                  sprintf("heroku config:set CURBCUT_PROD_DB_PASSWORD=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_PASSWORD"), app_name),
                  sprintf("heroku config:set AWS_ACCESS_KEY_ID=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_ACCESSKEY"), app_name),
                  sprintf("heroku config:set AWS_SECRET_ACCESS_KEY=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_SECRETACCESSKEY"), app_name),
                  sprintf("heroku config:set AWS_DEFAULT_REGION=%s -a %s",
                          Sys.getenv("CURBCUT_PROD_DB_REGION"), app_name))

  cmds <- c(
    paste0("cd ", wd),
    if (GA) "(Get-Content 'ui.R') -replace '# google_analytics', 'google_analytics' | Set-Content 'ui.R'",
    "heroku login",
    "heroku container:login",
    config_set,
    paste0("heroku container:push web -a ", app_name),
    paste0("heroku container:release web -a ", app_name),
    # Remove Docker image after release
    sprintf("docker rmi registry.heroku.com/%s/web", app_name),
    # To keep every session on their own dyno (no shared temporary files)
    paste0("heroku features:enable http-session-affinity -a ", app_name),
    "del data\\modules_panel_calculated.qs",
    "del global-bundle.pem",
    "del Dockerfile",
    if (GA) "(Get-Content 'ui.R') -replace 'google_analytics', '# google_analytics' | Set-Content 'ui.R'",
    if (bucket) "Rscript -e \"cc.data::bucket_write_folder('data', 'curbcut.montreal.data')\""
  )

  ps_file_path <- file.path(wd, "deploy_script.ps1")

  # Write commands to temporary PowerShell script
  paste0(cmds, collapse = "\n") |> writeLines(ps_file_path)

  # Execute PowerShell script
  # shell(paste0("start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ", ps_file_path))

  # Execute the script on a scheduled task. This way, we ensure the terminal opening
  # is not a child of this R session, and closing the latter won't crash the former.
  shell(
    sprintf(
      paste0(
        'schtasks /create /F /SC ONCE /ST %s /TN "%s" ',
        '/TR "powershell -ExecutionPolicy Bypass -File %s"'
      ),
      format(Sys.time() + 60, "%H:%M"),
      task_name,
      ps_file_path
    ),
    wait = TRUE
  )

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

  ps_file_path <- file.path(getwd(), "restart_dyno_script.ps1")

  paste0(cmds, collapse = "\n") |>
    writeLines(ps_file_path)

  shell(paste0("start cmd.exe @cmd /k powershell -ExecutionPolicy Bypass -File ", ps_file_path))
}
