#' Deploy an app in ECR
#'
#' The function initiates the AWS ECR process and authenticates the
#' container. After successful authentication, it pushes the web application
#' to the specified ECR repo.
#'
#' @param app_name <`character`> Must correspond to an existing ECR repository on
#' AWS account the user will login.
#' @param curbcut_branch <`character`> Is there a specific branch of Curbcut
#' that should be installed?
#' @param wd <`character`> Directory of the Curbcut-city repo.
#' @param GA <`logical`> Should google analytics be pushed with the image? Defaults
#' to FALSE. The function will also suggest to turn it to TRUE if the app name
#' is cc-montreal.
#' @param task_name <`character`> The name of the Windows task to build the Docker
#' container and upload to ECR AWS.
#' @param aws_cli_profile <`character`> The AWS IAM profile which should be used
#' to upload the container to ECR. Must have the right permissions. Must have
#' been previously set through the AWS CLI using `aws configure --profile X`.
#' @param tag <`character`> What should be the tag of the ECR image? Use `latest`
#' to make it the most up-to-date image of the repository.
#'
#' @return Opens a terminal and disconnect from the current R session.
#' @export
aws_deploy <- function(app_name, curbcut_branch = "HEAD", wd = getwd(),
                       GA = FALSE, task_name = "CurbcutDeploy",
                       aws_cli_profile = Sys.getenv("AWS_ADMIN_PROFILE"), tag) {

  if (!grepl("curbcut-", app_name)) {
    warning("Shouldn't `app_name` start with `curbcut-*` ?")
  }

  if (Sys.info()["sysname"] != "Windows") {
    stop("As of now, this function is only adapted for Windows.")
  }

  # If sending to cc-montreal, request if google analytics should be enabled
  if (app_name == "curbcut-montreal" & !GA) {
    ga_ask <- readline(
      prompt =
        paste0(
          "Do you want to include google analytics to your image? Y or N: "
        )
    )

    if (ga_ask %in% c("y", "Y", "yes", "YES")) {
      GA <- TRUE
    }
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
  suppressWarnings(docker_add_env("Dockerfile"))


  # Codes to push docker image to AWS ECR
  connect <- sprintf(paste0("aws ecr get-login-password --region %s --profile %s | docker ",
                            "login --username AWS --password-stdin %s.dkr.ecr.%s.amazo",
                            "naws.com"),
                     Sys.getenv("CURBCUT_PROD_DB_REGION"), aws_cli_profile,
                     Sys.getenv("CURBCUT_AWS_ACCOUNT_ID"),
                     Sys.getenv("CURBCUT_PROD_DB_REGION"))
  docker_id <- sprintf("$imageID = docker images --filter=reference='%s*' --quiet --all",
                       app_name)
  tag_to_aws <- sprintf("docker tag $imageID %s.dkr.ecr.%s.amazonaws.com/%s:%s",
                        Sys.getenv("CURBCUT_AWS_ACCOUNT_ID"),
                        Sys.getenv("CURBCUT_PROD_DB_REGION"),
                        app_name, tag)
  push <- sprintf("docker push %s.dkr.ecr.%s.amazonaws.com/%s:%s",
                  Sys.getenv("CURBCUT_AWS_ACCOUNT_ID"),
                  Sys.getenv("CURBCUT_PROD_DB_REGION"),
                  app_name, tag)
  restart_service <- sprintf("aws ecs update-service --cluster curbcut --service %s-service --force-new-deployment --profile %s",
                             app_name, aws_cli_profile)

  cmds <- c(
    paste0("cd ", wd),
    if (GA) "(Get-Content 'ui.R') -replace '# google_analytics', 'google_analytics' | Set-Content 'ui.R'",
    sprintf("docker build -t %s .", app_name),
    # Upload the task
    connect,
    docker_id,
    tag_to_aws,
    push,
    # Remove Docker image after release
    sprintf("docker rmi %s", app_name),
    "del data\\modules_panel_calculated.qs",
    "del global-bundle.pem",
    "del Dockerfile",
    if (GA) "(Get-Content 'ui.R') -replace 'google_analytics', '# google_analytics' | Set-Content 'ui.R'",
    restart_service
  )

  ps_file_path <- file.path(wd, "deploy_script.ps1")

  # Write commands to temporary PowerShell script
  paste0(cmds, collapse = "\n") |> writeLines(ps_file_path)

  # # Execute PowerShell script
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


#' Add Environment Variables to a Dockerfile
#'
#' This function adds specified environment variables to a Dockerfile,
#' essential for configuring the container's environment. It targets a
#' specific insertion point before the installation of R environment
#' (renv) packages. Environment variables related to both the project's
#' database and AWS credentials are set, ensuring necessary services
#' are correctly configured within the Docker container.
#'
#' @param dockerfile_path <`character`> Path to the Dockerfile. Default
#' is "Dockerfile". The function updates this Dockerfile by inserting
#' environment variable definitions.
#'
#' @return <`logical`> Returns nothing. In case of missing environment variables,
#' the function halts with an error message.
docker_add_env <- function(dockerfile_path = "Dockerfile") {
  # Define the environment variables to include
  env_vars <- c("CURBCUT_PROD_DB_HOST", "CURBCUT_PROD_DB_PORT",
                "CURBCUT_PROD_DB_NAME", "CURBCUT_PROD_DB_USER", "CURBCUT_PROD_DB_REGION",
                "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_DEFAULT_REGION")

  # Start the Dockerfile content with setting environment variables
  env_lines <- "\n# Set environment variables\nENV "
  env_lines <- paste0(env_lines, "PORT", "='", 3838, "' \\\n    ")
  env_lines <- paste0(env_lines, "RUNNING_IN_DOCKER", "='", "true", "' \\\n    ")

  # Loop through each environment variable and append it to the Dockerfile content
  for (var in env_vars) {
    value <- Sys.getenv(var)  # Default to "x" if the variable is not set
    if (value == "") stop(sprintf("Missing `%s` global environment", var))
    env_lines <- paste0(env_lines, var, "='", value, "' \\\n    ")
  }

  env_lines <- substr(env_lines, 1, nchar(env_lines) - 7) # Remove the last backslash and spaces

  # Read the existing Dockerfile content
  dockerfile_content <- readLines(dockerfile_path)

  # Find the line number where the renv installation is mentioned
  install_renv_line <- grep("RUN Rscript -e ", dockerfile_content)[1]

  # If not found, return a message
  if (length(install_renv_line) == 0) {
    cat("The specified marker for renv installation was not found in the Dockerfile.\n")
    return(FALSE)
  }

  # Insert the environment variables before the renv installation line
  dockerfile_content <- append(dockerfile_content, env_lines, install_renv_line - 2)

  # Write the updated content back to the Dockerfile
  writeLines(dockerfile_content, dockerfile_path)

  cat("Dockerfile has been updated with environment variables.\n")
}

#' Execute AWS CLI Command and Parse JSON Output
#'
#' Executes a specified AWS CLI command using a given AWS CLI profile (or a default
#' if not specified). If successful, returns the parsed JSON output. If an error
#' occurs in the system call or JSON parsing, prints the error message instead.
#'
#' @param call <`character`> The AWS CLI command to execute.
#' @param aws_cli_profile <`character`> The AWS CLI profile to use for the command.
#' Default is the value of the `AWS_ADMIN_PROFILE` environment variable.
#'
#' @return <`list`|`character`> Parsed JSON output from the executed AWS CLI command
#' or error message if an error occurred.
#' @export
aws_cli_cmd <- function(call, aws_cli_profile = Sys.getenv("AWS_ADMIN_PROFILE")) {
  # Format the call string with the specified AWS CLI profile
  call <- sprintf("%s --profile %s", call, aws_cli_profile)

  # Execute the command and capture the output and exit status
  output <- system(call, intern = TRUE, ignore.stderr = FALSE)
  if (!is.null(attr(output, "status"))) {
    stop(output)
  }
  output <- paste(output, collapse = "\n")
  # Attempt to parse the output as JSON
  jsonlite::fromJSON(output)
}

#' Duplicate an AWS ECS Task Definition for a new city
#'
#' Duplicates an existing AWS ECS task definition, substituting the specified city
#' name in the task definition, and registers it under the new city's name.
#'
#' @param new_city <`character`> The name of the new city for which to duplicate the task.
#' @param from <`character`> The name of the city from which to duplicate the task.
#' Defaults to "montreal".
#'
#' @return <`character`> A message indicating the outcome of the task duplication process.
#' @export
aws_duplicate_task <- function(new_city, from = "montreal") {

  # Get the definition of th
  task_def <- aws_cli_cmd(sprintf("aws ecs describe-task-definition --task-definition cc-%s",
                                   from))

  # Reorganise so it can be sent back as is
  task_def$taskDefinition <-
    task_def$taskDefinition[
      names(task_def$taskDefinition) %in% c("family", "taskRoleArn", "executionRoleArn", "networkMode",
                                            "containerDefinitions", "volumes", "placementConstraints",
                                            "requiresCompatibilities", "cpu", "memory", "tags", "pidMode",
                                            "ipcMode", "proxyConfiguration", "inferenceAccelerators",
                                            "ephemeralStorage", "runtimePlatform")]

  task_def <- task_def$taskDefinition
  task_def$requiresCompatibilities <- list(task_def$requiresCompatibilities)
  task_def <- jsonlite::toJSON(task_def, auto_unbox = TRUE, pretty = TRUE)
  task_def <- gsub(from, new_city, task_def)

  # Save to a temporary file
  tmp <- tempfile(fileext = ".json")
  writeLines(task_def, tmp)

  # Save the task
  query_task_reg <- sprintf("aws ecs register-task-definition --family cc-%s --cli-input-json file://%s",
                            new_city, tmp)
  aws_cli_cmd(query_task_reg)
}

#' Duplicate AWS ELB and Target Groups for a New City
#'
#' Duplicates an existing AWS Elastic Load Balancer and its associated target groups,
#' adjusting names and attributes for a new city. It also handles the duplication of
#' listeners for both HTTP and HTTPS protocols.
#'
#' @param new_city <`character`> The name of the new city for which to duplicate
#' the load balancer and target groups.
#' @param from <`character`> The name of the city from which to duplicate.
#' Defaults to "montreal".
#'
#' @return <`character`> A message indicating the outcome of the duplication process.
#' @export
aws_duplicate_LB <- function(new_city, from = "montreal") {
  # Get the target group to duplicate, and create it for the new city
  all_TGs <- aws_cli_cmd("aws elbv2 describe-target-groups")
  from_TG <- all_TGs$TargetGroups[grepl(from, all_TGs$TargetGroups$TargetGroupName), ]

  create_TG <- sprintf(paste0("aws elbv2 create-target-group --name %s --protocol ",
                              "%s --port %s --vpc-id %s --target-type %s"),
                       gsub(from, new_city, from_TG$TargetGroupName),
                       from_TG$Protocol, from_TG$Port, from_TG$VpcId,
                       from_TG$TargetType)
  created <- aws_cli_cmd(create_TG)
  TG_ARN <- created$TargetGroups$TargetGroupArn


  # Get attributes of the TG of the one to duplicate
  from_TG_attributes <- aws_cli_cmd(sprintf("aws elbv2 describe-target-group-attributes --target-group-arn %s",
                                             from_TG$TargetGroupArn))
  copied_attr <- paste0(sapply(seq_along(from_TG_attributes$Attributes$Key), \(x) {
    paste0(paste0("Key=", from_TG_attributes$Attributes$Key[[x]]),
           paste0(",Value=", from_TG_attributes$Attributes$Value[[x]]))
  }), collapse = " ")

  # Use the attributes of the TG to duplicate, and add them to the new curbcut
  sprintf("aws elbv2 modify-target-group-attributes --target-group-arn %s --attributes %s",
          TG_ARN, copied_attr) |>
    aws_cli_cmd()

  # Get the load balancer to dupliocate, and create it for the new city
  all_LB <- aws_cli_cmd("aws elbv2 describe-load-balancers")
  from_LB <- all_LB$LoadBalancers[grepl(from, all_LB$LoadBalancers$LoadBalancerName), ]

  # Save the LB
  create_lb <-
    paste(sprintf("--subnets %s", paste0(from_LB$AvailabilityZones[[1]]$SubnetId, collapse = " ")),
          sprintf("--security-groups %s", paste0(from_LB$SecurityGroups, collapse = " ")),
          sprintf("--scheme %s", paste0(from_LB$Scheme, collapse = " ")),
          sprintf("--ip-address-type %s", paste0(from_LB$IpAddressType, collapse = " ")))

  new_LB <- sprintf("aws elbv2 create-load-balancer --name %s %s",
                    gsub(from, new_city, from_LB$LoadBalancerName), create_lb) |>
    aws_cli_cmd()

  # Connect the LB with the TG! Add Listeners
  # Get Listerners of the one to duplicate
  from_listeners <-
    aws_cli_cmd(sprintf("aws elbv2 describe-listeners --load-balancer-arn %s",
                         from_LB$LoadBalancerArn))$Listeners

  # HTTPS
  default_actions_string <- from_listeners$DefaultActions[[1]]
  default_actions_json <- sprintf('Type="forward",TargetGroupArn="%s",Order=%s',
                                  TG_ARN, # TargetGroupArn
                                  default_actions_string$Order) # DurationSeconds for StickinessConfig
  create_listener <- paste(sprintf("--protocol %s", from_listeners$Protocol[[1]]),
                           sprintf("--port %s", from_listeners$Port[[1]]),
                           if (!is.na(from_listeners$SslPolicy[[1]])) sprintf("--ssl-policy %s", from_listeners$SslPolicy[[1]]) else "",
                           if (!is.na(from_listeners$Certificates[[1]])) sprintf("--certificates CertificateArn=%s", from_listeners$Certificates[[1]]) else "",
                           sprintf("--default-actions %s", default_actions_json))

  sprintf("aws elbv2 create-listener --load-balancer-arn %s %s",
          new_LB$LoadBalancers$LoadBalancerArn, create_listener) |>
    aws_cli_cmd()

  # HTTP
  default_actions_string <- from_listeners$DefaultActions[[2]]
  default_actions_json <- sprintf('Type="forward",TargetGroupArn="%s",Order=%s',
                                  TG_ARN, # TargetGroupArn
                                  default_actions_string$Order) # DurationSeconds for StickinessConfig
  create_listener <- paste(sprintf("--protocol %s", from_listeners$Protocol[[2]]),
                           sprintf("--port %s", from_listeners$Port[[2]]),
                           if (!is.na(from_listeners$SslPolicy[[2]])) sprintf("--ssl-policy %s", from_listeners$SslPolicy[[2]]) else "",
                           if (!is.null(from_listeners$Certificates[[2]])) sprintf("--certificates CertificateArn=%s", from_listeners$Certificates[[2]]) else "",
                           sprintf("--default-actions %s", default_actions_json))

  sprintf("aws elbv2 create-listener --load-balancer-arn %s %s",
          new_LB$LoadBalancers$LoadBalancerArn, create_listener) |>
    aws_cli_cmd()

  return("Success")
}

#' Duplicate an AWS ECS Service for a New City
#'
#' Duplicates an existing AWS ECS service for a new city, including updating the
#' task definition, target groups, and auto-scaling policies with city-specific names
#' and configurations.
#'
#' @param new_city <`character`> The name of the new city for which to duplicate the service.
#' @param from <`character`> The name of the city from which to duplicate.
#' Defaults to "montreal".
#' @param suffix <`character`> Issues sometimes when a service has been deleted, it
#' stays in a draining states for ever. We need to use a different name. Add a suffix
#' just for the service name.
#'
#'
#' @return <`character`> A message indicating the outcome of the service duplication process.
#' @export
aws_duplicate_service <- function(new_city, from = "montreal", suffix = NULL) {
  mtl_service <- aws_cli_cmd(sprintf("aws ecs describe-services --cluster curbcut --services curbcut-%s-service", from))$services

  # Create the service with the task definition
  create_service <- sprintf("aws ecs create-service --cluster curbcut --service-name curbcut-%s-service%s --task-definition cc-%s",
                            new_city, if (is.null(suffix)) "" else suffix, new_city)

  # Get this City's TG
  all_TGs <- aws_cli_cmd("aws elbv2 describe-target-groups")
  from_TG <- all_TGs$TargetGroups[grepl(from, all_TGs$TargetGroups$TargetGroupName), ]
  new_TG <- all_TGs$TargetGroups[grepl(new_city, all_TGs$TargetGroups$TargetGroupName), ]
  additional <-
    paste(sprintf('--load-balancers targetGroupArn="%s",containerName="%s",containerPort=%s',
                  new_TG$TargetGroupArn, gsub(from, new_city, mtl_service$loadBalancers[[1]]$containerName),
                  mtl_service$loadBalancers[[1]]$containerPort),
          sprintf("--desired-count %s", mtl_service$desiredCount),
          sprintf("--launch-type %s", "FARGATE"),
          sprintf("--network-configuration awsvpcConfiguration={subnets=[%s],securityGroups=[%s],assignPublicIp=%s}",
                  mtl_service$networkConfiguration$awsvpcConfiguration$subnets[[1]] |> paste0(collapse=","),
                  mtl_service$networkConfiguration$awsvpcConfiguration$securityGroups[[1]] |> paste0(collapse=","),
                  mtl_service$networkConfiguration$awsvpcConfiguration$assignPublicIp))

  aws_cli_cmd(paste(create_service, additional))

  # Add auto-scaling policies
  scalable_target <- aws_cli_cmd(sprintf("aws application-autoscaling register-scalable-target --service-namespace ecs --resource-id service/curbcut/curbcut-%s-service%s --scalable-dimension ecs:service:DesiredCount --min-capacity 1 --max-capacity 4", new_city, if (is.null(suffix)) "" else suffix))

  aws_duplicate_services_auto_scale(new_city = new_city, from = from,
                                    from_TG = from_TG, new_TG = new_TG,
                                    suffix = suffix)

}

#' Duplicate All AWS Components for a New City
#'
#' A wrapper function that duplicates all necessary AWS components for launching
#' in a new city. This includes task definitions, load balancers, target groups,
#' and ECS services.
#'
#' @param new_city <`character`> The name of the new city for which to duplicate all components.
#' @param from <`character`> The name of the city from which to duplicate. Optional.
#'
#' @return <`character`> A message indicating the outcome of the duplication process.
#' @export
aws_duplicate_all <- function(new_city, from = "montreal") {
  aws_duplicate_task(new_city, from)
  aws_duplicate_LB(new_city, from)
  aws_duplicate_service(new_city, from)

  cat(paste0("Note: For the CloudFront distribution, you will need to set it ",
             "with allowed HTTP methods: GET, HEAD, OPTIONS, PUT, POST, PATCH, ",
             "DELETE. You then need to configure the Route 53 DNS to include ",
             "the CloudFront distribution's domain name as an 'A' record ",
             "configured as an alias. This ensures that the application is ",
             "accessible via a user-friendly URL, routing through CloudFront ",
             "for optimized content delivery."))
}

#' Duplicate AWS Services and Auto-Scale for a new city
#'
#' Duplicates AWS auto-scaling configurations and related resources from an
#' existing city to a new city. This includes duplicating target groups and
#' load balancers, and updating scaling policies accordingly.
#'
#' @param new_city <`character`> string specifying the name of the new city
#' for which the AWS services should be duplicated.
#' @param from <`character`> string specifying the name of the original city
#' from which the AWS services are duplicated. Defaults to "montreal".
#' @param from_TG An optional `data.frame` specifying the original target group
#' details. If `NULL`, it will be fetched automatically based on the
#' `from` parameter.
#' @param new_TG An optional `data.frame` specifying the new target group details.
#' If `NULL`, it will be fetched automatically based on the `new_city` parameter.
#' @param from_LB An optional `data.frame` specifying the original load balancer
#' details. If `NULL`, it will be fetched automatically based on the
#' `from` parameter.
#' @param new_LB An optional `data.frame` specifying the new load balancer details.
#' If `NULL`, it will be fetched automatically based on the `new_city` parameter.
#' @param suffix <`character`> Issues sometimes when a service has been deleted, it
#' stays in a draining states for ever. We need to use a different name. Add a suffix
#' just for the service name.
#'
#' @details This function automatically identifies and duplicates AWS target groups,
#' load balancers, and auto-scaling policies from a specified "from" city
#' to a "new_city". It handles the creation of new target groups and load
#' balancers if not explicitly provided and updates scaling policies to
#' reflect the new city's resources.
#'
#' @return The function does not return any value but performs operations that
#' result in the creation and update of AWS resources.
aws_duplicate_services_auto_scale <- function(new_city, from = "montreal",
                                              from_TG = NULL, new_TG = NULL,
                                              from_LB = NULL, new_LB = NULL,
                                              suffix = NULL) {
  if (is.null(from_TG) & is.null(new_TG)) {
    all_TGs <- aws_cli_cmd("aws elbv2 describe-target-groups")
    from_TG <- all_TGs$TargetGroups[grepl(from, all_TGs$TargetGroups$TargetGroupName), ]
    new_TG <- all_TGs$TargetGroups[grepl(new_city, all_TGs$TargetGroups$TargetGroupName), ]
  }
  if (is.null(from_LB) & is.null(new_LB)) {
    all_LB <- aws_cli_cmd("aws elbv2 describe-load-balancers")
    from_LB <- all_LB$LoadBalancers[grepl(from, all_LB$LoadBalancers$LoadBalancerName), ]
    new_LB <- all_LB$LoadBalancers[grepl(new_city, all_LB$LoadBalancers$LoadBalancerName), ]
  }
  from_policies <- aws_cli_cmd("aws application-autoscaling describe-scaling-policies --service-namespace ecs")$ScalingPolicies
  from_policies <- from_policies[grepl(from, from_policies$ResourceId), ]

  for (i in seq_along(from_policies$PolicyARN)) {

    name <- gsub(from, new_city, from_policies$PolicyName[[i]])
    if (!grepl(new_city, name)) name <- paste0(name, "_", new_city)
    config_list <- from_policies$TargetTrackingScalingPolicyConfiguration[i, ] |>
      as.list()
    config_list <- lapply(config_list, \(x) if (is.data.frame(x)) as.list(x) else x)
    if (is.null(config_list$PredefinedMetricSpecification$ResourceLabel) | is.na(config_list$PredefinedMetricSpecification$ResourceLabel))
      config_list$PredefinedMetricSpecification$ResourceLabel <- NULL else {
        from_LB_ARN <- sub("^.*/", "", from_LB$LoadBalancerArn)
        new_LB_ARN <- sub("^.*/", "", new_LB$LoadBalancerArn)
        new_RL <- gsub(from_LB_ARN, new_LB_ARN, config_list$PredefinedMetricSpecification$ResourceLabel)

        new_TG_ARN <- sub("^.*/", "", new_TG$TargetGroupArn)
        from_TG_ARN <- sub("^.*/", "", from_TG$TargetGroupArn)
        new_RL <- gsub(from_TG_ARN, new_TG_ARN, new_RL)

        new_RL <- gsub(from, new_city, new_RL)
        config_list$PredefinedMetricSpecification$ResourceLabel <- new_RL
      }

    json_config <- jsonlite::toJSON(config_list, auto_unbox = TRUE, pretty = TRUE)
    # Save to a temporary file
    tmp <- tempfile(fileext = ".json")
    writeLines(json_config, tmp)

    call <- paste(sprintf("--policy-name %s", name),
                  sprintf("--resource-id %s", paste0(gsub(from, new_city, from_policies$ResourceId[[i]]), if (is.null(suffix)) "" else suffix)),
                  sprintf("--policy-type %s", from_policies$PolicyType[[i]]),
                  sprintf("--scalable-dimension %s", from_policies$ScalableDimension[[i]]),
                  sprintf("--target-tracking-scaling-policy-configuration file://%s", tmp))

    aws_cli_cmd(paste("aws application-autoscaling put-scaling-policy --service-namespace ecs", call))

  }

}

# from <- "montreal"
# for (new_city in c("laval", "bckelowna", "bccomoxvalley", "bcfraservalley", "bcvancouver", "bcprincegeorge")) {
#   all_TGs <- aws_cli_cmd("aws elbv2 describe-target-groups")
#   from_TG <- all_TGs$TargetGroups[grepl(from, all_TGs$TargetGroups$TargetGroupName), ]
#   new_TG <- all_TGs$TargetGroups[grepl(new_city, all_TGs$TargetGroups$TargetGroupName), ]
#
#   aws_duplicate_services_auto_scale(new_city = new_city, from = from,
#                                     from_TG = from_TG, new_TG = new_TG)
# }
#

# lapply(c("laval", "toronto"), \(new_city) {
#   aws_duplicate_LB(new_city, from)
#   aws_duplicate_service(new_city, from, suffix = 2)
# })

