
#' @export
#' @rdname TaskScheduler
schtasks_run <- function(
  task_name,
  system   = NULL,
  username = NULL,
  password = NULL,
  quiet    = FALSE
) {
  TaskScheduler$new(
    task_name = task_name,
    system    = system,
    username  = username,
    password  = password,
    quiet     = quiet
  )$schtasks_run()
}

#' @export
#' @rdname TaskScheduler
schtasks_end <- function(
  task_name,
  system   = NULL,
  username = NULL,
  password = NULL,
  quiet    = FALSE

) {
  TaskScheduler$new(
    task_name = task_name,
    system    = system,
    username  = username,
    password  = password,
    quiet     = quiet
  )$schtasks_end()
}

#' @export
#' @rdname TaskScheduler
schtasks_create <- function(
  task_name,
  system             = NULL,
  username           = NULL,
  password           = NULL,
  runas_username     = NULL,
  runas_password     = NULL,
  schedule           = NULL,
  modifier           = NULL,
  days               = NULL,
  months             = NULL,
  idle_time          = NULL,
  task_run           = NULL,
  start_time         = NULL,
  interval           = NULL,
  end_time           = NULL,
  duration           = NULL,
  terminate          = FALSE,
  start_date         = NULL,
  end_date           = NULL,
  channel_name       = NULL,
  delete_after_final = FALSE,
  v1                 = FALSE,
  force              = FALSE,
  level              = c("limited", "highest"),
  delay_time         = NULL,
  quiet              = FALSE
) {
  TaskScheduler$new(
    task_name          = task_name,
    system             = system,
    username           = username,
    password           = password,
    runas_username     = runas_username,
    runas_password     = runas_password,
    schedule           = schedule,
    modifier           = modifier,
    days               = days,
    months             = months,
    idle_time          = idle_time,
    task_run           = task_run,
    start_time         = start_time,
    interval           = interval,
    end_time           = end_time,
    duration           = duration,
    terminate          = terminate,
    start_date         = start_date,
    end_date           = end_date,
    channel_name       = channel_name,
    delete_after_final = delete_after_final,
    v1                 = v1,
    force              = force,
    level              = level,
    delay_time         = delay_time,
    quiet              = quiet
  )$schtasks_create()
}

#' @export
#' @rdname TaskScheduler
schtasks_create_xml <- function(xml_file) {
  TaskScheduler$new(xml_file = xml_file)$schtasks_create_xml()
}

#' @export
#' @rdname TaskScheduler
schtasks_delete <- function(
  task_name,
  system   = NULL,
  username = NULL,
  password = NULL,
  force    = FALSE,
  quiet    = FALSE
) {
  TaskScheduler$new(
    task_name = task_name,
    system    = system,
    username  = username,
    password  = password,
    force     = force,
    quiet     = quiet
  )$schtasks_delete()
}

#' @export
#' @rdname TaskScheduler
schtasks_query <- function(
  system    = NULL,
  username  = NULL,
  password  = NULL,
  format    = c("csv", "list", "table"),
  no_header = FALSE,
  verbose   = FALSE,
  raw       = FALSE,
  xml_type  = NULL,
  quiet     = FALSE
) {
  TaskScheduler$new(
    system    = system,
    username  = username,
    password  = password,
    format    = format,
    no_header = no_header,
    verbose   = verbose,
    raw       = raw,
    xml_type  = xml_type,
    quiet     = quiet
  )$schtasks_query()
}


#' @export
#' @rdname TaskScheduler
schtasks_change <- function(
  task_name,
  system             = NULL,
  username           = NULL,
  password           = NULL,
  runas_username     = NULL,
  runas_password     = NULL,
  task_run           = NULL,
  start_time         = NULL,
  interval           = NULL,
  end_time           = NULL,
  duration           = NULL,
  terminate          = FALSE,
  start_date         = NULL,
  end_date           = NULL,
  level              = NULL,
  enable             = TRUE,
  disable            = FALSE,
  delete_after_final = FALSE,
  delay_time         = NULL,
  quiet              = FALSE
) {
  TaskScheduler$new(
    task_name          = task_name,
    system             = system,
    username           = username,
    password           = password,
    runas_username     = runas_username,
    runas_password     = runas_password,
    task_run           = task_run,
    start_time         = start_time,
    interval           = interval,
    end_time           = end_time,
    duration           = duration,
    terminate          = terminate,
    start_date         = start_date,
    end_date           = end_date,
    level              = level,
    enable             = enable,
    disable            = disable,
    delete_after_final = delete_after_final,
    delay_time         = delay_time,
    quiet              = quiet
  )$schtasks_change()
}

#' @export
#' @rdname TaskScheduler
schtasks_showsid <- function(task_name, convert = TRUE) {
  TaskScheduler$new(
    task_name = task_name,
    convert   = convert
  )$schtasks_show_sid()
}
