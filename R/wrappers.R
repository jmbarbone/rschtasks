
#' @export
#' @rdname TaskScheduler
schtasks_run <- function(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL
) {
  do_schtasks(gather_params())$schtasks_run()
}

#' @export
#' @rdname TaskScheduler
schtasks_end <- function(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL
) {
  do_schtasks(gather_params())$schtasks_end()
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
  delay_time         = NULL
) {
  do_schtasks(gather_params())$schtasks_create()
}

#' @export
#' @rdname TaskScheduler
schtasks_create_xml <- function(xml_file) {
  do_schtasks(gather_params())$schtasks_create_xml()
}

#' @export
#' @rdname TaskScheduler
schtasks_delete <- function(
  task_name,
  system   = NULL,
  username = NULL,
  password = NULL,
  force    = FALSE
) {
  do_schtasks(gather_params())$schtasks_delete()
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
  xml_type  = NULL
) {
  do_schtasks(gather_params())$schtasks_query()
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
  delay_time         = NULL
) {
  do_schtasks(gather_params())$schtasks_change()
}

#' @export
#' @rdname TaskScheduler
schtasks_showsid <- function(task_name) {
  do_schtasks(gather_params())$schtasks_show_sid()
}


# helpers -----------------------------------------------------------------

#' Schedule task wrapper
#'
#' Wrapper for calling
#'
#' @param args args to be passed
#'
#' @keywords internal
#' @noRd
do_schtasks <- function(args) {
  do.call(TaskScheduler$new, args)
}

#' Gather params
#'
#' Gathers the formals and args passed in a function
#'
#' @keywords internal
#' @examples
#' foo <- function(a = 1, b = 3, c = 1:3, ..., .param = a) {
#'   gather_params2()
#' }
#'
#' # returns in original order
#' foo(c = 1, b = NULL)
#'
#' @noRd
gather_params <- function() {
  forms <- formals(sys.function(1))
  out <- c(as.list(sys.call(1))[-1], forms)
  out[!duplicated(names(out))][names(forms)]
}

