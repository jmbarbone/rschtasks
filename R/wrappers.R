#' Task Scheduling
#'
#' @description
#' An R6 class for creating and checking a task call and wrappers
#'
#' @param system The remote system to connect to
#' @param username The user context under which the `schtasks.exe` should
#'   execute
#' @param password The password for the given user context
#' @param task_name The path/name of the task to run now
#' @param runas_username The "run as" user account (user context) under which
#'   the tasks runs.  For the system account, valid values are `""`, `"NT
#'   AUTHORITY/SYSTEM"`, or `"SYSTEM"`.  For v2 tasks, `"NT
#'   AUTHORITY/LOCALSERVICE"` and `"NT AUTHORITYNETWORKSERVICE"` are also
#'   available as well as the well known SIDs for all three.
#' @param runas_password The password for the "run as" user.
#' @param schedule The schedule frequency.  Valid schedule types are : `minute`,
#'   `hourly`, `daily`, `weekly`, `monthly`, `once`, `on_start`, `on_logon`,
#'   `on_idle`, and `on_event`
#' @param modifier Refines the schedule type to allow finer control over
#'   schedule recurrence.  See section `Modifiers` for more details.
#' @param days The day of the week to run the task.  Can be either the name of
#'   the week (or an abbreviation).  If `schedule` is `monthly` can accept 1-31.
#'   Use `*` to specify all months.
#' @param months The month(s) of the year.  Defaults to the first of the month.
#'   Can use either the whole month (`month.name`) or month abbreviation
#'   (`month.abb`).
#' @param idle_time The amount of idle time to wait before running a task. Only
#'   valid for schedule `on_idle`.  Valid range from 1 to 999 (minutes).
#' @param task_run A string in the form of the `path/name` which uniquely
#'   identifies the scheduled task
#' @param start_time The start time to run the task.  Time format is `HH:mm` (24
#'   hour time).  Can accept `POSIXct` or `POSIXlt`.  Two special cases of
#'   `"now"` and `"asap"` are accepted; which are converted to the current time
#'   or the next minute, respectively.
#' @param interval The repetition interval in minutes.  Only applicable for
#'   `daily`, `weekly`, `monthly`, `once`.  Valid ranges are 1 - 599940. If
#'   `end_time` or `duration` are specified, `10` is set as the default.
#' @param end_time  The end time to run the task. The time format is `HH:mm` (24
#'   hour time).  Not applicable for `on_*` schedules.
#' @param duration The duration to run the task.  The time format is `HH:MM`.
#'   This is not applicable with `end_time`.  If `v1` is `TRUE` and `interval`
#'   is set, the duration defaults to `01:00`.
#' @param terminate If `TRUE`, terminates the task at the `end_time` or
#'   `duration time`.  This is not applicable for schedule types: `on_start`,
#'   `on_logon`, `on_idle`, and `on_event`.`
#' @param start_date The first date on which the task runs. Not applicable for
#'   schedules `once` or `on_*`.
#' @param end_date end_date The last date on which the task runs. Not applicable
#'   for schedules `once` or `on_*`.
#' @param channel_name The event channel for `on_event` triggers
#' @param delete_after_final If `TRUE` marks the task for deletion after its
#'   final run.
#' @param v1 If `TRUE` creates a task visible to pre-Vista platforms
#' @param force If `TRUE` forcefully creates the task and suppresses cmd
#'   warnings if the task already exists.
#' @param level The run level for the job, either `limited` or `highest`.
#' @param delay_time The wait time to delay the running of the task after the
#'   trigger is fired.  Time format is `mmmm::ss`.  Only valid for schedules
#'   `on_start`, `on_logon`, and `on_event`.
#' @param xml_file A path to a `.xml` file
#' @param format The format for the output.  One of: `csv`, `list`, or `table`
#' @param no_header If `TRUE`, does not display
#' @param verbose If `TRUE` displays the verbose task output
#' @param xml_type Displays the task definitions in XML format.  If `xml_type`
#'   is `single` then the output will be one valid `XML` file. If `concat` will
#'   concatenate all the `XML` definitions.
#' @param raw If `TRUE` returns the raw output rather than a `tibble`
#' @param enable If `TRUE` enables the scheduled task
#' @param disable If `TRUE` disables the scheduled task
#' @param convert Converts the output (currently only valid for `show_sid`)
#' @param quiet If `TRUE` will suppress outputs

#' @details
#' Commands are passed using [base::system2()]
#'
#' Wrappers for R6 object creation and command execution:
#'
#' \describe{
#'   \item{[schtasks_run()]}{Runs a scheduled task on demand}
#'   \item{[schtasks_end()]}{Stops a currently running scheduled task}
#'   \item{[schtasks_create()]}{Create a scheduled tasks on a local or remote
#'   system}
#'   \item{[schtasks_delete()]}{Deletes a scheduled task}
#'   \item{[schtasks_query()]}{Displays the scheduled tasks on the local or
#'   remote system.  Returns a `tibble`}
#'   \item{[schtasks_change()]}{Changes the program to run, or user account and
#'   password used by a scheduled task}
#'   \item{[schtasks_showsid()]}{Shows the security identifier corresponding to
#'   a scheduled task name}
#' }
#'
#' @section Modifiers:
#'
#' Valid values for the modifiers are as follows:
#'
#' \describe{
#'   \item{`monthly`}{1 - 12 or "first", "second", "third", "fourth", "last", or
#'   "last_day"}
#'   \item{`weekly`}{weeks 1 - 52}
#'   \item{`minute`}{1 - 1439 minutes}
#'   \item{`daily`}{1 - 365 days}
#'   \item{`hourly`}{1 - 23 hours}
#'   \item{`once`}{nothing}
#'   \item{`on_start`}{nothing}
#'   \item{`on_logon`}{nothing}
#'   \item{`on_idle`}{nothing}
#'   \item{`on_event`}{XPath event query string}
#' }
#'
#' @examples
#' \dontrun{
#' # These are recreations of the examples from the cmd help documents.  These
#' #   are nonsensical.
#'
#' # Creates a scheduled task
#' # Creates a scheduled task "doc" on the remote machine "abc" which runs
#' #   notepad.exe every hour under user "runasuser"
#' schtasks_create(
#'   system = "abc",
#'   username = "user",
#'   password = "password",
#'   runas_username = "unasuer",
#'   runas_password = "runaspassword",
#'   schedule = "hourly",
#'   task_name = "doc",
#'   task_run = "notepad"
#' )
#'
#' # Change a task
#' schtasks_change(
#'   password = "password",
#'   task_name = "/Backup/Backup and Restore"
#' )
#' schtasks_change(
#'   task_run = "restore.exe",
#'   task_name = "/Backup/Start Restore"
#' )
#'
#' schtasks_change(
#'   system = "system",
#'   username = "user",
#'   password = "password",
#'   runas_username = "newuser",
#'   task_name = "/Backup/Start Backup"
#' )
#'
#' # Query a task
#' schtasks_query(
#'   system = "system",
#'   username = "user",
#'   password = "password"
#' )
#'
#' schtasks_query(
#'   format = "list",
#'   verbose = TRUE,
#'   system = "system",
#'   username = "user",
#'   password = "password"
#' )
#' schtasks_query(
#'   format = "table",
#'   no_header = TRUE,
#'   verbose = TRUE
#' )
#' }
#'
#' @family schtasks
#' @name schtasks
NULL

#' @export
#' @rdname schtasks
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
  )$run()
}

#' @export
#' @rdname schtasks
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
  )$end()
}

#' @export
#' @rdname schtasks
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
  )$create()
}

#' @export
#' @rdname schtasks
schtasks_create_xml <- function(xml_file) {
  TaskScheduler$new(xml_file = xml_file)$create_xml()
}

#' @export
#' @rdname schtasks
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
  )$delete()
}

#' @export
#' @rdname schtasks
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
  )$query()
}


#' @export
#' @rdname schtasks
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
  )$change()
}

#' @export
#' @rdname schtasks
schtasks_showsid <- function(task_name, convert = TRUE) {
  TaskScheduler$new(
    task_name = task_name,
    convert   = convert
  )$show_sid()
}
