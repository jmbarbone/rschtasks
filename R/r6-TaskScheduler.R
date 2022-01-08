#' Task Scheduling
#'
#' @description
#' An R6 class for creating and checking a task call and wrappers
#'
#' @details
#' Commands are passed using [base::system2()]
#'
#' Wrappers for R6 object creation and command execution:
#'
#' \describe{
#'   \item{`schtasks_run()`}{Runs a scheduled task on demand}
#'   \item{`schtasks_end()`}{Stops a currently running scheduled task}
#'   \item{`schtasks_create()`}{Create a scheduled tasks on a local or remote
#'   system}
#'   \item{`schtasks_delete()`}{Deletes a scheduled task}
#'   \item{`schtasks_query()`}{Displays the scheduled tasks on the local or
#'   remote system.  Returns a `tibble`}
#'   \item{`schtasks_change()`}{Changes the program to run, or user account and
#'   password used by a scheduled task}
#'   \item{`schtasks_showsid()`}{Shows the security identifier corresponding to
#'   a scheduled task name}
#' }
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
#' @param env character vector of name=value strings to set environment
#'   variables (passed to [base::system2()])
#' @param convert Converts the output (currently only valid for `show_sid`)
#' @param quiet If `TRUE` will suppress outputs
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
#' @export
TaskScheduler <- R6::R6Class(
  "TaskScheduler",

  # Public ------------------------------------------------------------------

  # TODO do these have to be public fields?  Why not just all private?
  public = list(
    #' @field result The the  the call
    result = NULL,

    #' @field system_call The system call of the function
    system_call = NULL,

    #' @description Initializes the call
    initialize = function(
      # TODO add returns documentation for self$new()
      task_name          = NULL,
      system             = NULL,
      username           = NULL,
      password           = NULL,
      runas_username     = NULL,
      runas_password     = NULL,
      # note once is first
      schedule           = c("once", "minute", "hourly", "daily", "weekly", "monthly", "onstart", "onlogon", "onidle", "onevent"),
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
      xml_file           = NULL,
      format             = c("csv", "list", "table"),
      no_header          = FALSE,
      verbose            = FALSE,
      xml_type           = NULL, #single, concat
      raw                = FALSE,
      enable             = FALSE,
      disable            = FALSE,
      env                = "",
      convert            = TRUE,
      quiet              = FALSE
    ) {

      private$check_windows()
      private$check_schtasks()

      private$orig$task_name           <- private$task_name          <- task_name
      private$orig$system              <- private$system             <- system
      private$orig$username            <- private$username           <- username
      private$orig$password0           <- private$password0          <- password
      private$orig$runas_username      <- private$runas_username     <- runas_username
      private$orig$runas_password0     <- private$runas_password0    <- runas_password
      private$orig$schedule            <- private$schedule           <- toupper(arg_match(schedule))
      private$orig$modifier            <- private$modifier           <- modifier
      private$orig$days                <- private$days               <- days
      private$orig$months              <- private$months             <- months
      private$orig$idle_time           <- private$idle_time          <- idle_time
      private$orig$task_run            <- private$task_run           <- task_run
      private$orig$start_time          <- private$start_time         <- start_time
      private$orig$interval            <- private$interval           <- interval
      private$orig$end_time            <- private$end_time           <- end_time
      private$orig$duration            <- private$duration           <- duration
      private$orig$terminate           <- private$terminate          <- terminate
      private$orig$start_date          <- private$start_date         <- start_date
      private$orig$end_date            <- private$end_date           <- end_date
      private$orig$channel_name        <- private$channel_name       <- channel_name
      private$orig$delete_after_final  <- private$delete_after_final <- delete_after_final
      private$orig$v1                  <- private$v1                 <- v1
      private$orig$force               <- private$force              <- force
      private$orig$level               <- private$level              <- arg_match(level)
      private$orig$delay_time          <- private$delay_time         <- delay_time
      private$orig$format              <- private$format             <- arg_match(format)
      private$orig$convert             <- private$convert            <- is_true(convert)
      private$orig$quiet               <- private$quiet              <- is_true(quiet)

      invisible(self)
    },

    #' @description Runs `schtasks run`
    schtasks_run = function() {
      private$reset()
      private$param <- "run"

      private$check_task_name()
      private$check_system()
      private$check_username()
      private$check_password()

      private$args <- list(
        tn = private$task_name,
        s  = private$system,
        u  = private$username,
        i  = TRUE, # immediately
        p  = private$password0
      )

      private$send()
    },

    #' @description Runs `schtasks end`
    schtasks_end = function() {
      private$reset()
      private$param <- "end"

      private$check_task_name()
      private$check_system()
      private$check_username()
      private$check_password()

      private$args <- list(
        tn = private$task_name,
        s = private$system,
        u = private$username,
        p = private$password0
      )

      private$send()
    },

    #' @description Runs `schtasks create`
    schtasks_create = function() {
      private$reset()
      private$param <- "create"

      private$check_system()
      private$check_username()
      private$check_password()
      private$check_runas_username()
      private$check_runas_password()
      private$check_schedule()
      private$check_modifier()
      private$check_days()
      private$check_months()
      private$check_idletime()
      private$check_task_name()
      private$check_task_run()
      private$check_start_time()
      private$check_interval()
      private$check_end_time()
      private$check_duration()
      private$check_terminate()
      private$check_start_date()
      private$check_end_date()
      private$check_channel_name()
      private$check_delete_after_final()
      private$check_v1()
      private$check_force()
      private$check_level()
      private$check_delay_time()

      private$args <- list(
        s     = private$system,
        u     = private$username,
        p     = private$password0,
        ru    = private$runas_username,
        rp    = private$runas_password0,
        sc    = private$schedule,
        mo    = private$modifier,
        d     = private$days,
        m     = private$months,
        i     = private$idletime,
        tn    = private$task_name,
        tr    = private$task_run,
        st    = private$start_time,
        ri    = private$interval,
        et    = private$end_time,
        du    = private$duration,
        k     = private$terminate,
        sd    = private$start_date,
        ed    = private$end_date,
        ec    = private$channel_name,
        it    = NULL, # interactive
        np    = NULL, # no password
        z     = private$delete_after_final,
        xml   = NULL,
        v1    = private$v1,
        f     = private$force,
        rl    = private$level,
        delay = private$delay_time
      )

      private$send()
    },

    #' @description creates a task using an xml file, a special variant for
    #'   `schtasks create`
    schtasks_create_xml = function() {
      private$reset()
      private$param <- "create_xml"
      private$check_xml_file()
      private$args <- list(xml = xml_file)
      private$send()
    },

    #' @description Runs `schtasks delete`
    schtasks_delete = function() {
      private$reset()
      private$param <- "delete"

      private$check_task_name()
      private$check_system()
      private$check_username()
      private$check_password()
      private$check_force()

      private$args <- list(
        tn = private$task_name,
        s = private$system,
        u = private$username,
        p = private$password0,
        f = private$force
      )

      private$send()
    },

    #' @description Runs `schtasks query`
    schtasks_query = function() {
      private$reset()
      private$param <- "query"

      private$check_system()
      private$check_username()
      private$check_password()
      private$check_format()
      private$check_no_header()
      private$check_verbose()
      private$check_xml_type()

      private$args <- list(
        s   = private$system,
        u   = private$username,
        p   = private$password0,
        fo  = private$format,
        nh  = private$no_header,
        v   = private$verbose,
        xml = private$xml_type
      )

      private$send()
    },

    #' @description Runs `schtasks change`
    schtasks_change = function() {
      private$reset()
      private$param <- "change"

      private$check_system()
      private$check_username()
      private$check_password()
      private$check_task_name()
      private$check_runas_username()
      private$check_runas_password()
      private$check_task_run()
      private$check_start_time()
      private$check_interval()
      private$check_end_time()
      private$check_duration()
      private$check_terminate()
      private$check_start_date()
      private$check_end_date()
      private$check_level()
      private$check_enable()
      private$check_disable()
      private$check_delete_after_final()
      private$check_delay_time()

      private$args <- list(
        s       = private$system,
        u       = private$username,
        p       = private$password0,
        tn      = private$task_name,
        ru      = private$runas_username,
        rp      = private$runas_password0,
        tr      = private$task_run,
        st      = private$start_time,
        ri      = private$interval,
        et      = private$end_time,
        du      = private$duration,
        k       = private$terminate,
        sd      = private$start_date,
        ed      = private$end_date,
        it      = FALSE,
        rl      = private$level,
        enable  = private$enable,
        disable = private$disable,
        z       = private$delete_after_final,
        delay   = private$delay_time
      )

      private$send()
    },

    #' @description Runs `schtasks showsid`
    schtasks_show_sid = function() {
      private$reset()
      private$param <- "showsid"
      private$check_task_name()
      private$args <- list(tn = private$task_name)
      private$send()
    }
  ),

  # Private -----------------------------------------------------------------

  private = list(

    ## schtasks params ----
    params             = c("run", "end", "create", "create_xml", "delete", "query", "change", "showsid"),
    param              = NULL,
    task_name          = NULL,
    system             = NULL,
    username           = NULL,
    password           = NULL,
    runas_username     = NULL,
    runas_password     = NULL,
    # note: once is first
    # schedule = c("once", "minute", "hourly", "daily", "weekly", "monthly", "onstart", "onlogon", "onidle", "onevent"),
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
    xml_file           = NULL,
    format             = c("csv", "list", "table"),
    no_header          = FALSE,
    verbose            = FALSE,
    xml_type           = NULL,
    raw                = FALSE,
    enable             = FALSE,
    disable            = FALSE,

    ## others ----
    env                = "", # system2()
    password0          = NULL,
    runas_password0    = NULL,
    warnings           = NULL,
    args               = NULL,
    output             = NULL,
    stdout             = NULL,
    stderr             = NULL,
    convert            = TRUE,
    quiet              = FALSE,

    called             = FALSE,
    orig               = list(),

    ## functions ----

    # uses the original entries
    reset = function() {
      if (private$called) {
        for (i in names(private$orig)) {
          private[[i]] <- private$orig[[i]]
        }
      }
    },

    # wrap up
    send = function() {
      private$called <- TRUE
      private$clean_args()
      private$schtasks()
      private$show()
      invisible(self)
    },

    clean_args = function() {
      private$param <- arg_match0(private$param, private$params)

      private$args[purrr::map_lgl(private$args, is_true)] <- ""

      ok <- !purrr::map_lgl(private$args, ~is_false(.x) | is_null(.x))
      private$args <- private$args[ok]
      spaces <- grepl("[[:space:]]", private$args)
      nm <- paste0("/", names(private$args))
      private$args <- as.vector(private$args, "character")

      if (any(spaces)) {
        private$args[spaces] <- backslash(private$args[spaces])
        private$args[spaces] <- paste(nm[spaces], shQuote(private$args[spaces]))
      }

      if (any(!spaces)) {
        private$args[!spaces] <- paste(nm[!spaces], private$args[!spaces])
      }

      private$args <- trimws(private$args)
    },

    # calls schtasks via system2s
    schtasks = function() {
      stderr_file <- tempfile()
      on.exit(fs::file_delete(stderr_file), add = TRUE)

      self$system_call <- structure(
        list(
          "schtasks",
          # args = c(paste0("/", private$param), private$args)
          args = c(paste0("/", private$param), private$args)
        ),
        class = c("rschtasks_call", "list")
      )

      private$stdout <- suppressWarnings(system2(
        "schtasks",
        args      = self$system_call$args,
        stdout    = TRUE,
        stderr    = stderr_file,
        stdin     = "",
        input     = NULL,
        env       = private$env,
        wait      = TRUE,
        minimized = FALSE,
        invisible = TRUE,
        timeout   = 0
      ))

      # remove password
      self$system_call$args <- sub("/p\\s [[:alnum:][:punct:]]", "/p ****", self$system_call$args)

      private$stderr <- readLines(stderr_file)
      if (length(private$stderr)) {
        cat(self$system_call[[1]], " ", paste(self$system_call$args, collpse = " "), "\n", sep = "")
        warn(private$stderr)
      }
      invisible(self)
    },

    check_windows = function() {
      if (.Platform$OS.type != "windows") {
        abort(glue("OS is not windows"))
      }
    },

    check_schtasks = function() {
      loc <- system2("where", "schtasks", stdout = TRUE, stderr = TRUE)

      if (!fs::file_exists(loc)) {
        abort("Could not find schtasks")
      }
    },

    append_warnings = function(...) {
      private$warnings <- glue(private$warnings, ..., .sep = "\n")
    },

    # TODO should show() be a print() method instead? Or is this meant to be
    # private?
    show = function() {
      if (!is_null(private$warnings)) {
        # TODO use errors instead of warnings?
        # TODO add hooks: (Handle is invalid --> suggest force = TRUE)
        warn(private$warnings)
      }

      if (private$param == "query") {
        if (isTRUE(private$raw)) {
          self$result <- private$stdout
        } else {
          self$result <- switch(
            private$format,
            csv = schtasks_query_csv_read(private$stdout),
            list = schtasks_query_list_read(private$stdout),
            # table = private$stdout,
            {
              if (private$quiet) {
                cat(private$stdout, sep = "\n")
                private$stdout
              }
            }
          )
        }

        return(self$result)
      }

      self$result <- private$stdout

      if (private$convert) {
        # do others need conversion?
        self$result <- switch(
          private$param,
          # S-1-5-87-907618518-2201690017-3345919478-1888295809-4191631242
          showsid = regmatches(self$result, regexpr("[A-Z][-][0-9-]{60}", self$result)),
          self$result
        )
      }

      self$result <- structure(
        self$result,
        class = c("rschtasks_result", "character"),
        stdout = private$stdout
      )

      if (!private$quiet) {
        print(self$result)
      }
    },

    ## arg checkers ------------------------------------------------------------

    arg_schedule = c("minute", "hourly", "daily", "weekly", "monhtly", "once", "on_start", "on_logon", "on_idle", "on_event"),

    check_task_name = function() {
      if (is.null(private$task_name)) {
        abort("task_name must be set")
      }
    },

    check_password = function() {
      if (!is_null(private$password0)) {
        private$password0 <- rstudioapi::askForPassword(
          glue("Please enter password for {private$username}")
        )
      }
    },

    check_runas_password = function() {
      if (!is_null(private$runas_password0)) {
        # TODO private$runas_password0 or private$password0
        private$runas_password0 <- rstudioapi::askForPassword(
          glue("Please enter password for {private$runas_username}")
        )
      }
    },

    check_terminate = function() {
      if (private$terminate) {
        if (is_null(private$end_time) & is_null(private$duration)) {
          private$append_warnings(
            "terminate is set without either end_time or duration: This will be ignored"
          )
          private$terminate <- FALSE
        }

        if (is_true(private$schedule %in% c("on_start", "on_logon", "on_idle", "on_event"))) {
          private$append_warnings(
            "terminate is set but schedule is {schedule}: terminate will be ignored"
          )
          private$terminate <- FALSE
        }
      }
    },

    check_duration = function() {
      if (!is_null(private$duration)) {
        if (!is_null(private$end_time)) {
          private$append_warnings(
            "duration is so is end_time: duration will be ignored"
          )
          private$duration <- NULL
        }
      }
    },

    check_xml_file = function() {
      if (isTRUE(private$v1)) {
        if (!is_null(private$xml_file)) {
          private$append_warnings(
            "xml_file is not compatible with v1: ignoring xml_file"
          )
          private$xml_file <- NULL
          return()
        }
      }

      if (fs::is_file(private$xml_file)) {
        abort("xml_file not found")
      }
    },

    check_delay_time = function() {
      if (is_null(private$delay_time)) {
        return(NULL)
      }

      if (is_false(private$schedule %in% c("on_start", "on_logon", "on_event"))) {
        private$append_warnings(
          "schedule {private$schedule} is not valid for delay_time: delay_time will be ignored"
        )
        private$delay_time <- NULL
      }
    },

    #' check days
    check_days = function() {
      # browser()

      if (private$schedule == "ONCE") {
        private$days <- NULL
        return()
      }

      private$days <- private$days %||% "*"
      private$days <- tolower(private$days)

      private$days <- purrr::map_chr(
        private$days,
        ~switch(
          tolower(substr(.x, 1, 3)),
          monday    = "mon",
          tuesday   = "tue",
          wednesday = "wed",
          thursday  = "thu",
          friday    = "fri",
          saturday  = "sat",
          sunday    = "sun",
          .x
        )
      )

      if (is_null(private$days) || is_true(private$days == "*")) {
        return()
      }

      # What did .days_switch() do?
      # private$days <- .days_switch(private$days)

      if (is_null(private$schedule)) {
        private$schedule <- "<<not set>>"
      } else if (private$schedule == "monthly") {
        private$valid_days <- unique(c(private$valid_days, 1:31))
      }

      if (private$days %out% private$valid_days) {
        abort(glue("days `{x}` not valid for schedule {x}", x = private$days))
      }

      private$days <- toupper(private$days)
    },

    valid_days = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"),

    #' check months
    check_months = function() {
      if (is_null(private$months) || is_true(private$months == "*")) {
        return()
      }

      private$months <- tolower(private$months)
      private$months <- rep(private$month_abbr, 2)[match(private$months, c(private$month_abbr, private$month_name))]

      if (is_na(private$months)) {
        private$append_warnings("months [{private$months}] is not valid and will be ignored")
        private$months <- NULL
      }
    },

    mon_abbr = tolower(month.abb),
    mon_name = tolower(month.name),

    check_modifier = function() {
      if (is_null(private$modifier)) {
        return()
      }

      if (is_null(private$schedule)) {
        private$append_warnings("modifier [{private$modifier}] will be ignored as schedule is NULL")
        private$modifier <- NULL
        return()
      }

      switch(
        private$schedule,
        minute = private$modifer_limit(private$modifier, "minute", 1, 1439),
        hourly = private$modifer_limit(private$modifier, "hourly", 1, 23),
        daily = private$modifer_limit(private$modifier, "daily", 1, 365),
        weekly = private$modifer_limit(private$modifier, "weekly", 1, 52),
        monthly =  {
          if (is.character(private$modifier)) {
            private$modifier <- tolower(private$modifier)
            if (private$modifier %out% c("first", "second", "third", "fourth", "last", "lastday")) {
              private$append_warnings("modifier [{private$modifier}] is not valid for schedule [monhtly]")
              private$modifier <- NULL
              return()
            }
          } else {
            private$modifer_limit(private$modifier, "monhtly", 1, 52)
          }
        },
        onevent = private$check_xpath(private$modifier),
        # default
        {
          private$append_warnings("schedule [{private$schedule}] does not take any modifiers")
          private$modifier <- NULL
        }
      )
    },

    modifier_limit = function(x) {
      if (is_na(x)) {
        private$append_warnings("modifier is NA and will be ignored")
        return(NULL)
      }

      if (x < lower) {
        private$append_warnings("modifier [{x}] is below limit of [{lower}] for schedule [{private$schedule}]")
        return(lower)
      }

      if (x > upper) {
        private$append_warnings("modifier [{x}] is above limit of [{upper}] for schedule [{private$schedule}]")
        return(upper)
      }

      x
    },

    #' check idle_time
    check_idle_time = function() {
      if (is_null(private$idle_time)) {
        return()
      }

      # this conflicts with below -- may need to double check
      # if (!isTRUE(schedule == "on_idle")) {
      #   private$append_warnings("idle_time can only be set for schedule on_idle")
      #   idle_time <- NULL
      # }

      if (is_null(private$schedule) || private$schedule != "on_event") {
        private$append_warnings("idle_time is only available when schedule is on_event")
        private$idle_time <- NULL
        return()
      }

      if (private$idle_time < 1) {
        private$append_warnings("cannot set idle_time below 1")
        private$idle_time <- 1
      } else if (private$idle_time > 999) {
        private$append_warnings("cannot set idle_time above 999")
        private$idle_time <- 999
      }
    },

    check_schedule = function() {
      if (is_null(private$schedule)) {
        return()
      }

      private$schedule <-  sub("_", "", private$schedule)

      if (private$schedule == "ONCE") {
        private$days <- NULL
      }
    },

    check_start_time = function() {
      if (identical(private$start_time, "now")) {
        private$start_time <- Sys.time()
      } else if (identical(private$start_time, "asap")) {
        private$start_time <- fmt_hhmm(Sys.time() + 60)
      }

      if (inherits(private$start_time, "POSIXt")) {
        if (round(private$start_time, "mins") == round(Sys.time(), "mins")) {
          message("start_time is delayed a minute to prevent error")
          private$start_time <- trunc(Sys.time(), "mins") + 60
        }
      }

      if (!is.null(private$start_time)) {
        private$start_time <- fmt_hhmm(private$start_time)
      }
    },

    check_force = function() {
      if (!is_true_false(private$force)) {
        abort("force must be TRUE or FALSE")
      }
    },

    ## Null functions ----------------------------------------------------------

    check_end_time           = function() { }, # similar to start time?
    check_end_date           = function() { },
    check_start_date         = function() { },
    check_system             = function() { },
    check_username           = function() { },
    check_runas_username     = function() { },
    check_task_run           = function() { },
    check_interval           = function() { },
    check_format             = function() { },
    check_channel_name       = function() { },
    check_delete_after_final = function() { },
    check_v1                 = function() { },
    check_level              = function() { },
    check_xml_type           = function() { },
    check_verbose            = function() { },
    check_no_header          = function() { },
    check_idletime           = function() { }
  )
)


# print -------------------------------------------------------------------


#' @export
print.rschtasks_call <- function(x, ...) {
  cat(crayon::yellow(x[[1]]), x$args, "\n")
  invisible(x)
}

#' @export
print.rschtasks_result <- function(x, ...) {
  cat(gsub("^SUCCESS", crayon::green("SUCCESS"), attr(x, "stdout")), "\n")
  invisible(x)
}
