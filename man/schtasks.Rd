% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r6-TaskScheduler.R, R/wrappers.R
\name{TaskScheduler}
\alias{TaskScheduler}
\alias{schtasks}
\alias{schtasks_run}
\alias{schtasks_end}
\alias{schtasks_create}
\alias{schtasks_create_xml}
\alias{schtasks_delete}
\alias{schtasks_query}
\alias{schtasks_change}
\alias{schtasks_showsid}
\title{Task Scheduling}
\usage{
schtasks_run(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL,
  quiet = FALSE
)

schtasks_end(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL,
  quiet = FALSE
)

schtasks_create(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL,
  runas_username = NULL,
  runas_password = NULL,
  schedule = NULL,
  modifier = NULL,
  days = NULL,
  months = NULL,
  idle_time = NULL,
  task_run = NULL,
  start_time = NULL,
  interval = NULL,
  end_time = NULL,
  duration = NULL,
  terminate = FALSE,
  start_date = NULL,
  end_date = NULL,
  channel_name = NULL,
  delete_after_final = FALSE,
  v1 = FALSE,
  force = FALSE,
  level = c("limited", "highest"),
  delay_time = NULL,
  quiet = FALSE
)

schtasks_create_xml(xml_file)

schtasks_delete(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL,
  force = FALSE,
  quiet = FALSE
)

schtasks_query(
  system = NULL,
  username = NULL,
  password = NULL,
  format = c("csv", "list", "table"),
  no_header = FALSE,
  verbose = FALSE,
  raw = FALSE,
  xml_type = NULL,
  quiet = FALSE
)

schtasks_change(
  task_name,
  system = NULL,
  username = NULL,
  password = NULL,
  runas_username = NULL,
  runas_password = NULL,
  task_run = NULL,
  start_time = NULL,
  interval = NULL,
  end_time = NULL,
  duration = NULL,
  terminate = FALSE,
  start_date = NULL,
  end_date = NULL,
  level = NULL,
  enable = TRUE,
  disable = FALSE,
  delete_after_final = FALSE,
  delay_time = NULL,
  quiet = FALSE
)

schtasks_showsid(task_name, convert = TRUE)
}
\arguments{
\item{task_name}{The path/name of the task to run now}

\item{system}{The remote system to connect to}

\item{username}{The user context under which the \code{schtasks.exe} should
execute}

\item{password}{The password for the given user context}

\item{quiet}{If \code{TRUE} will suppress outputs}

\item{runas_username}{The "run as" user account (user context) under which
the tasks runs.  For the system account, valid values are \code{""}, \code{"NT AUTHORITY/SYSTEM"}, or \code{"SYSTEM"}.  For v2 tasks, \code{"NT AUTHORITY/LOCALSERVICE"} and \code{"NT AUTHORITYNETWORKSERVICE"} are also
available as well as the well known SIDs for all three.}

\item{runas_password}{The password for the "run as" user.}

\item{schedule}{The schedule frequency.  Valid schedule types are : \code{minute},
\code{hourly}, \code{daily}, \code{weekly}, \code{monthly}, \code{once}, \code{on_start}, \code{on_logon},
\code{on_idle}, and \code{on_event}}

\item{modifier}{Refines the schedule type to allow finer control over
schedule recurrence.  See section \code{Modifiers} for more details.}

\item{days}{The day of the week to run the task.  Can be either the name of
the week (or an abbreviation).  If \code{schedule} is \code{monthly} can accept 1-31.
Use \code{*} to specify all months.}

\item{months}{The month(s) of the year.  Defaults to the first of the month.
Can use either the whole month (\code{month.name}) or month abbreviation
(\code{month.abb}).}

\item{idle_time}{The amount of idle time to wait before running a task. Only
valid for schedule \code{on_idle}.  Valid range from 1 to 999 (minutes).}

\item{task_run}{A string in the form of the \code{path/name} which uniquely
identifies the scheduled task}

\item{start_time}{The start time to run the task.  Time format is \code{HH:mm} (24
hour time).  Can accept \code{POSIXct} or \code{POSIXlt}.  Two special cases of
\code{"now"} and \code{"asap"} are accepted; which are converted to the current time
or the next minute, respectively.}

\item{interval}{The repetition interval in minutes.  Only applicable for
\code{daily}, \code{weekly}, \code{monthly}, \code{once}.  Valid ranges are 1 - 599940. If
\code{end_time} or \code{duration} are specified, \code{10} is set as the default.}

\item{end_time}{The end time to run the task. The time format is \code{HH:mm} (24
hour time).  Not applicable for \verb{on_*} schedules.}

\item{duration}{The duration to run the task.  The time format is \code{HH:MM}.
This is not applicable with \code{end_time}.  If \code{v1} is \code{TRUE} and \code{interval}
is set, the duration defaults to \code{01:00}.}

\item{terminate}{If \code{TRUE}, terminates the task at the \code{end_time} or
\verb{duration time}.  This is not applicable for schedule types: \code{on_start},
\code{on_logon}, \code{on_idle}, and \code{on_event}.`}

\item{start_date}{The first date on which the task runs. Not applicable for
schedules \code{once} or \verb{on_*}.}

\item{end_date}{end_date The last date on which the task runs. Not applicable
for schedules \code{once} or \verb{on_*}.}

\item{channel_name}{The event channel for \code{on_event} triggers}

\item{delete_after_final}{If \code{TRUE} marks the task for deletion after its
final run.}

\item{v1}{If \code{TRUE} creates a task visible to pre-Vista platforms}

\item{force}{If \code{TRUE} forcefully creates the task and suppresses cmd
warnings if the task already exists.}

\item{level}{The run level for the job, either \code{limited} or \code{highest}.}

\item{delay_time}{The wait time to delay the running of the task after the
trigger is fired.  Time format is \code{mmmm::ss}.  Only valid for schedules
\code{on_start}, \code{on_logon}, and \code{on_event}.}

\item{xml_file}{A path to a \code{.xml} file}

\item{format}{The format for the output.  One of: \code{csv}, \code{list}, or \code{table}}

\item{no_header}{If \code{TRUE}, does not display}

\item{verbose}{If \code{TRUE} displays the verbose task output}

\item{raw}{If \code{TRUE} returns the raw output rather than a \code{tibble}}

\item{xml_type}{Displays the task definitions in XML format.  If \code{xml_type}
is \code{single} then the output will be one valid \code{XML} file. If \code{concat} will
concatenate all the \code{XML} definitions.}

\item{enable}{If \code{TRUE} enables the scheduled task}

\item{disable}{If \code{TRUE} disables the scheduled task}

\item{convert}{Converts the output (currently only valid for \code{show_sid})}
}
\description{
An R6 class for creating and checking a task call and wrappers
}
\details{
Commands are passed using \code{\link[base:system2]{base::system2()}}

Wrappers for R6 object creation and command execution:

\describe{
\item{\code{\link[=schtasks_run]{schtasks_run()}}}{Runs a scheduled task on demand}
\item{\code{\link[=schtasks_end]{schtasks_end()}}}{Stops a currently running scheduled task}
\item{\code{\link[=schtasks_create]{schtasks_create()}}}{Create a scheduled tasks on a local or remote
system}
\item{\code{\link[=schtasks_delete]{schtasks_delete()}}}{Deletes a scheduled task}
\item{\code{\link[=schtasks_query]{schtasks_query()}}}{Displays the scheduled tasks on the local or
remote system.  Returns a \code{tibble}}
\item{\code{\link[=schtasks_change]{schtasks_change()}}}{Changes the program to run, or user account and
password used by a scheduled task}
\item{\code{\link[=schtasks_showsid]{schtasks_showsid()}}}{Shows the security identifier corresponding to
a scheduled task name}
}
}
\section{Modifiers}{


Valid values for the modifiers are as follows:

\describe{
\item{\code{monthly}}{1 - 12 or "first", "second", "third", "fourth", "last", or
"last_day"}
\item{\code{weekly}}{weeks 1 - 52}
\item{\code{minute}}{1 - 1439 minutes}
\item{\code{daily}}{1 - 365 days}
\item{\code{hourly}}{1 - 23 hours}
\item{\code{once}}{nothing}
\item{\code{on_start}}{nothing}
\item{\code{on_logon}}{nothing}
\item{\code{on_idle}}{nothing}
\item{\code{on_event}}{XPath event query string}
}
}

\section{\code{schtasks.exe}}{


These functions specifically make it easier to an R user on Windows to
interact with the
\href{https://docs.microsoft.com/en-us/windows/win32/taskschd/schtasks}{\code{Schtasks.exe}}
applications.  Note that successful use of \code{Schtasks.exe} required \strong{Administrative privledged} and should be \emph{run as an administrator}.  This will likely be an issue if the user is using Rstudio
}

\examples{
\dontrun{
# These are recreations of the examples from the cmd help documents.  These
#   are nonsensical.

# Creates a scheduled task
# Creates a scheduled task "doc" on the remote machine "abc" which runs
#   notepad.exe every hour under user "runasuser"
schtasks_create(
  system = "abc",
  username = "user",
  password = "password",
  runas_username = "unasuer",
  runas_password = "runaspassword",
  schedule = "hourly",
  task_name = "doc",
  task_run = "notepad"
)

# Change a task
schtasks_change(
  password = "password",
  task_name = "/Backup/Backup and Restore"
)
schtasks_change(
  task_run = "restore.exe",
  task_name = "/Backup/Start Restore"
)

schtasks_change(
  system = "system",
  username = "user",
  password = "password",
  runas_username = "newuser",
  task_name = "/Backup/Start Backup"
)

# Query a task
schtasks_query(
  system = "system",
  username = "user",
  password = "password"
)

schtasks_query(
  format = "list",
  verbose = TRUE,
  system = "system",
  username = "user",
  password = "password"
)
schtasks_query(
  format = "table",
  no_header = TRUE,
  verbose = TRUE
)
}

}
\concept{schtasks}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{result}}{The the  the call}

\item{\code{system_call}}{The system call of the function}

\item{\code{exec}}{Should schtasks.exe be executed?  For testing purposes only.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{TaskScheduler$new()}}
\item \href{#method-run}{\code{TaskScheduler$run()}}
\item \href{#method-end}{\code{TaskScheduler$end()}}
\item \href{#method-create}{\code{TaskScheduler$create()}}
\item \href{#method-create_xml}{\code{TaskScheduler$create_xml()}}
\item \href{#method-delete}{\code{TaskScheduler$delete()}}
\item \href{#method-query}{\code{TaskScheduler$query()}}
\item \href{#method-change}{\code{TaskScheduler$change()}}
\item \href{#method-show_sid}{\code{TaskScheduler$show_sid()}}
\item \href{#method-clone}{\code{TaskScheduler$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initializes the call
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$new(
  task_name = NULL,
  system = NULL,
  username = NULL,
  password = NULL,
  runas_username = NULL,
  runas_password = NULL,
  schedule = c("once", "minute", "hourly", "daily", "weekly", "monthly", "onstart",
    "onlogon", "onidle", "onevent"),
  modifier = NULL,
  days = NULL,
  months = NULL,
  idle_time = NULL,
  task_run = NULL,
  start_time = NULL,
  interval = NULL,
  end_time = NULL,
  duration = NULL,
  terminate = FALSE,
  start_date = NULL,
  end_date = NULL,
  channel_name = NULL,
  delete_after_final = FALSE,
  v1 = FALSE,
  force = FALSE,
  level = c("limited", "highest"),
  delay_time = NULL,
  xml_file = NULL,
  format = c("csv", "list", "table"),
  no_header = FALSE,
  verbose = FALSE,
  xml_type = NULL,
  raw = FALSE,
  enable = FALSE,
  disable = FALSE,
  env = "",
  convert = TRUE,
  quiet = FALSE,
  exec = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{env}}{character vector of name=value strings to set environment
variables (passed to \code{\link[base:system2]{base::system2()}})}

\item{\code{exec}}{If \code{FALSE} will not run \code{\link[base:system2]{base::system2()}}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-run"></a>}}
\if{latex}{\out{\hypertarget{method-run}{}}}
\subsection{Method \code{run()}}{
Runs \verb{schtasks run}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$run()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-end"></a>}}
\if{latex}{\out{\hypertarget{method-end}{}}}
\subsection{Method \code{end()}}{
Runs \verb{schtasks end}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$end()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-create"></a>}}
\if{latex}{\out{\hypertarget{method-create}{}}}
\subsection{Method \code{create()}}{
Runs \verb{schtasks create}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$create()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-create_xml"></a>}}
\if{latex}{\out{\hypertarget{method-create_xml}{}}}
\subsection{Method \code{create_xml()}}{
creates a task using an xml file, a special variant for
\verb{schtasks create}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$create_xml()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-delete"></a>}}
\if{latex}{\out{\hypertarget{method-delete}{}}}
\subsection{Method \code{delete()}}{
Runs \verb{schtasks delete}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$delete()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-query"></a>}}
\if{latex}{\out{\hypertarget{method-query}{}}}
\subsection{Method \code{query()}}{
Runs \verb{schtasks query}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$query()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-change"></a>}}
\if{latex}{\out{\hypertarget{method-change}{}}}
\subsection{Method \code{change()}}{
Runs \verb{schtasks change}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$change()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-show_sid"></a>}}
\if{latex}{\out{\hypertarget{method-show_sid}{}}}
\subsection{Method \code{show_sid()}}{
Runs \verb{schtasks showsid}
check days
check months
check idle_time
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$show_sid()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskScheduler$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
