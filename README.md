
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rschtasks

<!-- badges: start -->
<!-- badges: end -->

The goal of rschtasks is to make it easier to created scheduled tasks on
a Windows machine in R.

## Installation

You can install the development version of rschtasks from
[Github](https://github.com/jmbarbone/rschtasks)

``` r
remotes::install_github("jmbarbone/rschtasks")
```

## Examples

``` r
library(rschtasks)
```

Create an object with the task parameters. This includes several checks
to make sure the tasks are set up properly, with a few additional
features.

``` r
task <- TaskScheduler$new(
  task_name  = "open notepad",
  task_run   = "notepad",
  schedule   = "once",
  start_time = "now",
  quiet      = TRUE,
  force      = TRUE
)
```

With the task you can run commands and see the results and calls. The
task object can be reused.

``` r
task$create()
#> start_time is delayed a minute to prevent error
task$result
#> SUCCESS: The scheduled task "open notepad" has successfully been created.
task$system_call
#> schtasks /create /sc ONCE /tn "open notepad" /tr notepad /st 17:05 /f /rl limited

task$show_sid()
task$result
#> SUCCESS: The SID "S-1-5-87-907618518-2201690017-3345919478-1888295809-4191631242" for the user name "open notepad" has been computed successfully.
task$system_call
#> schtasks /showsid /tn "open notepad"

task$delete()
task$result
#> SUCCESS: The scheduled task "open notepad" was successfully deleted.
task$system_call
#> schtasks /delete /tn "open notepad" /f
```

Alternatively, wrappers are provided and can be used instead.

``` r
schtasks_create(
  "open notepad",
  task_run = "notepad",
  schedule = "once",
  start_time = "asap"
)
#> SUCCESS: The scheduled task "open notepad" has successfully been created.
schtasks_showsid("open notepad")
#> SUCCESS: The SID "S-1-5-87-907618518-2201690017-3345919478-1888295809-4191631242" for the user name "open notepad" has been computed successfully.
schtasks_delete("open notepad", force = TRUE)
#> SUCCESS: The scheduled task "open notepad" was successfully deleted.
```

## See also

[taskscheduleR](https://github.com/bnosac/taskscheduleR), available on
[CRAN](https://CRAN.R-project.org)
