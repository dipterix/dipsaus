# Last submission - 2020-01-19

Version 0.0.4, rejected:

```
Checking this creates and leaves behind 'rave_data' in the user's home
dir,
apparently from

   dipsaus/R/logger.R:  log_dir <- getOption('dipsaus.logdir',
'~/rave_data/log')
```

Fix: that file was used internally for bug-report and has been removed. 

