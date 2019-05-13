# λ-time
A command-line time tracking tool written in Haskell

## Tracking time
```sh
$ lt

You clocked in at 8:04

$ lt

Your end of the day registered as 17:15

 Today looks like this:

┌────────────────┬────────────────────────┐
│ Today          │ 7 Hours and 30 Minutes │
├────────────────┼────────────────────────┤
│ Clock in       │ 08:04                  │
├────────────────┼────────────────────────┤
│ Clock out      │ 17:15                  │
├────────────────┼────────────────────────┤
│ Date           │ 2017-03-08             │
└────────────────┴────────────────────────┘
```

## Add a description / task:

# timelog
A command-line time log written in Haskell

```sh
$ lt note "Fixing some bugs, #13"

Added description for current tracking periode
```

## Generate a report

The report command will open an interactive GUI (built with  [brick](https://github.com/jtdaugherty/brick)) that will let you select log entries, modify and delete them.

```sh
$ lt report

┌────────────────┬────────────────────────┬─────────────────────────────────────────┐
│ 2019-04-08     │ 7 Hours and 30 Minutes │ WIP: Testing new implementation of...   │
├────────────────┼────────────────────────┼─────────────────────────────────────────┤
│ 2019-04-09     │ 6 Hours and 20 Minutes │ Fixed some bugs, issue # 13             │
├────────────────┼────────────────────────┼─────────────────────────────────────────┤
│ 2019-04-10     │ 4 Hours                │                                         │
├────────────────┼────────────────────────┼─────────────────────────────────────────┤
│ 2019-04-11     │ 8 Hours and 23 Minutes │                                         │
├────────────────┼────────────────────────┼─────────────────────────────────────────┤
│ 2019-04-12     │ 8 Hours                │ Working on project report               │
├────────────────┼────────────────────────┼─────────────────────────────────────────┤
│ 2019-04-13     │ 7 Hours and 30 Minutes │                                         │
└────────────────┴────────────────────────┴─────────────────────────────────────────┘

┌──── Commands ────────────┐
│ Delete                 d │
│ Show details           s │
│ Modify                 m │
└──────────────────────────┘
```

# Configuration
The logged data is stored in a text file of the current directory. This allows project specific
logs. If you want to have global log, you can export the path to your logfile through the
`TIMELOG_DATA_FILE` environment variable.


# Development Environment

To make it easy to work on this project on any machine, without having to install the Haskell environment, we use a Docker image that has all required dependencies installed. You can start the container using the following command:

```
docker-compose run haskell /bin/bash
```
