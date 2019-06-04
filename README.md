# λ-time
A command-line time tracking tool written in Haskell

---

## Tracking time

```sh
$ timelog start

Clocked in at 2019-06-04 19:19:21.447657126 UTC

$ timelog stop "Updated README.md"

Clocked out at 2019-06-04 19:20:19.151680884 UTC
```

## Open the GUI

The `ui` command will open an interactive GUI (built with  [brick](https://github.com/jtdaugherty/brick)) that will let you select log entries and delete them. You can not modity an entry, however, because immutability is a key-feature of λ-time. Totaly not because we didn't have time to implement this.

```sh
$ timelog ui
```

# Configuration

The logged data is stored in a text file of the current directory. This allows project specific
logs. If you want to have global log, you can export the path to your logfile through the
`TIMELOG_DATA_FILE` environment variable.


# Development Environment

To make it easy to work on this project on any machine, without having to install the Haskell environment, we use a Docker image that has all required dependencies installed. You can start the container using the following command:

```sh
docker-compose run haskell /bin/bash
```
