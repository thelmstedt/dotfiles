#!/usr/bin/env bash

apply_limits() {
    local pid=$1
    local cmd=$2
    echo "Fixing: $cmd (PID: $pid)"
    set -x
    sudo renice 1 $pid
    sudo taskpolicy -B -p $pid
    sudo taskpolicy -c utility -p $pid
    sudo taskpolicy -t 5 -l 5 -p $pid
    set +x
}

while true; do
  sleep 10
  ps aux | grep [w]dav | while read -r user pid cpu mem vsz rss tt stat start time cmd; do
      apply_limits "$pid" "$cmd"
  done

  ps aux | grep -i [d]efender | while read -r user pid cpu mem vsz rss tt stat start time cmd; do
      apply_limits "$pid" "$cmd"
  done
  ps aux | grep -i [s]yspolicyd | while read -r user pid cpu mem vsz rss tt stat start time cmd; do
      apply_limits "$pid" "$cmd"
  done
  ps aux | grep -i [x]protect | while read -r user pid cpu mem vsz rss tt stat start time cmd; do
      apply_limits "$pid" "$cmd"
  done
done
