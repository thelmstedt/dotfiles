#!/bin/bash

INDEX=$1

if [ -z $INDEX ]; then
  echo "Usage:"
  echo "tmv-tail-index.sh <INDEX>"
  exit 1;
fi

IFS=$'\n'

OUTPUT=$(kubectl get pods | grep tmv | sed -n ${INDEX}p)
IFS=""
#echo $OUTPUT
POD=$(echo ${OUTPUT} | cut -d ' ' -f 1)
echo $POD
kubectl exec --stdin --tty $POD -- tail -f /tmv/ceeqtm/logs/django/exceptions.log
