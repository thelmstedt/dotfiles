#!/bin/bash

# check service endpoint health the kubernetes way
# usage: ./check_service.sh <namespace> <service-name>

NAMESPACE=${1:-default}
SERVICE=${2}

if [ -z "$SERVICE" ]; then
  echo "usage: $0 <namespace> <service-name>"
  exit 1
fi

# get endpoints for the service
ENDPOINTS=$(kubectl get endpoints "$SERVICE" -n "$NAMESPACE" -o json 2>/dev/null)

if [ $? -ne 0 ]; then
  echo "service $SERVICE not found in namespace $NAMESPACE"
  exit 1
fi

# count ready vs not-ready addresses
READY=$(echo "$ENDPOINTS" | jq '[.subsets[]?.addresses // [] | length] | add // 0')
NOT_READY=$(echo "$ENDPOINTS" | jq '[.subsets[]?.notReadyAddresses // [] | length] | add // 0')
TOTAL=$((READY + NOT_READY))

echo "service: $NAMESPACE/$SERVICE"
echo "ready endpoints: $READY/$TOTAL"

if [ "$READY" -eq 0 ] && [ "$TOTAL" -gt 0 ]; then
  echo "STATUS: UNHEALTHY - no ready endpoints"
  exit 2
elif [ "$TOTAL" -eq 0 ]; then
  echo "STATUS: NO ENDPOINTS"
  exit 2
else
  echo "STATUS: OK"
  exit 0
fi