#!/bin/bash
set -euo pipefail

kubectl get nodes

echo
echo
nodes=$(kubectl get node --no-headers -o custom-columns=NAME:.metadata.name)

for node in $nodes; do
  echo "Node: $node"
  kubectl describe node "$node" | sed '1,/Non-terminated Pods/d'
  echo
  echo "-------------------------------------------------------------------------------------------"
  echo
done