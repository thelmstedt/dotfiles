#!/bin/bash
file="${1:?usage: $0 <filepath> [base-branch]}"
base="${2:-HEAD}"

git fetch --all -q

for branch in $(git branch -r | grep -v HEAD); do
  echo "=== $branch ==="
  git diff "$base:$file" "$branch:$file" 2>/dev/null || echo "(file absent)"
done