#!/bin/bash

RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
MAG=$(tput setaf 5)
CYAN=$(tput setaf 6)
RESET=$(tput sgr0)

# Determine if we're using main or master
if git show-ref --verify --quiet refs/remotes/origin/main; then
    DEFAULT_BRANCH="origin/main"
else
    DEFAULT_BRANCH="origin/master"
fi

git for-each-ref \
    --sort=-committerdate refs/remotes/ \
    --format="%(committerdate:relative)|%(refname:short)|%(authorname)|%(subject)" | \
while IFS="|" read -r date branch author message; do
    branch_trim=$(echo "$branch" | tr -d " ")
    if [ "$branch_trim" != "$DEFAULT_BRANCH" ]; then
        counts=$(git rev-list --left-right --count $DEFAULT_BRANCH..."$branch_trim" 2>/dev/null || echo "0 0")
        behind=$(echo "$counts" | cut -f1)
        ahead=$(echo "$counts" | cut -f2)
        printf "${RED}%s${RESET} | ${CYAN}%s${RESET} | ${MAG}%s${RESET} | %s | ${RED}↓%s${RESET} ${GREEN}↑%s${RESET}\n" \
            "$date" "$branch" "$author" "$message" \
            "$behind" "$ahead"
    else
        printf "${RED}%s${RESET} | ${CYAN}%s${RESET} | ${MAG}%s${RESET} | %s | ${CYAN}(default)${RESET}\n" \
            "$date" "$branch" "$author" "$message"
    fi
done