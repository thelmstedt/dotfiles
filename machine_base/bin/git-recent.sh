#!/bin/bash

RED=$(tput setaf 9)
GREEN=$(tput setaf 10)
#YELLOW=$(tput setaf 11)
BLUE=$(tput setaf 12)
MAG=$(tput setaf 13)
RESET=$(tput sgr0)


git for-each-ref \
    --sort=-committerdate \
    --format="%(committerdate:relative)|%(refname:short)|%(refname)|%(authorname)|%(subject)" \
    refs/remotes/ refs/heads/ | \
while IFS="|" read -r date branch fullref author message; do
    branch_trim=$(echo "$branch" | tr -d " ")

    if [[ $fullref == refs/remotes/* ]]; then
        BRANCH_COLOR=$BLUE
    else
        BRANCH_COLOR=$GREEN
    fi

    if [ "$branch_trim" != "$DEFAULT_BRANCH" ]; then
        counts=$(git rev-list --left-right --count $DEFAULT_BRANCH..."$branch_trim" 2>/dev/null || echo "0 0")
        behind=$(echo "$counts" | cut -f1)
        ahead=$(echo "$counts" | cut -f2)
        printf "${RED}%s${RESET} | ${BRANCH_COLOR}%s${RESET} | ${MAG}%s${RESET} | %s | ${RED}↓%s${RESET} ${GREEN}↑%s${RESET}\n" \
            "$date" "$branch" "$author" "$message" \
            "$behind" "$ahead"
    else
        printf "${RED}%s${RESET} | ${BRANCH_COLOR}%s${RESET} | ${MAG}%s${RESET} | %s | ${BLUE}(default)${RESET}\n" \
            "$date" "$branch" "$author" "$message"
    fi
done