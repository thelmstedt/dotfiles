#!/bin/bash

##
## this is deranged - i want my firefox to be default, but for reasons i want keeperpasswordmanager to use chromium
## but it doesn't respect `BROWSER=` and just uses xdg-open.
##

TEMP_DIR=$(mktemp -d)

# intercept exec calls and replace firefox with chromium
cat > "$TEMP_DIR/intercept.c" << 'EOF'
#define _GNU_SOURCE
#include <dlfcn.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

typedef int (*orig_execvp_f_type)(const char *file, char *const argv[]);

int execvp(const char *file, char *const argv[]) {
    orig_execvp_f_type orig_execvp;
    char *chromium_path = "/usr/bin/chromium";

    orig_execvp = (orig_execvp_f_type)dlsym(RTLD_NEXT, "execvp");

    if (strstr(file, "firefox")) {
        return orig_execvp(chromium_path, argv);
    }

    return orig_execvp(file, argv);
}
EOF

gcc -Wall -fPIC -shared -o "$TEMP_DIR/intercept.so" "$TEMP_DIR/intercept.c" -ldl

LD_PRELOAD="$TEMP_DIR/intercept.so" keeperpasswordmanager

rm -rf "$TEMP_DIR"