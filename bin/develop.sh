#!/bin/bash
JAVA_HOME_="$(dirname "$(dirname "$(realpath "$(which java)")")")"

JAVA_HOME="${JAVA_HOME:-"$JAVA_HOME_"}" \
    ghcid -c 'stack ghci --test' -T 'Spec.main' -s ':set args --failure-report=.failures.txt --rerun --fail-fast --rerun-all-on-success' 
