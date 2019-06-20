#!/bin/bash
JAVA_HOME_="$(dirname "$(dirname "$(realpath "$(which java)")")")"
JAVA_HOME="${JAVA_HOME:-"$JAVA_HOME_"}" stack test --ta '--fail-fast --failure-report=.failures.txt --rerun'
