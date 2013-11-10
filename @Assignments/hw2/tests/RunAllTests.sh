#!/usr/bin/env bash


## Setting up colors for reporting
export bldyel=${txtbld}$(tput setaf 3) #  yellow

## using xargs to run all test files that end in _tests.sml
ls | grep --null -e .*_tests.sml$ | xargs -n 1 bash -c 'echo $bldyel; echo "Running all tests for file: $0"; echo $txtrst; ./TestRunner.sh $0'
