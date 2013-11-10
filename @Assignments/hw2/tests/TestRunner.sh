#!/usr/bin/env bash


file="$1"

bldred=${txtbld}$(tput setaf 1) #  red
bldgre=${txtbld}$(tput setaf 2) #  greenC
bldyel=${txtbld}$(tput setaf 3) #  yellow
txtrst=$(tput sgr0)             #  Reset

output=$(sml < "$file")

if [[ "$output" == *"TESTS PASSED"* ]]; then
  echo $bldgre
  echo "ALL TESTS PASSED"
else
  if [[ "$output" == *"FAILED:"* ]]; then
    echo $bldred
    echo "$output" | sed -n '/FAILED:/,$p'
  else
    echo "$output"
  fi
fi
echo $txtrst
