#!/bin/bash

# This script will load all files in the current working directory that 
# end in .cpl into an array and test each element of the array with run_test()

CPL="../../../cimple -a"

# $1 is the test file name
function run_test() 
{
    $(cat $1 | $CPL > "$1.out")
    $(diff "$1.out" "$1.out.expected" > /dev/null)
    RESULT=$?
    if [ $(($RESULT)) -ne $(($2)) ]; then
        echo "Error: test $1 did not match expected output"
        exit 1
    fi

    echo "Test $1 passed"
}

function print_test_and_expected_result()
{
    echo "$1 - $2"
}

TESTS=(*.cpl)

for ((i=0; i<${#TESTS[@]}; ++i)); do
    run_test ${TESTS[i]} 0
done

exit 0
