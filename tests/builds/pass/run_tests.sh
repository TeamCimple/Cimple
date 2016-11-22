#!/bin/bash

CPL="../../../cimple"

# $1 is the integer return status, $2 is a potential error message
function check_result
{
    if [ $1 -ne 0 ]; then
        echo $2
        exit $1
    fi
}

# $1 is the test file name
function run_test 
{
    EXECUTABLE=${1%.*}
    ERR=$(cat $1 | $CPL > $EXECUTABLE.c)
    RESULT=$?

    check_result $RESULT $ERR

    ERR=$(gcc -o $EXECUTABLE $EXECUTABLE.c > /dev/null 2>&1)
    RESULT=$?

    check_result $RESULT $ERR

    #run the program and make sure it doesn't crash 

    ERR=$(./$EXECUTABLE > $EXECUTABLE.out)
    RESULT=$?

    check_result $RESULT $ERR

    ERR=$(diff "$EXECUTABLE.out" "$EXECUTABLE.out.expected" )
    RESULT=$?

    check_result $RESULT $ERR
    
    echo "Test $1 passed"

    rm $EXECUTABLE $EXECUTABLE.out $EXECUTABLE.c 
}

TESTS=(*.cpl)

for ((i=0; i<${#TESTS[@]}; ++i)); do
    run_test ${TESTS[i]} 0
done

exit 0
