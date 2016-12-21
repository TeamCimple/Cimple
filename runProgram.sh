#!/bin/bash

CPL="./cimple"

# $1 is the integer return status, $2 is a potential error message
function check_result
{
    if [ $1 -ne 0 ]; then
        echo $2
        exit $1
    fi
}

# $1 is the test file name
function run_program
{
    EXECUTABLE=${1%.*}
    ERR=$(cat $1 | $CPL > $EXECUTABLE.c)
    RESULT=$?

    check_result $RESULT $ERR

    ERR=$(gcc -o $EXECUTABLE $EXECUTABLE.c > /dev/null 2>&1)
    RESULT=$?

    check_result $RESULT $ERR

    #run the program and make sure it doesn't crash 

    ERR=$(./$EXECUTABLE)
    RESULT=$?

    echo $RESULT

    rm $EXECUTABLE $EXECUTABLE.c
}

run_program $1

exit 0
