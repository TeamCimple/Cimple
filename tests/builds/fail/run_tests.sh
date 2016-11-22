
CPL="../../../cimple"

# $1 is the integer return status, $2 is the name of the test
function check_result
{
    if [ $1 -eq 0 ]; then
        echo "Error: test $2 should have failed, but passed"
        exit 1
    fi
}

# $1 is the test file name
function run_test 
{
    ERR=$(cat $1 | $CPL > $1.c)
    RESULT=$?

    # Try to compile the program. This should fail
    check_result $RESULT $ERR

    echo "Test $1 passed"
}

TESTS=(*.cpl)

for ((i=0; i<${#TESTS[@]}; ++i)); do
    run_test ${TESTS[i]} 0
done

exit 0
