# script for testing the output of a version of the program
# against the correct output determined by a different
# implementation (the serial version without OpenMP).
#
# $1: name of the executable (either main_naive or
#     main_queue)

./bin/$1 > out/test.out

cat out/test.out | grep "is\s*[0-9\-]*.[0-9]*" > out/check_test.out

d=$(diff out/check_test.out out/check_org.out)

if [ "$d" == "" ]; then
  echo SUCCESS
else
  echo FAILURE
fi
