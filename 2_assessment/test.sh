./main > test.out

cat test.out | grep "is\s*[0-9\-]*.[0-9]*" > check_test.out

d=$(diff check_test.out check_org.out)

if [ "$d" == "" ]; then
  echo SUCCESS
else
  echo FAILURE
fi
