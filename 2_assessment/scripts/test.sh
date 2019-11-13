./bin/$1 > out/test.out

cat out/test.out | grep "is\s*[0-9\-]*.[0-9]*" > out/check_test.out

d=$(diff out/check_test.out out/check_org.out)

if [ "$d" == "" ]; then
  echo SUCCESS
else
  echo FAILURE
fi
