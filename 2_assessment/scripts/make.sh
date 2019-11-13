FILES_NAIVE="src/naive_affinity_schedule.f90 src/loops2_naive.f90"
FILES_QUEUE="src/priority_queue.f90 src/affinity_schedule.f90 \
  src/loops2_queue.f90"

if [ ! -d bin ]; then
  mkdir bin/
fi

if [ "$1" == "gfortran" ]; then
  $1 $FILES_NAIVE -O3 -fopenmp -o bin/main_naive
  $1 $FILES_QUEUE -O3 -fopenmp -o bin/main_queue
  echo Done making gfortran build
elif [ "$1" == "ifort" ]; then
  $1 $FILES_NAIVE -O3 -qopenmp -o bin/main_naive
  $1 $FILES_QUEUE -O3 -qopenmp -o bin/main_queue
  echo Done making ifort build
fi

rm *.mod
