if [ "$1" == "gfortran" ]; then
  $1 affinity_schedule.f90 loops2.f90 -O3 -fopenmp -o main
  echo Done making gfortran build
elif [ "$1" == "ifort" ]; then
  $1 affinity_schedule.f90 loops2.f90 -O3 -qopenmp -o main
  echo Done making ifort build
fi
