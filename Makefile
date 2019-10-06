# personal
FC=gfortran
FFLAGS=-O3 -fopenmp

# cirrus
#FC=ifort
#FFLAGS=-O3 -gopenmp

main: loops.f90
	$(FC) $< -o $@ $(FFLAGS)
