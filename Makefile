# cirrus
#FC=ifort
#FFLAGS=-O3 -gopenmp

EXE=main

.PHONY:
personal: loops.f90
	gfortran $< -o $(EXE) -O3 -fopenmp

.PHONY:
cirrus: loops.f90
	ifort $< -o $(EXE) -O3 -gopenmp
