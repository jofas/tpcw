FC_GNU = gfortran
FFLAGS_GNU = -fopenmp

FC_INT = ifort
FFLAGS_INT = -qopenmp

# change variables in case you wish to compile with ifort
# instead of gfortran
FC = $(FC_GNU)
FFLAGS = -O3 $(FFLAGS_GNU)

EXE_NAIVE = bin/main_naive
EXE_QUEUE = bin/main_queue

SRC_NAIVE = src/naive_affinity_schedule.f90 src/loops2_naive.f90
SRC_QUEUE = src/priority_queue.f90 src/affinity_schedule.f90 \
						src/loops2_queue.f90

OBJ_NAIVE = $(SRC_NAIVE:src/%.f90=%.o)
OBJ_QUEUE = $(SRC_QUEUE:src/%.f90=%.o)

.PHONY: all
all: mk_bin $(EXE_NAIVE) $(EXE_QUEUE) clean

.PHONY: clean
clean:
	@rm *.mod *.o
	@echo Done with cleanup.

.PHONY: mk_bin
mk_bin:
	@if [ ! -d bin ]; then mkdir bin; fi


$(EXE_NAIVE): $(OBJ_NAIVE)
	@$(FC) $^ -o $@ $(FFLAGS)
	@echo Done making $@.

$(EXE_QUEUE): $(OBJ_QUEUE)
	@$(FC) $^ -o $@ $(FFLAGS)
	@echo Done making $@.


$(OBJ_NAIVE): $(SRC_NAIVE)
	@$(FC) -c $^ $(FFLAGS)

$(OBJ_QUEUE): $(SRC_QUEUE)
	@$(FC) -c $^ $(FFLAGS)
