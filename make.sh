SRC=loops.f90.template
CLEAN=true
FC=ifort
FFLAGS="-O3 -qopenmp"

function compile {
  if [ "$2" == "" ]; then
    SUB="$1"
    FN="main_$1"
  else
    SUB="$1, $2"
    FN="main_$1_$2"
  fi

  cat $SRC | awk "{gsub(\"%%%\", \"$SUB\"); print \$0}" \
    > "$FN".f90

  $FC "$FN".f90 -o $FN $FFLAGS

  if [ $CLEAN ]; then
    rm "$FN".f90
  fi
}

compile static
compile auto

for i in 1 2 4 8 16 32 64; do
  compile static $i
  compile dynamic $i
  compile guided $i
done
