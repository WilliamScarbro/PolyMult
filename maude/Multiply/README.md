
Reduction framework

Base Langauge:
  EX-LANG.maude
  LABEL.maude
  
Operator Reduction Search:
  REDUCTION.maude
  FFT-REDUCTIONS.maude

Compilation:
  EX-DOMAIN.maude

FFT Implementation:
  FFT.maude

Utility: 
  UNIQ.maude
  FACTOR.maude

Trash (work in progress):
  LOOP.maude
  BFS.maude
  new-FFT.maude
  
./tests
  unit tests for maude files

./bin
  ./maude-pipe-cycle.sh
    steps through maude output one rule at a time
    expects input file to begin with "rew [1]" in system module
  ./maude-pipe.sh
    pretty prints maude output
    prepares output to be used in new maude call
  ./mpc 
    points to maude-pipe-cycle.sh
