#!/bin/bash
#PBS -N test
#PBS -V
#PBS -q normal
#PBS -A inhouse
#PBS -l select=8:ncpus=64:mpiprocs=64:ompthreads=1
#PBS -l walltime=00:30:00
#PBS -m abe

cd $PBS_O_WORKDIR

. ./setup_env2

TOTAL_CPUS=$(wc -l $PBS_NODEFILE | awk '{print $1}')

OUTFILE=out.${TOTAL_CPUS}.${PBS_JOBID}
ERRFILE=err.${TOTAL_CPUS}.${PBS_JOBID}
OUTFILE2=out.hilbert.${TOTAL_CPUS}.${PBS_JOBID}
ERRFILE2=err.hilbert.${TOTAL_CPUS}.${PBS_JOBID}
date
time mpirun ./prun_hilbert.ex  >> ${OUTFILE2} 2>> ${ERRFILE2}
date
time mpirun ./prun.ex  >> ${OUTFILE} 2>> ${ERRFILE}
date
