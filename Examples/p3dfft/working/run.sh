#!/bin/bash
#PBS -N p3dfft_256
#PBS -V
#PBS -q normal
#PBS -A inhouse
#PBS -l select=4:ncpus=64:mpiprocs=64:ompthreads=1
#PBS -l walltime=0:10:00
#PBS -m abe

cd $PBS_O_WORKDIR

. ./setup_env2

export LD_LIBRARY_PATH=/scratch/paop01/external/fftw-3.3.10//lib:$LD_LIBRARY_PATH

TOTAL_CPUS=$(wc -l $PBS_NODEFILE | awk '{print $1}')

OUTFILE=out.${TOTAL_CPUS}.${PBS_JOBID}
ERRFILE=err.${TOTAL_CPUS}.${PBS_JOBID}
OUTFILE2=out.hilbert.${TOTAL_CPUS}.${PBS_JOBID}
ERRFILE2=err.hilbert.${TOTAL_CPUS}.${PBS_JOBID}
date
mpirun ./test3D_r2c_cpp.hilbert  >> ${OUTFILE2} 2>> ${ERRFILE2}
date
mpirun ./test3D_r2c_cpp  >> ${OUTFILE} 2>> ${ERRFILE}
date
