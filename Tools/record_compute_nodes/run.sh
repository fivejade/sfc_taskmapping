#!/bin/bash
#PBS -N test
#PBS -V
#PBS -q normal
#PBS -A inhouse
#PBS -l select=8:ncpus=64:mpiprocs=64:ompthreads=1
#PBS -l walltime=0:10:00

cd $PBS_O_WORKDIR

. ./setup_env2

TOTAL_CPUS=$(wc -l $PBS_NODEFILE | awk ’{print $1}’)

OUTFILE=out.host.${TOTAL_CPUS}.${PBS_JOBID}
ERRFILE=err.host.${TOTAL_CPUS}.${PBS_JOBID}
# "record_compute_nodes 2" for 2D domain decomposition
# "record_compute_nodes 3" for 3D domain decomposition
mpirun ./record_compute_nodes 2 >> ${OUTFILE} 2>> ${ERRFILE}

