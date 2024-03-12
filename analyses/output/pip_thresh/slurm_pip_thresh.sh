#!/bin/bash

## slurm_pip_thresh.sh by JPJ 26 xii 23
## PURPOSE: to run pip_thresh.R
## USAGE: sbatch slurm_pip_thresh.sh

#SBATCH --job-name=pips
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --account=modelscape
#SBATCH --time=1:00:00
#SBATCH --mem=8G
#SBATCH -o /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/output/pip_thresh/pip_thresh_stdout
#SBATCH -e /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/output/pip_thresh/pip_thresh_stderr


module load arcc/1.0 gcc/12.2.0 r/4.2.2 gsl/2.7.1 openblas/0.3.21

cd /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/output/pip_thresh/
Rscript pip_thresh.R /project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario24_rep1.rds

rm file*
rm -Rf output
