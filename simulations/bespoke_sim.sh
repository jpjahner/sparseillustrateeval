#!/bin/bash
#SBATCH --job-name=bespoke_sims
#SBATCH --nodes=1
#SBATCH --account=modelscape
#SBATCH --mem-per-cpu=500G
#SBATCH --time=0-10:00:00

module load swset/2018.05 gcc/7.3.0 py-scipy/1.1.0 miniconda3/4.10.3 r/4.0.5-py27

Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/simulations/bespoke_sim.R 
