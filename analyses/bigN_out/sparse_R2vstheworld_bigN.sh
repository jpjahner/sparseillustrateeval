#!/bin/bash
#SBATCH --job-name=sparse_moreplots
#SBATCH --nodes=1
#SBATCH --time=0-24:00:00
#SBATCH --account=modelscape
#SBATCH --mem-per-cpu=200G

module load arcc/1.0 gcc/12.2.0 r

Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/bigN_out/sparse_R2vstheworld_bigN.R 
