#!/bin/bash
#SBATCH --job-name=sparse_sims_3_feeder
#SBATCH --nodes=1
#SBATCH --account=modelscape
#SBATCH --mem-per-cpu=20G

filename="$1"
output="$2"

module load swset/2018.05 gcc/7.3.0 py-scipy/1.1.0 miniconda3/4.10.3 r/4.0.5-py27
conda activate gemma

Rscript --vanilla /gscratch/emcfarl2/software/sparseillustrateeval/feeder_script_third_sims.R "${filename}" "${output}" 
conda deactivate
