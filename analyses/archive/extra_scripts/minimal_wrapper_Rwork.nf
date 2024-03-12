#!/usr/bin/env nextflow
// author: Alex Buerkle <buerkle@uwyo.edu>
nextflow.enable.dsl=2

// to run locally:   nextflow run sparse_wrapper.nf 
// or using SLURM: nextflow run sparse_wrapper.nf -c teton.config

// output will be in work/*/*/.commands.out and neighboring files

simsRds = Channel.fromPath( "/project/modelscape/analyses/sparse/data/simulations/sim_round3/*rep1.rds")  // edit the string to get different subsets

process iterateFiles{
       input:
         path x
       output:
         stdout

       """
       echo 'Working on $x'
       Rscript --vanilla /project/modelscape/analyses/sparse/code/nextflow/Rtest.R $x out_$x
       """
}

workflow{ 
    // workflow uses simsRds channel as input
    iterateFiles(simsRds) 
}
