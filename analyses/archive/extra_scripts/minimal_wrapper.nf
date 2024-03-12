#!/usr/bin/env nextflow
// author: Alex Buerkle <buerkle@uwyo.edu>
nextflow.enable.dsl=2

// invoke as, to run locally:   nextflow run sparse_wrapper.nf 
// or as, to use SLURM: nextflow run sparse_wrapper.nf -c teton.config
// output will be in work/*/*/.commands.out and neighboring files

simsRds = Channel.fromPath( "/project/modelscape/analyses/sparse/data/simulations/sim_round3/*.rds")

process iterateFiles{
       input:
         file x
       output:
         stdout

       """
       echo 'made it here $x $HOSTNAME'
       """
}

workflow{ 
    // workflow uses channels as input by default
    iterateFiles(simsRds) 
}
