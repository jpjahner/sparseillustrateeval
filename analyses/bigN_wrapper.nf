#!/usr/bin/env nextflow

// author: Josh Jahner <jpjahner@gmail.com> 16 vii 23
nextflow.enable.dsl=2

// to run locally:   nextflow run bigN_wrapper.nf 
// or using SLURM: nextflow run bigN_wrapper.nf -c beartooth.config
// and put in the background so it should continue running: nextflow run -bg bigN_wrapper.nf -c beartooth.config
// simsRds = Channel.fromPath( "/project/modelscape/analyses/sparse/data/simulations/sim_round3/scenario[1-3]_rep1.rds")  // edit the string to get different subsets

     simsRdsSmall = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/bigN/bigN_scenario1_rep*.rds'])  // 60 min
     simsRdsLarge = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/bigN/bigN_scenario2_rep*.rds'])  // 36 hrs

process iterateJobsTiny{
       label 'tiny'
       input:
         path x
       output:
         stdout

       """
       echo 'Working on file $x'
       Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/sparsity.R $x
       """
}

process iterateJobsSmall{
       input:
         path x
       output:
         stdout
       label 'small'
       """
       echo 'Working on file $x'
       Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/sparsity.R $x
       """
}

process iterateJobsMedium{
       label 'medium'
       input:
         path x
       output:
         stdout
       """x

       echo 'Working on file $x'
       Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/sparsity.R $x
       """
}

process iterateJobsLarge{
       label 'large'
       input:
         path x
       output:
         stdout
       """
       echo 'Working on file $x'
       Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/sparsity.R $x
       """
}

process iterateJobsHuge{
       label 'huge'
       input:
         path x
       output:
         stdout

       """
       echo 'Working on file $x'
       Rscript --vanilla /project/modelscape/analyses/sparseillustrateeval_bitbucket/analyses/sparsity.R $x
       """
}



workflow{ 
// workflow has as main script as scope, so can access simsRds channel
// workflows use channels as input by default
//iterateJobsTiny(simsRdsTiny)
iterateJobsSmall(simsRdsSmall)
//iterateJobsMedium(simsRdsMedium)
iterateJobsLarge(simsRdsLarge)
//iterateJobsHuge(simsRdsHuge)
}

