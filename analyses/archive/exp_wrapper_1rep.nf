#!/usr/bin/env nextflow

// author: Josh Jahner <jpjahner@gmail.com> 23 iii 23
nextflow.enable.dsl=2

// to run locally:   nextflow run sparse_wrapper.nf 
// or using SLURM: nextflow run sparse_wrapper.nf -c beartooth.config
// and put in the background so it should continue running: nextflow run -bg sparse_wrapper.nf -c beartooth.config
// simsRds = Channel.fromPath( "/project/modelscape/analyses/sparse/data/simulations/sim_round3/scenario[1-3]_rep1.rds")  // edit the string to get different subsets

     simsRdsTiny = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario1_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario4_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario7_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario1_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario4_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario7_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/behav_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/popbio_rep1.rds',
                                     '/project/modelscape/analyses/sparse/data/simulations/experiments/hydrology_rep1.rds']) // 10 min
     simsRdsSmall = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario2_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario5_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario8_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario2_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario5_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario8_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario3_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario6_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/correlation_scenario9_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario3_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario6_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/experiments/ncausal_scenario9_rep1.rds']) // 60 min
     simsRdsLarge = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/experiments/genomics_rep1.rds'])  // 36 hrs

//tiny   1, 4, 7, behav, popbio
//small  2, 3, 5, 6, 8, 9, hydrology
//large  genomics

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
       """
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



workflow{ 
// workflow has as main script as scope, so can access simsRds channel
// workflows use channels as input by default
iterateJobsTiny(simsRdsTiny)
iterateJobsSmall(simsRdsSmall)
//iterateJobsMedium(simsRdsMedium)
iterateJobsLarge(simsRdsLarge)
}

