#!/usr/bin/env nextflow

// author: Alex Buerkle <buerkle@uwyo.edu>
nextflow.enable.dsl=2

// to run locally:   nextflow run sparse_wrapper.nf 
// or using SLURM: nextflow run sparse_wrapper.nf -c beartooth.config
// and put in the background so it should continue running: nextflow run -bg sparse_wrapper.nf -c beartooth.config
// simsRds = Channel.fromPath( "/project/modelscape/analyses/sparse/data/simulations/sim_round3/scenario[1-3]_rep1.rds")  // edit the string to get different subsets

     simsRdsTiny = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario[1-9]_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario1[0-2]_rep1.rds']) // 10 min
     simsRdsSmall = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario1[3-5]_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario19_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario2[0-1]_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario2[8-9]_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario30_rep1.rds']) // 60 min
     simsRdsMedium = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario1[6-8]_rep1.rds',
                                       '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario2[2-4]_rep1.rds'])  // 24 hrs
     simsRdsLarge = Channel.fromPath(['/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario2[5-7]_rep1.rds',
                                      '/project/modelscape/analyses/sparse/data/simulations/sim_round5/scenario3[1-6]_rep1.rds'])  // 36 hrs

//tiny   scenarios 1-12
//small  scenarios 13-15, 19-21, 28-30
//medium scenarios 16-18, 22-24 
//large  scenarios 25-27, 31-36

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
iterateJobsMedium(simsRdsMedium)
iterateJobsLarge(simsRdsLarge)
}

