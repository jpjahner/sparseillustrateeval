[![DOI](https://zenodo.org/badge/770594978.svg)](https://zenodo.org/doi/10.5281/zenodo.10815221)

# README 

This repo stores code and output for a manuscript project related to understanding the limits to statistical learning in the life sciences

Interpretable and predictive models to harness the life science data revolution




## Simulations

* The scripts used to build the simulated data used in this study are found in [simulations](simulations/)

* The final 36 core scenarios (100 replicates each) were created with [sim_round5.R](simulations/sim_round5.R) by calling [sparse_sim.R](simulations/sparse_sim.R)

* The final 2 bigN scenarios (100 replicates each) were created with [sim_bigN.R](simulations/sim_bigN.R) by calling [sparse_sim.R](simulations/sparse_sim.R)

* See [sim_grid_round5.csv](simulations/sim_grid_round5.csv) and [sim_grid_bigN.csv](simulations/sim_grid_bigN.csv) for scenario specifications



## Analyses of simulations

* The scripts used to run analyses and summarize output are found in [analyses](analyses/)

* Required functions for statistical analyses and calculating performance metrics are found in [functions](functions/)

* Analysis of the 36 core scenarios implemented using [sparse_wrapper.nf](analyses/sparse_wrapper.nf)

```{bash}
module load arcc/1.0 nextflow/22.10.4
nextflow run -bg sparse_wrapper.nf -c beartooth.config
```

* Analysis of the 2 bigN scenarios implemented using [bigN_wrapper.nf](analyses/bigN_wrapper.nf)

```{bash}
module load arcc/1.0 nextflow/22.10.4
nextflow run -bg bigN_wrapper.nf -c beartooth.config
```

* Nextflow output for the 36 core scenarios was merged and downstream analyses were performed in [output](analyses/output/)

* Nextflow output for the 2 bigN scenarios was merged and downstream analyses were performed in [bigN_out](analyses/bigN_out/)


### Scripts for manuscript figures

* Figure 1: [example_fig.R](analyses/output/example_fig.R)

* Figure 2: [core_out.R](analyses/output/core_out.R)

* Figure 3: [is_oos_plot.R](analyses/output/is_oos_plot.R)

* Figure 4: [variable_selection_plot.R](analyses/output/variable_selection_plot.R)

* Figure 5: [bigN_out.R](analyses/bigN_out/bigN_out.R)

* Figure S1: [process_signal.R](analyses/output/process_signal.R)

* Figure S2: [f1_oos_plot.R](analyses/output/f1_oos_plot.R)

* Figure S3: [pip_thresh_plot.R](analyses/output/pip_thresh/pip_thresh_plot.R)
