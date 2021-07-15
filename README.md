# Discernment of Mediator and Outcome Measurement in the PACE trial

Ewan Carr  
Silia Vitoratou  
Trudie Chalder  
Kimberley Goldsmith<sup>*</sup>

<sup>*</sup> <kimberley.goldsmith@kcl.ac.uk>

## Abstract

> **Background.** When measuring latent traits, such as those used in
> psychology and psychiatry, it can be unclear whether the instruments used are
> measuring different concepts. This issue is particularly important in context
> of mediation analysis, since for a sound mediation hypothesis the mediator
> and outcome should be distinct. We sought to assess the extent of measurement
> overlap between mediators and outcomes in the PACE trial (n=640).
> 
> **Methods.** Potential measurement overlap was assessed using generalised
> linear latent variable models where confirmatory factor models quantified the
> extent to which the addition of cross-loading items resulted in significant
> improvements in model fit.
> 
> **Results.** Out of 26 mediator-outcome pairs considered, only six showed
> evidence of cross-loading items, supporting the suggestion that mediator and
> outcome constructs in the PACE trial were conceptually distinct.
> 
> **Conclusions.** This study highlights the importance of assessing
> measurement overlap in mediation analyses with latent traits to ensure
> mediator and outcome instruments are distinct.

:notebook: [`10.1101/2021.01.25.21250436`](https://doi.org/10.1101/2021.01.25.21250436)

## About

* This repository contains R code used in the above paper. 
* The scripts are presented to demonstrate how the analyses were conducted.
  However, they're quite specific to the dataset used in the paper and would
  require adapting to a new dataset.
* Please get in touch (open an issue or via the above email address) if you 
  have questions about the code or analyses and we'll do our best to help.

## Workflow

### `0-Prepare-data.R`

1. Import the raw SPSS dataset;
2. Select and rename the required columns;
3. Export the data for use in R (`pace.Rdata`) and Mplus using `prepareMplusData` from the `MplusAutomation` package.

### `1-Fit-all-models.R`

1. Import a list of model specifications (`model_spec.R`);
2. Generate and run the required Mplus input files for each analysis;   

    1. **Single factor models**
    
        This fits a single factor confirmatory factor analysis (CFA) for each
        mediator and outcome construct separately. Input files (and outputs,
        after running) are stored in the `separate_factors` folder.

    2. **Single factor model for each mediator-outcome pair** ('Model A')
    
        This fits a single CFA model using the items from each mediator-outcome
        pair. Input files are stored in `model_a`.

    3. **Examine cross-loadings from each mediator-outcome pair** ('Model B')

        This uses an iterative procedure to test (using `DIFFTEST` in Mplus)
        whether the addition of cross-loading items to the model improves
        overall model fit. Input files are stored in `model_b`.

3. Gather model outputs (using `readModels` from MplusAutomation) and save.

### `2-Examine-cross-loadings`

1. Import saved models;
2. Categorise cross-loadings for each mediator-outcome pair, as described in
   the manuscript.

***

The remaining files were used to process the fitted models and prepare outputs
(tables, figures, etc.) used in the manuscript. These scripts are highly
specific to our manuscript and probably not needed if replicating in another
dataset.

### `3-Process-outputs.R`

1. Import saved models;
2. Generate tables or statistics required in the manuscript.

### `4-Output-for-RMarkdown.R`

1. Import saved models;
2. Prepare numbers/summaries required in RMarkdown document.

###  `5-Longitudinal-invariance.R`

This file fits a series of models to test longitudinal measurement invariance
for mediators and outcomes measured at 0, 12, and 52 weeks. The input files
for these models were written by hand and can be found in the
`longitudinal_invariance` folder.

1. Fit each model in Mplus;
2. Import the outputs using `readModels`;
3. Produce table summarising fit statistics and `DIFFTEST` results.
