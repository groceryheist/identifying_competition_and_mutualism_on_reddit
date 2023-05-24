---
title: Reproduction code for "Identifying Competition and Mutualism between Online Groups". 
---

This repository contains code used in an analysis of ecological relationships among subreddits published as: 

TeBlunthuis, N., & Hill, B. M. (2022). Identifying competition and mutualism between online groups. International AAAI Conference on Web and Social Media (ICWSM 2022), 16, 993–1004. <https://doi.org/10.1609/icwsm.v16i1.19352>

This repository only contains code. The data is available in the
[Harvard Dataverse](https://doi.org/10.7910/DVN/KLGHKY).

== Overview ==

The top-level files run the analyses reported in the paper, but depend
on several submodules. The entire project has been automated using
`GNU make`. It has been designed to run on the Hyak supercomputer at
the University of Washington. Reach out to Nathan TeBlunthuis
(nathante@umich.edu; @groceryheist on github) if you wish to attempt
to reproduce this analysis.

The `cdsc_reddit` submodule contains a pipeline for building datasets
from the Pushshift reddit corpus as well as measures of density,
similarity, and clustering. This pipeline has been used in several
projects, but the included version is a snapshot for reproducing this
analysis.

The `overleaf` submodule builds the manuscript using `knitr`. 

The `pyRembr` and `RemembR` submodules contains utilities for saving
data that can be moved to overleaf.
