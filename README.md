# BiocPkgTools

<!-- badges: start -->
[![R-CMD-check](https://github.com/seandavi/BiocPkgTools/actions/workflows/R-CMD-check.yaml/badge.svg?branch=RELEASE_3_21)](https://github.com/seandavi/BiocPkgTools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Bioconductor has a rich ecosystem of metadata around packages, usage, and build status. 
This package is a simple collection of functions to access that metadata from R in a tidy data format. 
The goal is to expose metadata for data mining and value-added functionality such as package searching, text mining, and analytics on packages. 

Functionality includes access to ***computable versions of***:

- Download statistics
- Detailed package information
- Package dependendencies (and reverse dependencies)
- Package BiocViews categories
- Build reports
- Vignettes (including examples of text mining)

