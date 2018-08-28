#' BiocPkgTools: Examine and analyze Bioconductor package metadata
#'
#' Bioconductor has a rich ecosystem of metadata around
#' packages, usage, and build status. This package is a simple
#' collection of functions to access that metadata from R. The goal
#' is to expose metadata for data mining and value-added functionality
#' such as package searching, text mining, and analytics on
#' packages.
#'
#' @section For developers:
#' The \code{\link{build_status}} function returns a computable
#' form of the Bioconductor Build Report. 
#'
#' @section For users:
#' The \code{\link{getBiocDownloadStats}} function gets Bioconductor
#' download stats, allowing users to quickly find commonly used
#' packages. The \code{\link{getBiocPkgList}} is useful for getting
#' a complete listing of all Bioconductor packages. 
#'
#' @section Infrastructure:
#' Bioconductor packages all have Digital Object Identifiers (DOIs).
#' This package contains basic infrastructure for creating, updating,
#' and de-referencing DOIs.
#' 
#' @docType package
#' @name BiocPkgTools
NULL
