#' Create FDA-style dataset and program definitions.
#'
#' define helps you create define.pdf and associated file tree 
#' for FDA-style submission of analysis datasets, etc.  
#' It converts csv files to SAS Transport (xpt) format,
#' using metadata encoded in a specification file. It enforces
#' the txt extension for other (presumably ASCII) files.
#'
#' The only function you're likely to need from \pkg{define} is
#' \code{\link{define}}. You may want to learn more about 
#' metadata encoding from \pkg{metrumrg}; see \code{\link{as.spec}}.
#'
#' @docType package
#' @name define-package
#' @import SASxport
#' @import methods
#' @import metrumrg
NULL