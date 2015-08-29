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
#' @author Tim Bergsma, \email{bergsmat@@gmail.com}
#' @importFrom Hmisc label
#' @importFrom Hmisc label.data.frame
#' @importFrom Hmisc label<-.data.frame
#' @importFrom metrumrg write.spec
#' @importFrom metrumrg safe.call
#' @importFrom metrumrg read.spec
#' @importFrom metrumrg specification
#' @importFrom metrumrg write.spec
#' @importFrom metrumrg %matches%
#' @importFrom metrumrg as.document
#' @importFrom metrumrg command
#' @importFrom metrumrg as.define
#' @importFrom metrumrg tabular
#' @importFrom metrumrg wrap
#' @importFrom metrumrg parens
#' @importFrom metrumrg as.spec
#' @importFrom SASxport write.xport
#' @importFrom metrumrg as.pdf
#' @importFrom metrumrg encoded
#' @importFrom metrumrg decodes
#' @importFrom metrumrg codes
#' @importFrom metrumrg encode
#' @importFrom metrumrg glue
#' @importFrom SASxport lookup.xport
#' @importFrom SASxport read.xport
#' @importFrom metrumrg tabular.data.frame
#' @importFrom metrumrg as.document
#' @importFrom metrumrg as.document.character



NULL