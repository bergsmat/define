
#' Define objects per FDA guidance.
#' 
#'  Defines (documents) a set of files in a manner intended to comply with 
#'  FDA guidance on submission of study data and related documentation.
#'  In particular, files in csv format are converted to SAS Transport (xpt),
#'  extensions for other files (presumably ASCII) are coerced to txt, 
#'  files are copied to a directory tree, and 
#'  define.pdf is created at the top level to describe the files in more detail.
#' 
#' @export

define <- function(x,...)UseMethod('define')


#'  Define a set of files per FDA guidance.
#'  
#'  \code{tag} is taken by default as the names of \code{x}, but may be supplied explicitly.
#'  The following should have the same length as \code{x}: \code{tag}, \code{description}, \code{subdir}.
#'  \code{subdir} may also be length one. Other arguments have length one.
#'  
#'  The function iterates across the elements of \code{x} to create a 'submission'
#'  object, a side effect of which is to copy (conditionally, transformed) each 
#'  corresponding file to (\code{subdir} of) \code{dir}.  The 'submission' object is then
#'  converted to a pdf, written directly to \code{dir} as '<stem>.pdf'.
#'  
#'  Arguments \code{short}, \code{protocol}, \code{sponsor}, \code{program}, \code{author}, and \code{date} are length-one
#'  character that define attributes of the pdf title page. Alternatively, 
#'  they may be specified by names that reflect position rather than semantics:
#'  \code{lhead1}, \code{lhead2}, \code{rhead1}, \code{rhead2}, \code{lfoot}, \code{rfoot} (respectively).
#'  
#  @seealso \url{http://www.fda.gov/downloads/Drugs/DevelopmentApprovalProcess/FormsSubmissionRequirements/ElectronicSubmissions/UCM163565.pdf}
#  @seealso \url{http://www.fda.gov/downloads/ForIndustry/DataStandards/StudyDataStandards/UCM312964.pdf}
#' @seealso \url{http://tinyurl.com/fda-pdf-spec-4-0}
#' @seealso \url{http://tinyurl.com/fda-study-data-spec-2-0}
#'  
#' @param x paths to existing files to be documented; possibly a named character vector
#' @param stem the base of the file name for the pdf to be created
#' @param tag short object names for each element of x; appears in pdf menu, and as table name in XPT file
#' @param description informative labels for each element of x
#' @param title a title to appear in the pdf
#' @param short short title a.k.a. lhead1 (upper left pdf header)
#' @param protocol relevant protocols a.k.a. lhead2 (lower left pdf header)
#' @param sponsor study sponsor a.k.a. rhead1 (upper right pdf header)
#' @param program drug development program a.k.a. rhead2 (lower right pdf header)
#' @param author document author a.k.a. lfoot left pdf footer, italicized)
#' @param date date format string a.k.a. rfoot (right footer) today's date by default
#' @param logo file path for logo to include on cover page
#' @param logoscale size adjustment for logo
#' @param dir path to directory in which to place pdf and copied (transformed) files
#' @param subdir path to subdirectories to which to copy each (transformed) file represented by x; use NULL to suppress archiving
#' @param clear should dir be deleted if it exists?
#' @param units should units for continuous varables be printed in Codes column?
#' @param ... passed to as.submission and as.pdf
#' @examples 
#' library(spec)                   # read and write data specifications
#' library(latexpdf)               # make dummy logo for pdf
#' library(encode)                 # encode factor levels for spec file
#' 
#' dir <- tempdir()                # a place to experiment
#' dir <- gsub('\\\\','/',dir)     # clean up windows path
#' outdir <- file.path(dir,'out')  # where to put the define archive

#' csv <- file.path(dir,'theoph.csv')    # path to data
#' script <- file.path(dir,'theoph.R')   # path to script making data
#' spec <- file.path(dir,'theoph.spec')  # path to data specification
#' 
#' # make dummy logo
#' \dontrun{
#' as.pdf('{\\huge \\em Pharma, Inc.}',wide = 50, long = 8,stem = 'logo', dir = dir)
#' }
#' # browseURL(system.file(package = 'define', 'logo.pdf')) # cached
#' logo <- system.file(package = 'define', 'logo.pdf')     # path to dummy logo
#' 
#' # make data more interesting
#' Theoph$renal <- 0
#' 
#' # create script
#' code <- "write.csv(x = Theoph,file = csv,row.names = FALSE,quote = FALSE)"
#' writeLines(code,script)
#' 
#' # 'run' the script
#' eval(parse(text = code))
#' 
#' # make data specification
#' s <- specification(Theoph)
#' renalcat <- c(
#'  'GFR >= 90 mL/min/1.73m^2',
#'  '60 <= GFR < 90 mL/min/1.73m^2',
#'  '45 <= GFR < 60 mL/min/1.73m^2',
#'  '30 <= GFR < 45 mL/min/1.73m^2',
#'  'GFR < 30 mL/min/1.73m^2'
#' )
#' codes <- encode(0:4, renalcat)
#' codes
#' s$guide[s$column == 'renal'] <- codes
#' 
#' write.spec(s,spec)
#' 
#' file.exists(csv)
#' file.exists(spec)
#' # define(c(theoph = csv),stem = 'minimal',dir = outdir, clean=FALSE)
#' # browseURL(file.path(outdir,'minimal.pdf'))
#' # browseURL(system.file(package = 'define', 'minimal.pdf')) # cached
#' 
#' \dontrun{
#' define(
#'   x = c(
#'     theodat = csv,
#'     theoprg = script
#'    ),
#'   subdir = c(
#'     'm5/datasets/analysis/datasets',
#'     'm5/datasets/analysis/programs'
#'   ),
#'   description = c(
#'     'Theophylline PK Dataset',
#'     'Theophylline PK Script'
#'   ),
#'   title = 'Pharmacokinetics of Theophylline',
#'   short = 'Theophylline PK',
#'   protocol = 'Protocol tpk-001',
#'   sponsor = 'Pharma, Inc.',
#'   program = 'Theophylline',
#'   author = 'define package for R',
#'   logo = logo,
#'   logoscale = 2,
#'   clear = FALSE,
#'   dir = outdir
#' )
#' # browseURL(file.path(outdir,'define.pdf'))
#' }
#' # browseURL(system.file(package = 'define','define.pdf')) # cached
#' # browseURL(system.file(package = 'define','poster.pdf')) # earlier work
#' 
#' # Alternatively, supply aesthetics by position:
#' \dontrun{
#' define(
#'   x = c(
#'     theodat = csv,
#'     theoprg = script
#'    ),
#'   subdir = c(
#'     'm5/datasets/analysis/datasets',
#'     'm5/datasets/analysis/programs'
#'   ),
#'   description = c(
#'     'Theophylline PK Dataset',
#'     'Theophylline PK Script'
#'   ),
#'   title = 'Pharmacokinetics of Theophylline',
#'   lhead1 = 'Theophylline PK',
#'   lhead2 = 'Protocol tpk-001',
#'   rhead1 = 'Pharma, Inc.',
#'   rhead2 = 'Theophylline',
#'   lfoot = 'define package for R',
#'   rfoot = '\\mydate \\today',
#'   logo = logo,
#'   logoscale = 2,
#'   clear = FALSE,
#'   dir = outdir
#' )
#' }
#' 
#' # Tags for elements of x can be given explicitly rather than as names:
#' \dontrun{
#' define(
#'   x   = c(csv, script),
#'   tag = c('theodat','theoprg'),
#'   subdir = c(
#'     'm5/datasets/analysis/datasets',
#'     'm5/datasets/analysis/programs'
#'   ),
#'   description = c(
#'     'Theophylline PK Dataset',
#'     'Theophylline PK Script'
#'   ),
#'   title = 'Pharmacokinetics of Theophylline',
#'   lhead1 = 'Theophylline PK',
#'   lhead2 = 'Protocol tpk-001',
#'   rhead1 = 'Pharma, Inc.',
#'   rhead2 = 'Theophylline',
#'   lfoot = 'define package for R',
#'   rfoot = '\\mydate \\today',
#'   logo = logo,
#'   logoscale = 2,
#'   clear = FALSE,
#'   dir = outdir
#' )
#' }
#' 
#' # If the title is short, no need to supply a short version.
#' # Most arguments have suitable defaults. But be sure to
#' # supply tags, or names for elements of x.
#' \dontrun{
#' define(
#'   x = c(
#'     theodat = csv,
#'     theoprg = script
#'    ),
#'   description = c(
#'     'Theophylline PK Dataset',
#'     'Theophylline PK Script'
#'   ),
#'   title = 'Theophylline PK',
#'   dir = outdir
#' )
#' }
#' 
#' 
#' @return invisible result of as.pdf.  Used for side effects.
#' @export
#' @describeIn define character method for define

define.character<- function(
  x,
  stem = 'define',
  tag = names(x),
  description = basename(x),
  title = dirname(x[[1]]),
  short = title,
  protocol = '~',
  sponsor = '~',
  program = '~',
  author = '~',
  date = '\\mydate \\today',
  logo = NULL,
  logoscale = 1,
  dir = './define',
  subdir = '.', 
  clear = TRUE,
  units = FALSE,
  ...  
){
  if(clear)unlink(dir, recursive = TRUE,force = TRUE)
  if(!file.exists(dir)) dir.create(dir,recursive = TRUE)
  stopifnot(file.exists(dir))
  stopifnot(
    length(description) == length(x),
    length(tag) == length(x),
    length(dir) == 1,
    is.null(subdir) | length(subdir) %in% c(1,length(x)),
    length(title) == 1,
    length(short) == 1,
    length(protocol) == 1,
    length(sponsor) == 1,
    length(program) == 1,
    length(author) == 1,
    length(date) == 1
  )
  description <- as.character(description)
  title <- as.character(title)
  short <- as.character(short)
  protocol <- as.character(protocol)
  sponsor <- as.character(sponsor)
  program <- as.character(program)
  author <- as.character(author)
  date <- as.character(date)
  
  y <- as.submission(
    x = x,
    tag = tag,
    description = description,
    dir = dir,
    subdir = subdir,
    ...
  )
  z <- as.document(   #. i.e. as.document.submission
    y, 
    title = title,
    short = short,
    protocol = protocol,
    sponsor = sponsor,
    program = program,
    author = author,
    date = date,
    logo = logo,
    logoscale = logoscale,
    units = units,
    ...
  )
  as.pdf(z,stem = stem,dir = dir,...)
}

