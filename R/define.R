#' Generate a latex hyperlink.
#' 
#' Generates a latex hyperlink.
.hyperlink <- function(label, caption)command('hyperlink',args=c(label,.fragment(.escape(caption))))

#' Generate a latex hypertarget.
#' 
#' Generates a latex hypertarget.
.hypertarget <- function(label, caption)command('hypertarget',args=c(label,.fragment(.escape(caption))))

#' Generate a latex executeref.
#' 
#' Generates a latex executeref.
.executeref <- function(file)command('href',args=c(.execute(file),.fragment(.escape(file))))

#' Generate a latex staticref.
#' 
#' Generates a latex staticref (unlinked) to a file path.
.staticref <- function(file) .fragment(.escape(file))

#' Generate an executable path.
#' 
#' Generates an executable path.
.execute <- function(file)glue('run:./',file)

#' Break up a file path for wrapping in a table.
#' 
#' Breaks up a file path for wrapping in a table.
.fragment <- function(x)gsub('/','/\\\\hspace{0pt}',x)

#' Format special characters for latex rendering.
#' 
#' Formats special characters for latex rendering.
.escape <- function(x){
  x <- gsub('_','\\_',x,fixed=TRUE) # ok
  x <- gsub('#','\\#',x,fixed=TRUE) # problematic
  x <- gsub('$','\\$',x,fixed=TRUE) # ok
  x <- gsub('%','\\%',x,fixed=TRUE) # problematic
  x <- gsub('&','\\&',x,fixed=TRUE) # ok
  x <- gsub('*','\\*',x,fixed=TRUE) # problematic
  x <- gsub('^','\\^',x,fixed=TRUE) # ok
  x <- gsub('?','\\?',x,fixed=TRUE) # problematic
  x
}



#' Create latex content for define.doc menu.
#' 
#' Creates latex content for define.doc menu.
.menu <- function(
  tag,
  title,
  des,
  file,
  src,
  spec,
  grid = TRUE,
  rules = 1,
  colwidth = c('0.5in','1.5in','1.5in','2in'),
  tabularEnvironment = 'longtable',
  walls = 1,
  tabnum = FALSE,
  pretable = paste('\\hline \\multicolumn{4}{|l|}{\\textbf{Contents}} \\\\'),
  prepos = 1,
  headerBold = TRUE,
  ...
){
  menu <- data.frame(
    File=ifelse(
      spec,
      .hyperlink(des,tag),
      tag
    ),
    Description=ifelse(
      spec, 
      .hypertarget(tag,des),
      des
    ),
    Location=.executeref(file),
    Source = ifelse(
      is.na(src),
      '',
      .staticref(src)
    )
  )
  
  if(headerBold) names(menu) <- glue('\\textbf{',names(menu),'}')
  menu <- tabular.data.frame(
    menu,
    grid=grid,
    rules=rules,
    colwidth=colwidth,
    tabularEnvironment=tabularEnvironment,
    walls=walls,
    ...
  )
  menu <- append(menu,pretable,prepos)
  menu
}

#'  Reverse a string.
#'  
#'  Reverses the order of characters in a string.
.strReverse <- function(x)sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

#'  Isolate a filename extenstion.
#'  
#'  Returns the extension of a filename.
.extension <- function(x,...){
  x <- .strReverse(x)
  x <- sub('\\..*','',x)
  x <- .strReverse(x)
  x
}
.base <- function(x,...){
  x <- .strReverse(x)
  x <- sub('^[^.]*.','',x)
  x <- .strReverse(x)
  x    
}

#' Create a submission.
#' 
#' Returns a list of vetted artifacts (spec or char corresponding to x)
#' that represent a submission object for further processing.
#' @export
as.submission <- function(x,...)UseMethod('as.submission') 

#' Coerce a submission to class submission.
#' 
#' Coerces a submission to class submission.
#' @param x object of dispatch
#' @param ... passed along
#' @export
as.submission.submission <- function(x,...)x

#' Render a character vector as class submission.
#' 
#' Converts a character vector of file names to class submission.
#' 
#' @param x filenames: xpt, csv, spec, txt, other
#' @param tag short names for each element of x
#' @param description informative multi-word label for each element of x
#' @param dir parent directory for placement of submission artifacts
#' @param subdir optional subdirectories relative to dir for each submission artifact
#' @param ... passed along to handlers
#' @export
#' @return a list of artifacts each having attributes: x, tag, des, file, spec
#' @describeIn as.submission
#' 
as.submission.character <- function(
  x, # xpt, csv, spec, txt, other
  tag = names(x),
  description = basename(x),
  dir = '.',
  subdir = NULL,
  ...  
){
  exists <- file.exists(x)
  if(!all(exists))stop('cannot find e.g. ',x[!exists][[1]])
  if(is.null(tag))tag <- rep_len(letters,length.out=length(x))
  need <- is.na(tag) | tag == ''
  tag[need] <- rep_len(letters,length.out=sum(need))
  tag <- make.unique(tag)
  if(length(subdir) == 1) subdir <- rep_len(subdir,length.out=length(x))
  stopifnot(
    length(description) == length(x),
    length(tag) == length(x),
    length(dir) == 1,
    length(subdir) == length(x) | is.null(subdir) 
  )
  description <- as.character(description)
  
  # case csv:  if spec enforce/use spec else make/enforce/use spec; copy as xpt
  # case xpt:  make/enforce/use spec use labels; copy
  # case spec: enforce/use spec, copy csv as xpt
  # case txt:  copy as is
  # case other:copy as txt
  classify <- function(x)structure(x,class=union(.extension(x),class(x)))
  
  y <- lapply(x, classify)
  dups <- duplicated(.base(x))
  #if(any(dups))warning('possible duplicate specification, e.g.',x[dups][[1]])
  
  out <- lapply(y,.handleEach,ref=x,tag=tag,dir=dir,subdir=subdir,des=description,copy=!is.null(subdir),...)
  class(out) <- 'submission'
  out
}

#' Coerce to class xport.
#' 
#' Coerces to class xport.
as.xport <- function(x, ...)UseMethod('as.xport')

#' Coerce labeled to xport.
#' 
#' Coerces labeled to xport.
#' @param x a labeled data.frame
#' @param name a name for the data.frame
#' @param file where to write the data.frame
#' @param autogen.formats passed to write.xport
#' @param ... passed to write.xport
#' @describeIn as.xport
#' 
as.xport.labeled <- function(x,name,file,autogen.formats=FALSE,...){
  y <- list(x)
  names(y) <- name
  write.xport(list=y,file=file,autogen.formats=autogen.formats)
  invisible(x)
}

#' Coerce to class labeled.
#' 
#' Coerces to class labeled.

as.labeled <- function(x,...)UseMethod('as.labeled')

#' Coerce character to class labeled.
#' 
#' Coerces character to a labeled data.frame.
#' 
#' @param x length-one filename for csv-formatted data file
#' @param spec length-one file name for spec
#' @param ... passed to as.labeled.dat
#' @describeIn as.labeled
#' @seealso \code{\link{as.labeled.data.frame}}
#' 
as.labeled.character <- function(x,spec,...){
  dat <- read.csv(x,as.is=TRUE,na.strings=c('','.'))
  if(missing(spec)) spec <- metrumrg::read.spec(sub('\\.csv$','.spec',x))
  #spec$guide[encoded(spec)] <- recode(spec$guide[encoded(spec)])
  as.labeled(dat,spec=spec,...)
}

#' Coerce data.frame to labeled.
#' 
#' Coerces data.frame to labeled.
#' 
#' Numeric values less than 1e-70 are coerced to zero to solve SAS encoding issues.
#' Column names are forced unique and forced SAS-compliant with \code{]link{makesasnames}}.
#' Labels are added to the data.frame column names, and to the data.frame itself.
#' 
#' @method as.labeled data.frame
#' @param x data.frame
#' @param label a SAS-style label for x
#' @param spec a spec (specification) data.frame containing column labels
#' @param check should the data.frame be required to match its specification?
#' @param ... ignored
#' @seealso \code{\link{as.labeled.character}}
#' 
as.labeled.data.frame <- function(x, label, spec, check=TRUE, ...){
  spec <- as.spec(spec)
  if(check) stopifnot(x %matches% spec)
  x[] <- lapply(names(x),function(col){
    var <- x[[col]]
    label <- with(spec, label[column == col])
    Hmisc::label(var) <- label
    if(is.numeric(var))var[var < 1e-70] <- 0
    var
  })
  Hmisc::label(x) <- label
  names(x) <- makesasnames(names(x))
  class(x) <- union('labeled',class(x))
  x
}

#' Isolate arguments for an extension-classified define target.
#' 
#' Handles each define target by isolating the relevant argument subset 
#' and forwarding to generic handler.
#' 
#' @param x path to one define target, classified by extension
#' @param ref set of all define targets: key to isolating further arguments
#' @param tag short names for items
#' @param dir directory for storing artifacts
#' @param subdir subdirectories relative to dir for storing artifacts
#' @param des descriptions of items
#' @param copy should x be copied to dir(/subdir)?
#' @param ... passed along
#' 
.handleEach <- function(x,ref,tag,dir,subdir,des,copy,...){
  tag <- tag[match(x,ref)]
  subdir <- subdir[match(x,ref)]
  des <- des[match(x,ref)]
  .handle(x,tag=tag,dir=dir,subdir=subdir,des=des,copy=copy,...)
}

#' Handle a define target.
#' 
#' Handles a particular define target.
.handle <- function(x,...)UseMethod('.handle')

#' Handle a define target that is a path to a csv file.
#' 
#' Handles a define target that is a path to a csv file. If there is not
#' a corresponding *.spec file in the same directory, it will be created
#' and a message will be issued.  Probably spec should be edited 
#' and \code{link{define}} should be re-run, since column labels 
#' and item decodes (i.e. factor labels) cannot be guessed.
#' 
#' @param x path to a csv file
#' @param tag short name
#' @param dir directory for storing artifact
#' @param subdir subdirectory relative to dir for storing artifact
#' @param des description of item
#' @param copy should x be copied to dir(/subdir)?
#' @param ... passed along
#' @describeIn .handle
#' 
.handle.csv <- function(x,tag,dir,subdir,des,copy,...){
  file <- sub('\\.csv$','.spec',x)
  spec <- if(file.exists(file)) metrumrg::read.spec(file) else metrumrg::specification(read.csv(x,as.is=TRUE,na.strings=c('','.')))
  if(!file.exists(file)) {
    message('creating ',file,'; edit and re-run as necessary')
    metrumrg::write.spec(spec,file)
  }
  stopifnot(x %matches% spec)
  spec$guide[encoded(spec)] <- recode(spec$guide[encoded(spec)]) # remove placeholder decodes
  file <- as.character(NA)
  if(copy){ 
    file <- .copy(x=x,tag=tag,dir=dir,subdir=subdir,des=des,spec=spec,export=TRUE,...)
    path <- file.path(dir,file)
    if(!file.exists(path)) warning(path,' seems not to have been created') else {
      sasnames <- lookup.xport(file.path(dir,file))[[1]]$name
      if(length(sasnames) != length(spec$column)) stop('item count mismatch between specification and XPT file')
      spec$column <- sasnames
    }
    list(x=x,tag=tag,des=des,file=file,spec=spec)
  }
}

#' Copy target to directory.
#' 
#' Copies define target to define directory.  In case of csv files, output is SAS Transport (xpt)
#' rather than direct copy.
#' 
#' @param x path to a file
#' @param tag short name
#' @param dir directory for storing artifact
#' @param subdir subdirectory relative to dir for storing artifact
#' @param des description of item: SAS-style label for csv conversion to xpt
#' @param spec spec data.frame if x is csv: source of SAS-style column labels for csv conversion to xpt
#' @param overwrite should file be overwritten if already present?
#' @param export should x be (read and) converted to xpt?
#' @param ... passed to as.xport, as.labeled, and file.copy.

.copy <- function(x,tag,dir,subdir, des=NULL, spec=NULL, overwrite=TRUE, export = FALSE, ext=if(export) '.xpt' else '.txt', ...){
  file <- file.path(subdir,glue(tag,ext))
  path <- file.path(dir,file)
  dirname <- dirname(path)
  if(!file.exists(dirname))dir.create(dirname,recursive=TRUE) 
  method = if(export) 'exporting ' else 'copying '
  message(method,x,' to ',path)
  if(export)  as.xport(name = tag,file=path,x = as.labeled(x,label = des,spec = spec,check = FALSE,...),...)
  if(!export) metrumrg::safe.call(file.copy,from=x,to=path,overwrite=overwrite,...)
  file
}

#' Handle a define target that is a path to a spec file.
#' 
#' Handles a path to a spec file by redirecting to corresponding .csv.  Error if x does not exist.
#' 
#' @param x path to a spec file
#' @param tag short name
#' @param dir directory for storing artifact
#' @param subdir subdirectory relative to dir for storing artifact
#' @param des description of item
#' @param copy should x be copied to dir(/subdir)?
#' @param ... passed along
#' @describeIn .handle
#' 
.handle.spec <- function(x,tag,dir,subdir,des,copy,...){
  x  <- sub(x,'\\.spec$','.csv')
  stopifnot(file.exists(x))
  class(x) <- union('csv', class(x))
  .handle(x,tag=tag,dir=dir,subdir=subdir,des=des,copy=copy,...)
}

#' Handle a define target that is a path to an xpt file.
#' 
#' Handles a path to an xpt file.  File must exist and contain 
#' a single table.  Warnings are issued if des does not match
#' SAS table label or tag does not match SAS storage name.  SAS
#' column labels are harvested for pdf documentation.

#' @param x path to a csv file
#' @param tag short name
#' @param dir directory for storing artifact
#' @param subdir subdirectory relative to dir for storing artifact
#' @param des description of item
#' @param copy should x be copied to dir(/subdir)?
#' @param ... passed along
#' @describeIn .handle
#' 
.handle.xpt <- function(x,tag,dir,subdir,des,copy,...){
  file <- read.xport(file=x,as.list=TRUE) # file is list of data.frame
  if(length(file) != 1) stop('expecting a single data.frame from ',x)
  nm <- names(file)
  file <- file[[1]] # file is data.frame
  lab <- Hmisc::label(file)
  if(is.null(nm) | is.na(nm) | nm != tag)warning('supplied tag ',tag, ' does not match xport name ',nm)
  if(is.null(lab)| is.na(lab)|lab != des)warning('supplied description ',lab,' does not match xport label ', lab)
  spec <- metrumrg::specification(file)
  spec$label <- sapply(file,Hmisc::label) # file is data.frame, no longer needed
  file <- if(copy) .copy(x=x,tag=tag,dir=dir,subdir=subdir,ext='.xpt') else as.character(NA)
  list(x=x,tag=tag,des=des,file=file,spec=spec)                                 
}

#' Handle a define target that is a path to a text file.
#' 
#' Handles a path to a text file.

#' @param x path to a text file
#' @param tag short name
#' @param dir directory for storing artifact
#' @param subdir subdirectory relative to dir for storing artifact
#' @param des description of item
#' @param copy should x be copied to dir(/subdir)?
#' @param ... passed along
#' @describeIn .handle
#' 

.handle.txt <- function(x,tag,dir,subdir,des,copy,...){
  file <-if(copy) .copy(x=x,tag=tag,dir=dir,subdir=subdir,ext='.txt',...) else as.character(NA)
  list(x=x,tag=tag,des=des,file=file,spec=NA)
}

#' Handle a define target by default.
#' 
#' Handles a path to a file by calling the (default) handler for txt.
#' the txt extension will be enforced.

#' @param x path to a file
#' @param tag short name
#' @param dir directory for storing artifact
#' @param subdir subdirectory relative to dir for storing artifact
#' @param des description of item
#' @param copy should x be copied to dir(/subdir)?
#' @param ... passed along
#' @describeIn .handle

.handle.default <- function(x,tag,dir,subdir,des,copy,...){
  .handle.txt(x=x,tag=tag,dir=dir,subdir=subdir,des=des,copy=copy,...)
}

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
#'  tag is taken by default as the names of x, but may be supplied explicitly.
#'  The following should have the same length as x: tag, description, subdir.
#'  subdir may also be length one. Other arguments have length one.
#'  
#'  The function iterates across the elements of x to create a submission
#'  object, a side effect of which is to copy (conditionally, transformed) each 
#'  corresponding file to (subdir of) dir.  The submission object is then
#'  converted to a pdf, written directly to dir as <stem>.pdf.
#'  
#'  Arguments short, protocol, sponsor, program, author, and date are length-one
#'  character that define attributes of the pdf title page. Alternatively, 
#'  they may be specified by names that reflect position rather than semantics:
#'  lhead1, lhead2, rhead1, rhead2, lfoot, rfoot (respectively).
#'  
#  @seealso \url{http://www.fda.gov/downloads/Drugs/DevelopmentApprovalProcess/FormsSubmissionRequirements/ElectronicSubmissions/UCM163565.pdf}
#  @seealso \url{http://www.fda.gov/downloads/ForIndustry/DataStandards/StudyDataStandards/UCM312964.pdf}
#'  @seealso \url{http://tinyurl.com/fda-pdf-spec-4-0}
#'  @seealso \url{http://tinyurl.com/fda-study-data-spec-2-0}
#'  
#'  @param x paths to existing files to be documented; possibly a named character vector
#'  @param stem the base of the file name for the pdf to be created
#'  @param tag short object names for each element of x; appears in pdf menu, and as table name in XPT file
#'  @param description informative labels for each element of x
#'  @param title a title to appear in the pdf
#'  @param short short title a.k.a. lhead1 (upper left pdf header)
#'  @param protocol relevant protocols a.k.a. lhead2 (lower left pdf header)
#'  @param sponsor study sponsor a.k.a. rhead1 (upper right pdf header)
#'  @param program drug development program a.k.a. rhead2 (lower right pdf header)
#'  @param author document author a.k.a. lfoot left pdf footer, italicized)
#'  @param date date format string a.k.a. rfoot (right footer) today's date by default
#'  @param logo file path for logo to include on cover page
#'  @param logoscale size adjustment for logo
#'  @param dir path to directory in which to place pdf and copied (transformed) files
#'  @param subdir path to subdirectories to which to copy each (transformed) file represented by x; use NULL to suppress archiving
#'  @param clear should dir be deleted if it exists?
#'  @param ... passed to as.submission and as.pdf
#'  
#'  @return invisible result of as.pdf.  Used for side effects.
#'  @export
#'  @describeIn define
#'  @examples 
#'  
#' code <- "write.csv(x=Theoph,file='theoph.csv',row.names=FALSE,quote=FALSE)"
#' writeLines(code,'theoph.R')
#' eval(parse(text=code))
#' write.spec(specification(Theoph),'theoph.spec')
#' define(c(theoph='theoph.csv'),stem='minimal')
#' as.pdf('{\\huge \\em Pharma, Inc.}',wide=50, long = 8,stem='logo')
#' define(
#'   x=c(
#'     theodat='theoph.csv',
#'     theoprg='theoph.R'
#'    ),
#'   subdir=c(
#'     'm5/datasets/analysis/datasets',
#'     'm5/datasets/analysis/programs'
#'   ),
#'   description=c(
#'     'Theophylline PK Dataset',
#'     'Theophylline PK Script'
#'   ),
#'   title = 'Pharmacokinetics of Theophylline',
#'   short = 'Theophylline PK',
#'   protocol = 'Protocol tpk-001',
#'   sponsor = 'Pharma, Inc.',
#'   program = 'Theophylline',
#'   author = 'define package for R',
#'   logo = 'logo.pdf',
#'   logoscale = 2,
#'   clear=FALSE
#' )

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
  ...  
){
  if(clear)unlink(dir, recursive=TRUE,force=TRUE)
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
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
    x=x,
    tag=tag,
    description=description,
    dir = dir,
    subdir = subdir,
    ...
  )
  z <- as.document(   #. i.e. as.document.submission
    y, 
    title=title,
    short=short,
    protocol=protocol,
    sponsor=sponsor,
    program=program,
    author=author,
    date=date,
    logo=logo,
    logoscale=logoscale,
    ...
  )
  as.pdf(z,stem=stem,dir=dir,...)
}


#' Coerce a submission object to a document.
#' 
#' Coerces a submission object to a document.
#' 
#' Makes a pdf-ready character object representing a latex document.  
#' Essentially a wrapper for as.document.  Title, logo, headers,
#' footers, date are placed on the title page.  The second 
#' page has a menu (table) of defined objects that creates bi-directional links
#' to any defined data tables.  Links are also created to the storage locations
#' relative to the (resulting) define.pdf.  Following pages table the attributes
#' of data items in any datasets. 
#' 
#' 
#' @param x a list of artifacts each having attributes: x, tag, des, file, spec
#' @param title a title for the document
#' @param short short title
#' @param protocol relevant protocol
#' @param sponsor program sponsor
#' @param program drug development program
#' @param author document author
#' @param date today's date by default
#' @param lhead1 left header 1, e.g. short title
#' @param lhead2 left header 2, e.g. relevant protocol(s)
#' @param rhead1 right header 1, e.g. sponsor
#' @param rhead2 right header 2, e.g. development program
#' @param lfoot left footer (italicized), e.g. responsible party
#' @param rfoot right footer, today's date by default
#' @param logo file path for title page logo
#' @param logoscale size adjustment for logo
#' @param morePreamble passed to \code{\link{as.document.character}}
#' @param geoLeft passed to \code{\link{as.document.character}}
#' @param geoRight passed to \code{\link{as.document.character}}
#' @param geoTop passed to \code{\link{as.document.character}}
#' @param geoBottom passed to \code{\link{as.document.character}}
#' @param pagestyle passed to \code{\link{as.document.character}}
#' @param thispagestyle passed to \code{\link{as.document.character}}
#' @param ... passed to as.define, tabular, as.document.character
#' @export
#' 
#' 
as.document.submission <- function (
  x, #list(x,tag,des,file,spec)
  title,
  short=title,
  protocol='~',
  sponsor='~',
  program='~',
  author='~',
  date= '\\mydate \\today',
  lhead1=short,
  lhead2=protocol,
  rhead1=sponsor,
  rhead2=program,
  lfoot=author,
  rfoot=date,
  logo=NULL,
  logoscale=1,
  morePreamble = c(
    metrumrg::command("usepackage", args = "longtable"), 
    metrumrg::command("usepackage", args = "hyperref", options='breaklinks'), 
    metrumrg::command("usepackage", args = "graphicx"),
    metrumrg::command("usepackage", args = "tocbibind"),
    metrumrg::command("usepackage", args = "fixltx2e"),
    metrumrg::command("usepackage", args = "tabu"),
    metrumrg::command("usepackage", args = "datetime",options='nodayofweek'),
    '\\newdateformat{mydate}{\\twodigit{\\THEDAY}{-}\\shortmonthname[\\THEMONTH]-\\THEYEAR}',
    metrumrg::command("NeedsTeXFormat", args="LaTeX2e"),
    "\\hypersetup{colorlinks=true,filecolor=blue,linkcolor=blue}",
    metrumrg::command("usepackage", args = c("fancyhdr,lastpage")),
    metrumrg::command('pagestyle',args='fancy'),
    metrumrg::command("lhead", args = paste(lhead1,'\\\\',lhead2,collapse=' ')),
    metrumrg::command("rhead", args = paste(rhead2,'\\\\',rhead1,collapse=' ')),
    metrumrg::command("lfoot", args = metrumrg::command("textit",args= lfoot)),
    metrumrg::command("rfoot", args = '\\mydate \\today'),
    metrumrg::command("date",  args = '\\mydate \\today'),
    metrumrg::command("title", args=title)    
  ),
  geoLeft = "1in", 
  geoRight = "1in", 
  geoTop = "1in", 
  geoBottom = "1in", 
  pagestyle = NULL,
  thispagestyle = NULL,
  ...
) {
  menu <- .menu(
    title = title,
    tag = sapply(x,`[[`,'tag'),
    des =  sapply(x,`[[`,'des'),
    file =  sapply(x,`[[`,'file'),
    src  =  sapply(x,`[[`,'x'),
    spec =  sapply(x,function(i)!identical(NA,i[['spec']]))
  )
  
  specified <- sapply(x,function(i)!identical(NA,i[['spec']]))
  specified <- x[specified]
  
  specify <- function(x,sep=' : ',collapse='\n\n', ...){
    caption <- x[['tag']]
    spec <- x[['spec']]
    des <- x[['des']]
    def <- metrumrg::as.define(spec,sep=sep,collapse=collapse,...)
    #tab <- tabular(def,caption = caption,...)
    tab <- tabular(def,...)
    tab <- wrap(tab,'center')
    link <- .hyperlink(caption,des)
    target <- .hypertarget(des,parens(caption))
    #link <- glue('\\hypertarget{',caption,'}{',des,'}')
    tab <- c('\\pagebreak\n',link,target,tab)
    tab
  }
  
  specifics <- lapply(specified,specify,...)
  specifics <- unlist(specifics)
  body <- c(
    menu,
    specifics
  )
  
  doc <- as.document(
    morePreamble = morePreamble, 
    prolog = c(
      if(is.null(logo)) NULL else wrap(environment='center',metrumrg::command('includegraphics',args=logo,options=glue('scale=',logoscale))),
      wrap(environment='center',metrumrg::command('huge',args=metrumrg::command('textbf',args=title))),
      metrumrg::command('thispagestyle',args='fancy')
    ),
    geoLeft = geoLeft, 
    geoRight = geoRight, 
    geoTop = geoTop, 
    geoBottom = geoBottom, 
    pagestyle = pagestyle, 
    thispagestyle = thispagestyle,
    body, 
    ...
  )
  doc
}

#' Make names for a dataset that are unique and follow SAS naming conventions.
#' 
#' Makes names for a dataset that are unique and follow SAS naming conventions.
#' Modeled after SASxport::makeSasNames, but handling special cases.
#' 
#' @param names existing column names
#' @param nchar limit on number of characters
#' @param maxPasses limit on number of reconciliation attempts
#' @param quiet should messages be suppressed?
#' 
#' @return character
makesasnames <-function (names, nchar = 8, maxPasses = 10, quiet = FALSE) {
  names <- as.character(names)
  names <- toupper(names)
  #tooLong <- nchar(names) > 8
  tooLong <- nchar(names) > nchar
  shortNames <- names
  if (any(tooLong)) {
    shortNames[tooLong] <- substr(names[tooLong], 1, nchar)
    if (!quiet) 
      warning("Truncated ", sum(tooLong), " long names to ", nchar," characters.")
  }
  varNames <- shortNames
  passes <- 0
  dups <- FALSE
  while (any(duplicated(varNames)) && passes < maxPasses) {
    passes <- passes + 1
    dups <- duplicated(varNames)
    repeatCount <- table(varNames) - 1
    digitChars <- nchar(as.character(repeatCount)) + 1
    names(digitChars) <- names(repeatCount)
    newNames <- make.names(substr(varNames, 1, nchar - digitChars[varNames]), unique = TRUE)
    changed <- newNames != names
    varNames[dups] <- newNames[dups]
  }
  if (any(duplicated(varNames))) 
    stop("Unable to make all names unique after ", passes, 
         " passes.")
  if (any(dups) && !quiet) 
    warning("Made ", sum(dups), " duplicate names unique.")
  varNames
}


#' Recode an encoded object.
#' 
#' Recodes an encoded object.
#' 
#' @export
#' @seealso \code{\link{encode}}
recode <- function(x,...)UseMethod('recode')

#' Recode a single string.
#' 
#' Recodes a single string.
#' @param x string to be recoded
#' @param sep separator to use for output
#' @return character
#' @seealso \code{\link{recode.character}}
#' 
.recode <- function(x,sep=substr(x,1,1),...){ # for a single character string
  stopifnot(length(x)==1)
  codes <- codes(x)
  decodes <- decodes(x)
  decodes[!is.na(decodes) & decodes == codes] <- NA
  encode(codes, decodes, sep=sep,...)
}
#' Recode an encoded character vector, dropping noninformative labels.
#' 
#' Recodes an encoded character vector.
#' 
#' \code{\link{specification}} creates a template spec object
#' corresponding to a data frame.  Not able to guess factor
#' level decodes (labels) or column descriptions (e.g. SAS labels)
#' it supplies defaults by repeating the argument values. These have
#' value for development, but not for reporting.  This function
#' drops non-informative decodes.
#' 
#' @param x vector of strings that represent encodings
#' @param ... passed to sapply
#' @return character
#' @export
#' @describeIn recode
recode.character <- function(x,...)sapply(x,.recode,USE.NAMES=FALSE,...)















