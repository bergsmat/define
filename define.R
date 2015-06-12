library(metrumrg)
library(SASxport)
.hyperlink <- function(label, caption)glue('\\hyperlink{',label,'}{',caption,'}')
.hypertarget <- function(label, caption)glue('\\hypertarget{',label,'}{',caption,'}')

defineMenu <- function(
  dname,
  title,
  desc,
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
  fragment <- function(x)gsub('/','/\\\\hspace{0pt}',x)
  menu <- data.frame(
    File=ifelse(spec,.hyperlink(desc,dname),dname),
    Description=ifelse(spec, .hypertarget(dname,desc),desc),
    Location=paste('\\href{run:./',file,'}{',fragment(file),'}',sep=''),
    Source = ifelse(is.na(src),'',src)
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
.strReverse <- function(x)sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
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

as.submission <- function(x,...)UseMethod('as.submission') 
# returns a list of vetted artifacts (spec or char corresponding to x)
as.submission.submission <- function(x,...)x
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
#  title <- as.character(title)
#  short <- as.character(short)
#  sponsor <- as.character(sponsor)
  
  # case csv:  if spec enforce/use spec else make/enforce/use spec; copy as xpt
  # case xpt:  make/enforce/use spec use labels; copy
  # case spec: enforce/use spec, copy csv as xpt
  # case txt:  copy as is
  # case other:copy as txt
  classify <- function(x)structure(x,class=.extension(x) %u% class(x))

  y <- lapply(x, classify)
  dups <- duplicated(.base(x))
  if(any(dups))stop('probable duplicate specification, e.g.',x[dups][[1]])
  
  out <- lapply(y,.handleEach,ref=x,tag=tag,dir=dir,subdir=subdir,des=description,copy=!is.null(subdir),...)
  class(out) <- 'submission'
  out
}

as.xport <- function(x, ...)UseMethod('as.xport')
as.xport.labeled <- function(x,name,file,autogen.formats=FALSE,...){
  y <- list(x)
  names(y) <- name
  write.xport(list=y,file=file,autogen.formats=autogen.formats)
  invisible(x)
}
as.labeled <- function(x,...)UseMethod('as.labeled')
as.labeled.character <- function(x,spec,...){
  dat <- read.csv(x,as.is=TRUE,na.strings=c('','.'))
  if(missing(spec)) spec <- read.spec(sub('\\.csv$','.spec',x))
  as.labeled(dat,spec=spec,...)
}
as.labeled.data.frame <- function(x, label, spec, check=TRUE, ...){
  spec <- as.spec(spec)
  if(check) stopifnot(x %matches% spec)
  x[] <- lapply(names(x),function(col){
    var <- x[[col]]
    label <- with(spec, label[column == col])
    label(var) <- label
    if(is.numeric(var))var[var < 1e-70] <- 0
    var
  })
  label(x) <- label
  names(x) <- makesasnames(names(x))
  class(x) <- 'labeled' %u% class(x)
  x
}

.handleEach <- function(x,ref,tag,dir,subdir,des,copy,...){
  tag <- tag[match(x,ref)]
  subdir <- subdir[match(x,ref)]
  des <- des[match(x,ref)]
  .handle(x,tag=tag,dir=dir,subdir=subdir,des=des,copy=copy,...)
}

.handle <- function(x,...)UseMethod('.handle')
.handle.csv <- function(x,tag,dir,subdir,des,copy,...){
  file <- sub('\\.csv$','.spec',x)
  spec <- if(file.exists(file)) read.spec(file) else specification(read.csv(x,as.is=TRUE,na.strings=c('','.')))
  if(!file.exists(file)) {
    message('creating ',file,'; edit and re-run as necessary')
    write.spec(spec,file)
  }
  stopifnot(x %matches% spec)
  file <- if(copy) .copy(x=x,tag=tag,dir=dir,subdir=subdir,des=des,spec=spec,export=TRUE,...) else as.character(NA)
  list(x=x,tag=tag,des=des,file=file,spec=spec)
}

.copy <- function(x,tag,dir,subdir, des=NULL, spec=NULL, overwrite=TRUE, export = FALSE, ext=if(export) '.xpt' else '.txt', ...){
  file <- file.path(subdir,glue(tag,ext))
  path <- file.path(dir,file)
  dirname <- dirname(path)
  if(!file.exists(dirname))dir.create(dirname,recursive=TRUE) 
  method = if(export) 'exporting ' else 'copying '
  message(method,x,' to ',path)
  if(export)  as.xport(name = tag,file=path,x = as.labeled(x,label = des,spec = spec,check = FALSE,...),...)
  if(!export) safe.call(file.copy,from=x,to=path,overwrite=overwrite,...)
  file
}

.handle.spec <- function(x,tag,dir,subdir,des,copy,...){
  x  <- sub(y,'\\.spec$','.csv')
  stopifnot(file.exists(x))
  class(x) <- 'csv' %u% class(x)
  .handle(x,tag=tag,dir=dir,subdir=subdir,des=des,copy=copy,...)
}
.handle.xpt <- function(x,tag,dir,subdir,des,copy,...){
  file <- read.xport(file=x,as.list=TRUE) # file is list of data.frame
  if(length(file) != 1) stop('expecting a single data.frame from ',x)
  nm <- names(file)
  file <- file[[1]] # file is data.frame
  lab <- label(file)
  if(is.null(nm) | is.na(nm) | nm != tag)warning('supplied tag ',tag, ' does not match xport name ',nm)
  if(is.null(lab)| is.na(lab)|lab != des)warning('supplied description ',lab,' does not match xport label ', lab)
  spec <- specification(file)
  spec$label <- sapply(file,label) # file is data.frame, no longer needed
  file <- if(copy) .copy(x=x,tag=tag,dir=dir,subdir=subdir,ext='.xpt') else as.character(NA)
  list(x=x,tag=tag,des=des,file=file,spec=spec)                                 
}

.handle.txt <- function(x,tag,dir,subdir,des,copy,...){
  file <-if(copy) .copy(x=x,tag=tag,dir=dir,subdir=subdir,ext='.txt',...) else as.character(NA)
  list(x=x,tag=tag,des=des,file=file,spec=NA)
}
.handle.default <- function(x,tag,dir,subdir,des,copy,...){
  .handle.txt(x=x,tag=tag,dir=dir,subdir=subdir,des=des,copy=copy,...)
}

define <- function(x,...)UseMethod('define')
define.character<- function(
  x, # possibly named
  stem,
  tag = names(x),
  description = basename(x),
  title = dirname(x[[1]]),
  short = title,
  sponsor = '',
  program = '',
  protocol = '',
  consultant = '',
  logo = NULL,
  dir = '.',
  subdir = '.', # use NULL to suppress archiving, or supply length(x) paths
  ...  
){
  if(!file.exists(dir)) dir.create(dir,recursive=TRUE)
  stopifnot(file.exists(dir))
  stopifnot(
    length(description) == length(x),
    length(tag) == length(x),
    length(dir) == 1,
    is.null(subdir) | length(subdir) %in% c(1,length(x)),
    is.character(title),
    is.character(short),
    length(title) == 1,
    length(short) == 1,
    length(sponsor) == 1,
    length(consultant) == 1
  )
  description <- as.character(description)
  title <- as.character(title)
  short <- as.character(short)
  sponsor <- as.character(sponsor)
  consultant <- as.character(consultant)
  
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
    sponsor=sponsor,
    program=program,
    protocol=protocol,
    logo=logo,
    consultant=consultant,
    ...
  )
  as.pdf(z,stem=stem,dir=dir,...)
}

as.document.submission <- function (
  x, #list(x,tag,des,file,spec)
  title,
  short=title,
  sponsor='',
  program='',
  protocol='',
  logo=NULL,  
  consultant='',
  morePreamble = c(
    command("usepackage", args = "longtable"), 
    command("usepackage", args = "hyperref", options='breaklinks'), 
    command("usepackage", args = "graphicx"),
    command("usepackage", args = "tocbibind"),
    command("usepackage", args = "fixltx2e"),
    command("usepackage", args = "tabu"),
    command("usepackage", args = "datetime",options='nodayofweek'),
    '\\newdateformat{mydate}{\\twodigit{\\THEDAY}{-}\\shortmonthname[\\THEMONTH]-\\THEYEAR}',
    command("NeedsTeXFormat", args="LaTeX2e"),
    "\\hypersetup{colorlinks=true,filecolor=blue,linkcolor=blue}",
    command("usepackage", args = c("fancyhdr,lastpage")),
    command('pagestyle',args='fancy'),
    command("lhead", args = paste(short,'\\\\',protocol,collapse=' ')),
    command("rhead", args = paste(program,'\\\\',sponsor,collapse=' ')),
    command("lfoot", args = command("textit",args= consultant)),
    command("rfoot", args = '\\mydate \\today'),
    command("date",  args = '\\mydate \\today'),
    command("title", args=title)    
  ),
  geoLeft = "1in", 
  geoRight = "1in", 
  geoTop = "1in", 
  geoBottom = "1in", 
  pagestyle = NULL,
  thispagestyle = NULL,
  ...
) {
  menu <- defineMenu(
    title = title,
    dname = sapply(x,`[[`,'tag'),
    desc =  sapply(x,`[[`,'des'),
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
    def <- as.define(spec,sep=sep,collapse=collapse,...)
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
      wrap(environment='center',command('includegraphics',args=logo,options='scale=2')),
      wrap(environment='center',command('huge',args=command('textbf',args=title))),
      command('thispagestyle',args='fancy')
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

  
  
  