#' Coerce to Define
#' 
#' Coerces to class 'define'.  Generic, with method for 'spec'.
#' @export
#' @param x object
#' @param ... passed arguments
as.define <- function (x, ...) UseMethod("as.define")

#' Coerce to Define from Spec
#' 
#' Coerces to class 'define' from class 'spec'. Extra arguments ignored.
#' @describeIn as.define method for spec
#' @export
#' @param sep separates codes from respective decodes where given
#' @param collapse separates code/decode pairs where given
#' @param escape values to escape for proper latex formatting
#' @seealso \code{\link[spec]{specification}}
#' @seealso \code{\link[spec]{as.spec}}

as.define.spec <- function(x,sep = ' = ',collapse = '; ',escape = c('_','%','$'),...){
  x$required <- NULL
  names(x) <- c('Variable', 'Label', 'Type', 'Codes','Comments')
  x$Type <- map(x$Type, from = c('character','numeric','integer','datetime'),
                to = c('char','num','num','char'))
  codes <- codes(x$Codes)
  decodes <- decodes(x$Codes)
  blend <- function(codes,decodes){
    if(length(codes) == 1) if(is.na(codes))return(as.character(NA))
    stopifnot(length(codes) == length(decodes))
    list <- paste(codes,decodes,sep = sep)
    list[is.na(decodes)] <- codes[is.na(decodes)]
    string <- paste(list,collapse = collapse)
    string
  }
  x$Codes <- sapply(seq_along(codes),function(i)blend(codes[[i]],decodes[[i]]))
  polish <- function(x,escape){
    for(i in escape)x <- gsub(i,paste0('\\',i),x,fixed = TRUE)
    x
  }    
  x[] <- lapply(x, polish,escape = escape)
  class(x) <- c('define','data.frame')
  x
}

#' Coerce to PDF from Define
#' 
#' Coerces to PDF from class 'define'.
#' @export
#' @param x define object
#' @param stem passed to \code{\link[latexpdf]{as.pdf}}
#' @param ... passed to \code{\link[latexpdf]{as.pdf}} and \code{\link[latexpdf]{as.document}}
#' @import latexpdf
as.pdf.define <- function(x,stem,...)as.pdf(stem=stem,as.document(x,...),...)
  

#' Coerce to Tabular from Define
#' 
#' Coerces to class 'tabular' from class 'define'.
#' @export
#' @param x define object
#' @param caption caption for definitions table
#' @param grid passed to \code{\link[latexpdf]{as.tabular.data.frame}}
#' @param rules passed to \code{\link[latexpdf]{as.tabular.data.frame}}
#' @param colwidth passed to \code{\link[latexpdf]{as.tabular.data.frame}}
#' @param tabularEnvironment passed to \code{\link[latexpdf]{as.tabular.data.frame}}
#' @param walls passed to \code{\link[latexpdf]{as.tabular.data.frame}}
#' @param tabnum whether to number the table
#' @param pretable material to include before table, typically a caption
#' @param prepos after which line number should pretable be inserted?
#' @param headerBold whether to use a bold header
#' @param ... passed to \code{\link[latexpdf]{as.tabular}}
#' @import latexpdf

as.tabular.define <- function(
  x,
  caption = '',
  grid = TRUE,
  rules = 1,
  colwidth = c('1in','1in','0.5in','1.5in','1.5in'),
  tabularEnvironment = 'longtable',
  walls = 1,
  tabnum = FALSE,
  pretable = if(is.null(caption)) '' else paste(if(tabnum) '\\caption{' else '\\caption*{',caption,'}\\\\'),
  prepos = 1,
  headerBold = TRUE,
  ...
){
  if(headerBold) names(x) <- paste0('\\textbf{',names(x),'}')
  class(x) <- 'data.frame'
  tab <- as.tabular(
    x,
    grid=grid,
    rules=rules,
    colwidth=colwidth,
    tabularEnvironment=tabularEnvironment,
    walls=walls,
    ...
  )
  tab <- append(tab,pretable,prepos)
  tab
}

#' Coerce to Document from Define
#' 
#' Coerces to class 'document' from class 'define'.
#' @export
#' @param x passed to \code{\link{as.tabular.define}}
#' @param morePreamble passed to \code{\link[latexpdf]{as.document.character}}
#' @param geoLeft passed to \code{\link[latexpdf]{as.document.character}}
#' @param geoRight passed to \code{\link[latexpdf]{as.document.character}}
#' @param geoTop passed to \code{\link[latexpdf]{as.document.character}}
#' @param geoBottom passed to \code{\link[latexpdf]{as.document.character}}
#' @param pagestyle passed to \code{\link[latexpdf]{as.document.character}}
#' @param ... passed to \code{\link[latexpdf]{as.document.character}} and \code{\link{as.tabular.define}}
#' @import latexpdf
as.document.define <- function(
  x,
  morePreamble = command('usepackage',args = 'longtable'),
  geoLeft = '1in',
  geoRight = '1in',
  geoTop = '1in',
  geoBottom = '1in',
  pagestyle = command("pagestyle", args = "plain"),
  ...
){
  tab <- as.tabular(x, ...)
  tex <- as.document(
    morePreamble = morePreamble,
    geoLeft = geoLeft,
    geoRight = geoRight,
    geoTop = geoTop,
    geoBottom = geoBottom,
    pagestyle = pagestyle,
    tab,
    ...
  )
  tex
}

